;;;; cl-sf/src/objects.lisp
;;;; CLOS sObject layer — the public API surface of cl-sf.
;;;;
;;;; This file owns everything the user touches:
;;;;   - SOBJECT base class (all Salesforce records are instances of this or a subclass)
;;;;   - DEFINE-SOBJECT-CLASS macro (generates typed subclasses)
;;;;   - HASH->SOBJECT / SOBJECT->HASH-TABLE (conversion boundary)
;;;;   - SELECT (SOQL queries) and the wrappers SOBJECT-GET, DATABASE-INSERT, etc.
;;;;     All of these call cl-sf/api:% internals and convert at the boundary.
;;;;
;;;; The CLOS layer is the ONLY layer the user touches.
;;;; cl-sf/api (raw HTTP) is never called by user code directly.

#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-cltl2))

(in-package #:cl-sf/objects)

;; Defined in cl-sf/bulk, which loads after this file (it needs our classes).
(declaim (special cl-sf/bulk:*bulk-threshold*))

;;; ──────────────────────────────────────────────────────────────────
;;; Registries
;;; ──────────────────────────────────────────────────────────────────
;;; Declared ahead of the base class below because PRINT-OBJECT on
;;; SOBJECT reads *SOBJECT-SLOT-MAPS* directly (to show current slot
;;; values instead of the possibly-stale fetched snapshot).

(defvar *sobject-classes* (make-hash-table :test #'equal)
  "Maps Salesforce API name string → CLOS class object.
   e.g. \"Account\" → #<STANDARD-CLASS ACCOUNT>
   Populated by DEFINE-SOBJECT-CLASS and LOAD-SOBJECT-CLASSES.")

(defvar *sobject-field-kinds* (make-hash-table :test #'eq)
  "Maps class-name symbol → EQUAL hash-table of sf-field-name → \"date\" or
   \"datetime\", for the fields whose slots hold LOCAL-TIME timestamps.
   A class with no such fields has no entry.  Set by DEFINE-SOBJECT-CLASS.")

(defvar *sobject-slot-maps* (make-hash-table :test #'eq)
  "Maps class-name symbol → alist of (sf-field-name-string . slot-name-symbol).
   Used by HASH->SOBJECT to populate slots and by SOBJECT->HASH-TABLE to
   serialise them back.
   e.g. 'ACCOUNT → ((\"Name\" . name) (\"Phone\" . phone) ...)")

;;; ──────────────────────────────────────────────────────────────────
;;; Dates and datetimes: Salesforce text <-> LOCAL-TIME:TIMESTAMP
;;; ──────────────────────────────────────────────────────────────────

(defun %timestamp-text-slow (timestamp kind)
  "The reference implementation of %TIMESTAMP-TEXT, via LOCAL-TIME's formatter
   (about 40x slower; kept for years outside 0000-9999 and for the tests)."
  (local-time:format-timestring
   nil timestamp :timezone local-time:+utc-zone+
                 :format (cond ((equal kind "date")
                                '((:year 4) #\- (:month 2) #\- (:day 2)))
                               ((equal kind "time")
                                '((:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:msec 3) #\Z))
                               (t
                                '((:year 4) #\- (:month 2) #\- (:day 2) #\T
                                  (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:msec 3) #\Z)))))

(defun %timestamp-text (timestamp kind)
  "TIMESTAMP (a LOCAL-TIME:TIMESTAMP, taken as UTC) as Salesforce text for a
   field of describe type KIND: \"date\" → 2024-03-05, \"time\" → 03:04:05.000Z,
   anything else (datetime) → 2024-03-05T03:04:05.000Z.  Used for query
   literals and for values sent to Salesforce.  Writes the digits directly:
   LOCAL-TIME's formatter costs ~2 µs and ~600 bytes a value, which adds up
   over a large save."
  (declare (type local-time:timestamp timestamp) (optimize (speed 3) (safety 0)))
  (let* ((days (local-time:day-of timestamp))        ; since 2000-03-01
         (sod  (local-time:sec-of timestamp))        ; seconds into the UTC day
         (msec (floor (the (integer 0 999999999) (local-time:nsec-of timestamp)) 1000000)))
    (declare (type fixnum days sod msec))
    ;; days since 1970-01-01 → civil date (Howard Hinnant's algorithm)
    (let* ((z   (+ days 11017 719468))
           (era (floor z 146097))
           (doe (- z (* era 146097)))
           (yoe (floor (- doe (floor doe 1460) (- (floor doe 36524)) (floor doe 146096)) 365))
           (doy (- doe (+ (* 365 yoe) (floor yoe 4) (- (floor yoe 100)))))
           (mp  (floor (+ (* 5 doy) 2) 153))
           (day (1+ (- doy (floor (+ (* 153 mp) 2) 5))))
           (month (if (< mp 10) (+ mp 3) (- mp 9)))
           (year  (+ yoe (* era 400) (if (<= month 2) 1 0))))
      (declare (type fixnum z era doe yoe doy mp day month year))
      (if (not (<= 0 year 9999))
          (%timestamp-text-slow timestamp kind)
          (let* ((date-p (equal kind "date"))
                 (time-p (equal kind "time"))
                 (out (make-string (cond (date-p 10) (time-p 13) (t 24))))
                 (i 0))
            (declare (type fixnum i))
            (macrolet ((put (char) `(progn (setf (schar out i) ,char) (incf i)))
                       (digits (value width)
                         `(let ((v ,value))
                            (declare (type fixnum v))
                            (loop for k from (1- ,width) downto 0
                                  do (setf (schar out (+ i k)) (code-char (+ 48 (mod v 10)))
                                           v (floor v 10)))
                            (incf i ,width))))
              (unless time-p
                (digits year 4) (put #\-) (digits month 2) (put #\-) (digits day 2))
              (unless date-p
                (unless time-p (put #\T))
                (digits (floor sod 3600) 2) (put #\:)
                (digits (floor (mod sod 3600) 60) 2) (put #\:)
                (digits (mod sod 60) 2) (put #\.)
                (digits msec 3) (put #\Z)))
            out)))))

(defun %parse-temporal (string kind)
  "Parse the date or datetime Salesforce wrote in STRING into a
   LOCAL-TIME:TIMESTAMP, or return NIL if it isn't one.  Accepts 2024-03-05,
   2024-03-05T03:04:05Z, .123 fractions up to nanoseconds, and the offsets
   Z, +0000, +00:00 and +00 (REST writes +0000, GraphQL writes Z).  A date
   is midnight UTC.  KIND (\"date\" or \"datetime\") only says whether a time
   part is expected: a date field ignores one."
  (declare (type string string))
  (let ((n (length string)) (i 0))
    (declare (type fixnum n i))
    (labels ((digits (count)
               (when (<= (+ i count) n)
                 (let ((v 0))
                   (dotimes (k count)
                     (let ((d (digit-char-p (char string (+ i k)))))
                       (unless d (return-from digits nil))
                       (setf v (+ (* v 10) d))))
                   (incf i count)
                   v)))
             (next (c)
               (when (and (< i n) (char= (char string i) c))
                 (incf i)
                 t)))
      (let ((year (digits 4)) month day)
        (unless (and year (next #\-) (setf month (digits 2)) (next #\-) (setf day (digits 2))
                     (<= 1 month 12) (<= 1 day 31))
          (return-from %parse-temporal nil))
        (when (or (= i n) (equal kind "date"))
          (return-from %parse-temporal
            (local-time:encode-timestamp 0 0 0 0 day month year :offset 0)))
        (unless (or (next #\T) (next #\Space))
          (return-from %parse-temporal nil))
        (let ((hour (digits 2)) minute (second 0) (nsec 0) (offset 0))
          (unless (and hour (next #\:) (setf minute (digits 2))
                       (<= hour 23) (<= minute 59))
            (return-from %parse-temporal nil))
          (when (next #\:)
            (setf second (digits 2))
            (unless (and second (<= second 60))
              (return-from %parse-temporal nil)))
          (when (next #\.)
            (let ((start i))
              (loop while (and (< i n) (digit-char-p (char string i))) do (incf i))
              (let ((len (- i start)))
                (unless (<= 1 len 9) (return-from %parse-temporal nil))
                (setf nsec (* (parse-integer string :start start :end i)
                              (expt 10 (- 9 len)))))))
          (cond ((next #\Z))
                ((and (< i n) (member (char string i) '(#\+ #\-)))
                 (let ((sign (if (char= (char string i) #\-) -1 1)))
                   (incf i)
                   (let ((oh (digits 2)) (om 0))
                     (unless oh (return-from %parse-temporal nil))
                     (next #\:)
                     (when (< i n)
                       (setf om (digits 2))
                       (unless om (return-from %parse-temporal nil)))
                     (setf offset (* sign (+ (* oh 3600) (* om 60))))))))
          (unless (= i n) (return-from %parse-temporal nil))
          (local-time:encode-timestamp nsec second minute hour day month year :offset offset))))))

(defun %from-salesforce (value kind)
  "VALUE as stored in a slot: a date/datetime STRING of KIND becomes a
   timestamp; anything else (including a string that doesn't parse) is kept."
  (or (and (stringp value) (%parse-temporal value kind)) value))

(defun %to-salesforce (value kind)
  "VALUE as sent to Salesforce: a timestamp becomes text; anything else is kept."
  (if (typep value 'local-time:timestamp) (%timestamp-text value kind) value))

;;; ──────────────────────────────────────────────────────────────────
;;; Base class
;;; ──────────────────────────────────────────────────────────────────

(defclass sobject ()
  ((sf-id   :initarg  :id
            :accessor sobject-id
            :initform nil
            :type     (or string null)
            :documentation "Salesforce record Id (18-char string).")
   (sf-type :initarg  :type
            :accessor sobject-type
            :initform nil
            :type     (or string null)
            :documentation "Salesforce API name of the sObject type, e.g. \"Account\".")
   (sf-raw  :initarg  :raw
            :accessor sobject-raw
            :initform nil
            :documentation
            "Original jzon hash-table from the API response.
             Always populated; used as authoritative storage for:
               - Unknown/unregistered sObject types (entire record lives here)
               - Fields not mapped to named slots on typed subclasses
               - Read-back of fields set server-side (e.g. CreatedDate)"))
  (:documentation
   "Base class for all Salesforce sObject records.
    Typed subclasses are generated by DEFINE-SOBJECT-CLASS.
    For types without a registered class, a plain SOBJECT is returned
    with the full raw hash-table accessible via SOBJECT-RAW."))

(defun %print-to-string (obj)
  (with-output-to-string (s) (print-object obj s)))

(defun %sobject-print-field-value (v)
  "Render one SOBJECT-RAW field value for PRINT-OBJECT, recursively —
   a nested record should print exactly as it would on its own.

   Two hash-table shapes occur in real Salesforce responses:
     - A relationship record (dot-path fields like CreatedBy.Name):
       {\"attributes\": {\"type\": \"User\", ...}, \"Name\": \"...\"}
       → converted via HASH->SOBJECT (no CONN — printing must stay a pure,
       network-free operation, so an unregistered type just prints generic)
       and printed the same way any other sobject would be.
     - A child sub-query wrapper (e.g. (SELECT Id FROM Contacts) nested in
       a field list): {\"totalSize\": N, \"done\": T, \"records\": [...]}
       → each element of RECORDS gets the same recursive treatment,
       joined as a bracketed list.

   STRING is checked before VECTOR — strings are vectors in Lisp, and
   would otherwise get treated as a sub-query array."
  (etypecase v
    (hash-table
     (let ((records (gethash "records" v)))
       (if records
           (format nil "[~{~A~^, ~}]"
                   (loop for r across records
                         collect (%print-to-string (hash->sobject r))))
           (%print-to-string (hash->sobject v)))))
    (string v)
    (vector (format nil "[~A records]" (length v)))
    (t      v)))

(defmethod print-object ((obj sobject) stream)
  ;; SOBJECT-RAW holds exactly the fields actually returned by the query
  ;; (it's the original API response hash-table) — its KEYS say which
  ;; fields to show, not every field the sObject type could have.  But
  ;; its VALUES are a frozen fetch-time snapshot: for any key that maps
  ;; to a real slot (via *SOBJECT-SLOT-MAPS*), show the slot's CURRENT
  ;; value instead, so a SETF made after the fetch (e.g. (SETF
  ;; (ACCOUNT-NAME acct) "Foo")) is reflected in what prints — matching
  ;; what SOBJECT->HASH-TABLE would actually send on DATABASE-UPDATE.
  ;; Keys with no slot (relationship fields, joined ALIAS.Field keys)
  ;; keep showing the raw fetched value — there's no live state for them.
  ;; Falls back to the old compact "#<Type Id>" form when there's no raw
  ;; response yet (a freshly MAKE-INSTANCE'd, not-yet-saved object).
  (let ((raw (sobject-raw obj)))
    (if (and raw (plusp (hash-table-count raw)))
        (let ((slot-map (gethash (class-name (class-of obj)) *sobject-slot-maps*)))
          (format stream "~A(~{~A~^, ~})"
                  (let ((type (sobject-type obj)))
                    (if (and type (plusp (length type))) type "SObject"))
                  (loop for k being the hash-keys of raw using (hash-value v)
                        unless (equal k "attributes")
                        collect (format nil "~A=~A" k
                                        (%sobject-print-field-value
                                         (let ((entry (and slot-map (assoc k slot-map :test #'equal))))
                                           (if (and entry (slot-boundp obj (cdr entry)))
                                               (slot-value obj (cdr entry))
                                               v)))))))
        (print-unreadable-object (obj stream :type t :identity nil)
          (format stream "~A ~A"
                  (or (sobject-type obj) "?")
                  (or (sobject-id   obj) "(unsaved)"))))))

;;; ──────────────────────────────────────────────────────────────────
;;; Read-only fields
;;; ──────────────────────────────────────────────────────────────────

(defparameter *sf-read-only-fields*
  '("Id" "CreatedDate" "CreatedById" "LastModifiedDate" "LastModifiedById"
    "SystemModstamp" "IsDeleted" "LastActivityDate" "LastViewedDate"
    "LastReferencedDate" "attributes")
  "Salesforce fields that are always server-managed and must be omitted
   from create / update / upsert request bodies.")

;;; ──────────────────────────────────────────────────────────────────
;;; Conversion: hash-table → SOBJECT
;;; ──────────────────────────────────────────────────────────────────

;;; ──────────────────────────────────────────────────────────────────
;;; Lazy class registration — DESCRIBE-SOBJECT on first encounter of a
;;; type, so callers never have to remember to call LOAD-SOBJECT-CLASSES.
;;; ──────────────────────────────────────────────────────────────────

(defvar *sobject-registration-lock* (sb-thread:make-mutex :name "sobject-class-registration")
  "Guards lazy CLOS class generation (EVAL + DEFINE-SOBJECT-CLASS) against
   concurrent registration of the SAME type from multiple threads — e.g.
   two threads (e.g. parallel bulk workers) that both first encounter a
   given sObject type at the same time.")

(defun %eval-define-sobject-class (name desc)
  "Build and EVAL a DEFINE-SOBJECT-CLASS form for sObject NAME from its
   DESCRIBE-SOBJECT hash-table DESC.  Returns the generated class-name
   symbol.  Shared by LOAD-SOBJECT-CLASSES (bulk, parallel describes) and
   %REGISTER-SOBJECT-CLASS (lazy, one type at a time)."
  (let ((class-sym (intern (string-upcase name) *package*)))
    (eval `(define-sobject-class
               ,class-sym ,name
             ,@(loop for field across (gethash "fields" desc #())
                     for sf-name = (gethash "name" field)
                     for sf-type = (gethash "type" field)
                     for temporal = (member sf-type '("date" "datetime") :test #'equal)
                     ;; read-only dates (CreatedDate, LastModifiedDate, ...) get a
                     ;; slot too, so they read as timestamps; writes skip them by name
                     when (or temporal
                              (not (member sf-name *sf-read-only-fields* :test #'equal)))
                       collect (if temporal
                                   (list (intern (string-upcase sf-name)) sf-name :sf-type sf-type)
                                   (list (intern (string-upcase sf-name)) sf-name)))))
    class-sym))

(defun %register-sobject-class (conn name &optional desc)
  "DESCRIBE-SOBJECT NAME via CONN (unless DESC, that describe result, is
   already in hand) and register a CLOS class for it in
   *SOBJECT-CLASSES*.  Returns the class, or NIL if DESCRIBE-SOBJECT fails
   (caching the failure as :FAILED so repeated records of an undescribable
   type — no permission, doesn't exist — don't each retry the API call)."
  (sb-thread:with-mutex (*sobject-registration-lock*)
    ;; Re-check — another thread may have resolved this type while we waited.
    (let ((existing (gethash name *sobject-classes*)))
      (when existing
        (return-from %register-sobject-class (unless (eq existing :failed) existing))))
    (handler-case
        (find-class (%eval-define-sobject-class
                     name (or desc (describe-sobject name :conn conn))))
      (error (e)
        (warn "cl-sf: lazy describe-sobject failed for ~A: ~A" name e)
        (setf (gethash name *sobject-classes*) :failed)
        nil))))

(defun hash->sobject (hash &optional hint-type conn)
  "Convert a jzon hash-table HASH to a SOBJECT instance.

   HINT-TYPE — optional Salesforce API name string.  If nil, the type is
               read from the 'attributes.type' key that Salesforce embeds
               in every query record.
   CONN      — if the type has no registered class yet, used to lazily
               DESCRIBE-SOBJECT and generate one on the spot (see
               %REGISTER-SOBJECT-CLASS); if nil, falls back to generic
               SOBJECT immediately, same as before this existed.

   Returns a typed subclass instance if one is registered in *SOBJECT-CLASSES*
   (loading it lazily via CONN if needed), or a plain SOBJECT with the full
   hash stored in SF-RAW otherwise."
  (when (null hash) (return-from hash->sobject nil))
  (let* ((attrs   (and (hash-table-p hash)
                       (gethash "attributes" hash)))
         (sf-type (or hint-type
                      (and (hash-table-p attrs) (gethash "type" attrs))
                      ""))
         (id      (gethash "Id" hash))
         (registered (gethash sf-type *sobject-classes*))
         (class   (cond
                    ((and registered (not (eq registered :failed))) registered)
                    ((eq registered :failed) nil)
                    ((and conn (plusp (length sf-type)))
                     (%register-sobject-class conn sf-type))
                    (t nil))))
    (if class
        ;; ── Typed path: known (or newly lazy-registered) class ───────
        (let* ((obj      (make-instance class :id id :type sf-type :raw hash))
               (slot-map (gethash (class-name class) *sobject-slot-maps*)))
          (when slot-map
            (let ((kinds (gethash (class-name class) *sobject-field-kinds*)))
              (dolist (entry slot-map)
                (let* ((sf-name (car entry))
                       (val     (gethash sf-name hash)))
                  ;; a JSON null parses as the symbol CL:NULL: an empty field
                  (unless (or (null val) (eq val 'null))
                    (setf (slot-value obj (cdr entry))
                          (let ((kind (and kinds (gethash sf-name kinds))))
                            (if kind (%from-salesforce val kind) val))))))))
          obj)
        ;; ── Generic fallback: unknown type, or no CONN to describe it ─
        (make-instance 'sobject :id id :type sf-type :raw hash))))

;;; ──────────────────────────────────────────────────────────────────
;;; Conversion: SOBJECT → hash-table
;;; ──────────────────────────────────────────────────────────────────

(defvar *writable-slot-cache* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "slot-map -> (read-only-list . slot-map entries that are not read-only).
   A class's slot map is replaced (never mutated) when it is redefined, and
   *SF-READ-ONLY-FIELDS* is compared by identity, so a stale entry can't be
   returned.")

(defvar *read-only-set-cache* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "read-only field list -> EQUAL hash-table of its members.")

(defun %read-only-set (list)
  "LIST (normally *SF-READ-ONLY-FIELDS*) as a hash-set, for O(1) membership."
  (or (gethash list *read-only-set-cache*)
      (setf (gethash list *read-only-set-cache*)
            (let ((set (make-hash-table :test #'equal)))
              (dolist (field list set) (setf (gethash field set) t))))))

(defun %writable-slots (slot-map)
  "SLOT-MAP's (sf-name . slot-name) entries minus the read-only fields,
   computed once per slot map instead of once per record."
  (let ((cached (gethash slot-map *writable-slot-cache*))
        (read-only *sf-read-only-fields*))
    (if (and cached (eq (car cached) read-only))
        (cdr cached)
        (let* ((set     (%read-only-set read-only))
               (entries (remove-if (lambda (entry) (gethash (car entry) set)) slot-map)))
          (setf (gethash slot-map *writable-slot-cache*) (cons read-only entries))
          entries))))

(defun sobject->hash-table (obj)
  "Convert SOBJECT OBJ to a hash-table suitable for JSON serialisation.

   Rules:
     - NIL slot values are omitted (Salesforce treats absent fields as
       unchanged on update, avoiding accidental nullification).
     - Fields listed in *SF-READ-ONLY-FIELDS* are always excluded.
     - For typed objects: uses the registered slot map.
     - For generic SOBJECT instances: copies from SF-RAW minus read-only fields."
  (let ((slot-map (gethash (class-name (class-of obj)) *sobject-slot-maps*)))
    (if slot-map
        ;; ── Typed path ───────────────────────────────────────────────
        (let* ((entries (%writable-slots slot-map))
               (result  (make-hash-table :test #'equal :size (max 7 (length entries))))
               (kinds   (gethash (class-name (class-of obj)) *sobject-field-kinds*)))
          (dolist (entry entries result)
            (let ((slot (cdr entry)))
              (when (slot-boundp obj slot)
                (let ((val (slot-value obj slot)))
                  (when val
                    (setf (gethash (car entry) result)
                          (if (typep val 'local-time:timestamp)
                              (%to-salesforce val (or (and kinds (gethash (car entry) kinds))
                                                      "datetime"))
                              val))))))))
        ;; ── Generic path: copy from sf-raw ──────────────────────────
        (let ((raw (sobject-raw obj)))
          (if raw
              (let ((result    (make-hash-table :test #'equal :size (max 7 (hash-table-count raw))))
                    (read-only (%read-only-set *sf-read-only-fields*)))
                (maphash (lambda (k v)
                           (unless (gethash k read-only)
                             (setf (gethash k result) v)))
                         raw)
                result)
              (make-hash-table :test #'equal))))))

;;; ──────────────────────────────────────────────────────────────────
;;; define-sobject-class macro
;;; ──────────────────────────────────────────────────────────────────

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %field-initarg (slot-name)
    "The MAKE-INSTANCE keyword for a field's slot: its name, except where that
     is one of SOBJECT's own initargs (:ID :TYPE :RAW) — a field called Type
     is common, and sharing :TYPE with the sObject's type would fill the field
     with the object's name (\"Opportunity\") and write it back on the next
     update.  Those fields take :<NAME>-FIELD instead."
    (let ((key (alexandria:make-keyword slot-name)))
      (if (member key '(:id :type :raw))
          (alexandria:make-keyword (format nil "~A-FIELD" slot-name))
          key))))

(defmacro define-sobject-class (class-name sf-api-name &body field-specs)
  "Define a CLOS class CLASS-NAME representing the Salesforce object SF-API-NAME.
   Registers the class in *SOBJECT-CLASSES* and its field map in *SOBJECT-SLOT-MAPS*.

   FIELD-SPECS — list of (slot-name sf-field-name &key type documentation sf-type)
   A field whose slot name is ID, TYPE or RAW is set with :ID-FIELD, :TYPE-FIELD
   or :RAW-FIELD (the plain keywords belong to SOBJECT itself).
   SF-TYPE \"date\" or \"datetime\" makes the slot hold a LOCAL-TIME:TIMESTAMP:
   text from Salesforce is parsed on the way in, and a timestamp is written
   back as text by SOBJECT->HASH-TABLE and the Bulk CSV writer.

   Generated accessor name: <class-name>-<slot-name>

   Example:
     (define-sobject-class account \"Account\"
       (name           \"Name\"          :type (or string null))
       (phone          \"Phone\"         :type (or string null))
       (annual-revenue \"AnnualRevenue\" :type (or number null))
       (billing-city   \"BillingCity\"   :type (or string null)))

   Usage after definition:
     (let ((acct (cl-sf:sobject-get \"Account\" \"001...\")))
       (account-name acct))          ; → \"Acme Corp\"
       (setf (account-phone acct) \"555-1234\")
       (cl-sf:database-update acct)"
  (let* ((slot-defs
          (mapcar (lambda (spec)
                    (destructuring-bind (slot-name sf-name
                                        &key (type t) documentation sf-type)
                        spec
                      (declare (ignore sf-name sf-type))
                      `(,slot-name
                        :initarg       ,(%field-initarg slot-name)
                        :accessor      ,(intern (format nil "~A-~A"
                                                        class-name slot-name))
                        :initform      nil
                        ,@(unless (eq type t) `(:type ,type))
                        ,@(when documentation `(:documentation ,documentation)))))
                  field-specs))
         (field-map-form
          `(list ,@(mapcar (lambda (spec)
                             (destructuring-bind (slot-name sf-name &rest _) spec
                               (declare (ignore _))
                               `(cons ,sf-name ',slot-name)))
                           field-specs)))
         (temporal-fields
          (loop for (nil sf-name . options) in field-specs
                for sf-type = (getf options :sf-type)
                when (member sf-type '("date" "datetime") :test #'equal)
                  collect (cons sf-name sf-type))))
    `(progn
       (defclass ,class-name (sobject)
         (,@slot-defs)
         (:default-initargs :type ,sf-api-name)
         (:documentation ,(format nil "CLOS representation of Salesforce ~A." sf-api-name)))
       ;; Register for HASH->SOBJECT dispatch
       (setf (gethash ,sf-api-name *sobject-classes*) (find-class ',class-name))
       ;; Register field↔slot mapping for conversion
       (setf (gethash ',class-name *sobject-slot-maps*) ,field-map-form)
       ;; Which of those slots hold LOCAL-TIME timestamps (none: no entry)
       ,(if temporal-fields
            `(setf (gethash ',class-name *sobject-field-kinds*)
                   (let ((kinds (make-hash-table :test #'equal)))
                     (loop for (sf-name . kind) in ',temporal-fields
                           do (setf (gethash sf-name kinds) kind))
                     kinds))
            `(remhash ',class-name *sobject-field-kinds*))
       ',class-name)))

;;; ──────────────────────────────────────────────────────────────────
;;; SELECT — SOQL written as Lisp tokens, compiled to SOQL text
;;;
;;; (let ((min-revenue 1000000))
;;;   (select Id Name CreatedBy.Name
;;;           FROM Account
;;;           WHERE AnnualRevenue > min-revenue AND Industry = "Technology"
;;;           ORDER BY Name DESC NULLS LAST
;;;           LIMIT 10))
;;;
;;; The grammar is Salesforce's own (clause order and keywords exactly as in
;;; the SOQL reference), with two reader-forced differences: there are no
;;; commas (the Lisp reader rejects a bare `,`, so lists are whitespace
;;; separated), and the leading SELECT is implied by the macro name.
;;;
;;;   fields      Id  Name  Owner.Name  (COUNT Id) cnt  (FIELDS ALL)
;;;               (SELECT Id FROM Contacts)        child subquery
;;;               TYPEOF What WHEN Account THEN Phone ELSE Name END
;;;               A function is a list: (name arg...).  A bare symbol right
;;;               after a function is its alias (SOQL has no AS).
;;;   FROM        FROM Account [alias]
;;;   USING       USING SCOPE Mine
;;;   WHERE       infix: field op value, joined by AND / OR / NOT and
;;;               parentheses.  op is = != <> < <= > >= LIKE IN NOT IN
;;;               INCLUDES EXCLUDES.  The field side may be a function:
;;;               (CALENDAR_YEAR CloseDate) = 2024
;;;   WITH        passthrough: WITH SECURITY_ENFORCED, WITH DATA CATEGORY
;;;               Geography__c AT (usa__c uk__c), ...
;;;   GROUP BY    fields, or (ROLLUP f...) / (CUBE f...); then HAVING cond
;;;   ORDER BY    field [ASC|DESC] [NULLS FIRST|LAST] ...
;;;   LIMIT n  OFFSET n  FOR VIEW|REFERENCE|UPDATE  UPDATE TRACKING|VIEWSTAT
;;;   SET OPTIONS (name = value ...)
;;;   ALL ROWS    (SELECT only) include deleted/archived rows (queryAll)
;;;
;;; VALUES (right of an operator, LIMIT, OFFSET) need no wrapper:
;;;   "text" 42        literals (strings are escaped)
;;;   min-revenue      a symbol bound in the surrounding Lisp code (let,
;;;                    function parameter, special) -> its value, escaped
;;;   (* base 2)       any other list is a Lisp expression -> its value
;;;   NULL TRUE TODAY  an unbound symbol is emitted as SOQL text
;;;   (RAW "LAST_N_DAYS:5")   verbatim text, for tokens the Lisp reader
;;;                    can't read (a colon would be a package marker)
;;; After IN / INCLUDES / EXCLUDES a list is literal values if it starts with
;;; a string or number ("A" "B"); (SELECT ...) is a semi-join subquery; any
;;; other list or bound symbol is a Lisp expression returning a list.
;;; Runtime values are rendered by %SOQL-RENDER-VALUE, which escapes them —
;;; never splice a Lisp string into SOQL text yourself.
;;;
;;; Everything static folds into one constant string at macroexpansion
;;; time; only value positions that use Lisp code defer to a runtime
;;; CONCATENATE.
;;; ──────────────────────────────────────────────────────────────────

(defun %soql-escape-string (s)
  "Escape S for embedding as a SOQL/Apex single-quoted string literal."
  (with-output-to-string (out)
    (loop for c across s
          do (case c
               (#\' (write-string "\\'" out))
               (#\\ (write-string "\\\\" out))
               (t (write-char c out))))))

(defun %soql-literal-string (s)
  (format nil "'~A'" (%soql-escape-string s)))

(defun %soql-render-value (val)
  "Render an arbitrary Lisp VAL as SOQL text, safely escaped.  Used both at
   macroexpansion time (literals) and at runtime (Lisp values in a query)."
  (etypecase val
    (string               (%soql-literal-string val))
    (null                 "NULL")
    ((eql t)              "TRUE")
    (integer              (princ-to-string val))
    (real                 (let ((*read-default-float-format* 'double-float))
                            (prin1-to-string (float val 1d0))))
    (local-time:timestamp (%timestamp-text val "datetime"))
    (list                 (format nil "(~{~A~^, ~})" (mapcar #'%soql-render-value val)))
    (vector               (%soql-render-value (coerce val 'list)))))

;;; — Piece lists —
;;; Every %SOQL-COMPILE-* / %SOQL-PARSE-* function returns a flat list of
;;; PIECES: a literal string, or a Lisp form that evaluates to a string at
;;; runtime.  %SOQL-STRING joins them.

(defun %soql-cat (&rest piece-lists) (apply #'append piece-lists))

(defun %soql-interpose (piece-lists sep)
  "Join PIECE-LISTS with literal string SEP between each."
  (loop for (pl . more) on piece-lists
        append pl
        when more collect sep))

(defvar *soql-from* nil
  "While a query is compiled: the sObject name of its FROM clause (a string).")
(defvar *soql-field* nil
  "While a condition's value is compiled: the field being compared (a string),
   or NIL if the left side is not a plain field.")

(defun %soql-kw= (x name)
  (and x (symbolp x) (string-equal (symbol-name x) name)))

(defun %soql-earmuffed-p (sym)
  "T if SYM follows Lisp's *special* or +constant+ earmuff naming.  Fallback
   signal on implementations without SB-CLTL2 (see %SOQL-VARIABLE-P)."
  (let* ((name (symbol-name sym)) (len (length name)))
    (and (> len 2)
         (or (and (char= (char name 0) #\*) (char= (char name (1- len)) #\*))
             (and (char= (char name 0) #\+) (char= (char name (1- len)) #\+))))))

(defun %soql-variable-p (sym env)
  "T if SYM is bound — lexically, specially, or as a symbol-macro — at the
   call site, i.e. it's a real Lisp variable in scope rather than a SOQL
   field or keyword.  Only ever asked about VALUE positions, so a field
   name elsewhere can never be hijacked by a same-named variable.

   On SBCL this asks the real lexical environment via SB-CLTL2.  Elsewhere
   it falls back to the earmuff convention (%SOQL-EARMUFFED-P)."
  #+sbcl
  (and (member (sb-cltl2:variable-information sym env) '(:lexical :special :symbol-macro))
       t)
  #-sbcl
  (progn env (%soql-earmuffed-p sym)))

;;; — Words, items, functions (compile-time text only) —

(defun %soql-word (tok)
  "TOK (symbol, string or number) as literal SOQL text."
  (etypecase tok
    (symbol (symbol-name tok))
    ((or string number) (%soql-render-value tok))))

(defun %soql-function-text (form)
  "(COUNT Id) -> \"COUNT(ID)\"; arguments may be words or nested functions."
  (unless (and (consp form) (symbolp (car form)))
    (error "cl-sf: malformed SOQL function call: ~S" form))
  (format nil "~A(~{~A~^, ~})" (symbol-name (car form))
          (mapcar (lambda (a) (if (consp a) (%soql-function-text a) (%soql-word a)))
                  (cdr form))))

(defun %soql-item (tok)
  "A field reference or function call, as text."
  (cond ((consp tok) (%soql-function-text tok))
        ((symbolp tok) (symbol-name tok))
        (t (error "cl-sf: malformed SOQL field: ~S" tok))))

;;; — Values —

(defun %soql-value (v env)
  "Compile one scalar value; see the section header for the rules."
  (cond
    ((stringp v)  (list (%soql-literal-string v)))
    ((numberp v)  (list (%soql-render-value v)))
    ((null v)     (list "NULL"))
    ((eq v t)     (list "TRUE"))
    ((symbolp v)  (if (%soql-variable-p v env)
                      (list `(%soql-render-value-for ,v ,*soql-from* ,*soql-field*))
                      (list (symbol-name v))))
    ((%soql-kw= (car v) "RAW")
     (unless (stringp (second v))
       (error "cl-sf: (RAW ...) takes a literal string, got ~S" v))
     (list (second v)))
    ((consp v)    (list `(%soql-render-value-for ,v ,*soql-from* ,*soql-field*)))
    (t (error "cl-sf: malformed SOQL value: ~S" v))))

(defun %soql-value-list (v env)
  "Compile the right side of IN / NOT IN / INCLUDES / EXCLUDES."
  (cond
    ((and (consp v) (%soql-kw= (car v) "SELECT"))
     (%soql-cat '("(") (%soql-compile-query (cdr v) env) '(")")))
    ((and (consp v) (not (symbolp (car v))))      ; ("A" "B" 3) literal list
     (%soql-cat '("(")
                (%soql-interpose (mapcar (lambda (e) (%soql-value e env)) v) ", ")
                '(")")))
    (t (%soql-value v env))))

;;; — WHERE / HAVING conditions: recursive descent, OR < AND < NOT —
;;; Each %SOQL-PARSE-* takes the remaining token list and returns
;;; (values pieces remaining-tokens).  Multi-part AND/OR groups are always
;;; parenthesized, which is what SOQL requires when the two are mixed.

(defparameter *soql-comparison-ops* '("=" "!=" "<>" "<" "<=" ">" ">=" "LIKE"))
(defparameter *soql-list-ops*       '("IN" "INCLUDES" "EXCLUDES"))

(defun %soql-operator-p (tok)
  (and tok (symbolp tok)
       (let ((n (symbol-name tok)))
         (or (string= n "NOT")
             (member n *soql-comparison-ops* :test #'string=)
             (member n *soql-list-ops* :test #'string=)))))

(defun %soql-parse-chain (toks env sep parse-operand)
  "Parse OPERAND (SEP OPERAND)* — shared by OR and AND."
  (let (parts)
    (loop
      (multiple-value-bind (pieces rest) (funcall parse-operand toks env)
        (push pieces parts)
        (setf toks rest))
      (if (%soql-kw= (car toks) sep)
          (pop toks)
          (return)))
    (values (if (cdr parts)
                (%soql-cat '("(") (%soql-interpose (nreverse parts) (format nil " ~A " sep)) '(")"))
                (first parts))
            toks)))

(defun %soql-parse-or (toks env)
  (%soql-parse-chain toks env "OR" #'%soql-parse-and))

(defun %soql-parse-and (toks env)
  (%soql-parse-chain toks env "AND" #'%soql-parse-primary))

(defun %soql-parse-primary (toks env)
  (let ((tok (car toks)))
    (cond
      ((null toks) (error "cl-sf: SOQL condition ended unexpectedly"))
      ((%soql-kw= tok "NOT")
       (multiple-value-bind (pieces rest) (%soql-parse-primary (cdr toks) env)
         (values (%soql-cat '("NOT ") pieces) rest)))
      ;; A list followed by an operator is a function on the field side;
      ;; any other list is a parenthesized sub-condition.
      ((and (consp tok) (not (%soql-operator-p (cadr toks))))
       (values (%soql-compile-condition tok env) (cdr toks)))
      (t (%soql-parse-comparison toks env)))))

(defun %soql-parse-comparison (toks env)
  (let* ((field-token (first toks))
         (lhs  (%soql-item (pop toks)))
         (*soql-field* (and (symbolp field-token) (symbol-name field-token)))
         (op   (pop toks))
         (name (if (and op (symbolp op)) (symbol-name op)
                   (error "cl-sf: expected a SOQL operator after ~A, got ~S" lhs op))))
    (when (string= name "NOT")
      (unless (%soql-kw= (pop toks) "IN")
        (error "cl-sf: NOT after ~A must be followed by IN" lhs))
      (setf name "NOT IN"))
    (unless toks (error "cl-sf: missing value after ~A ~A" lhs name))
    (let ((head (list (format nil "~A ~A " lhs name))))
      (cond
        ((or (string= name "NOT IN") (member name *soql-list-ops* :test #'string=))
         (values (%soql-cat head (%soql-value-list (pop toks) env)) toks))
        ((member name *soql-comparison-ops* :test #'string=)
         (values (%soql-cat head (%soql-value (pop toks) env)) toks))
        (t (error "cl-sf: unknown SOQL operator ~A" name))))))

(defun %soql-compile-condition (toks env)
  "Compile all of TOKS as one WHERE/HAVING condition."
  (multiple-value-bind (pieces rest) (%soql-parse-or toks env)
    (when rest (error "cl-sf: unexpected ~S in SOQL condition" (car rest)))
    pieces))

;;; — Clauses —

(defun %soql-compile-typeof (toks)
  "TOKS follow TYPEOF, up to and including END.  Returns (values text rest)."
  (let ((end (position-if (lambda (x) (%soql-kw= x "END")) toks)))
    (unless end (error "cl-sf: TYPEOF without END"))
    (let ((body (subseq toks 0 end)) (fields nil) (first-field nil))
      (values
       (with-output-to-string (s)
         (format s "TYPEOF ~A" (%soql-word (pop body)))
         (dolist (tok body)
           (let ((word (%soql-word tok)))
             (cond ((%soql-kw= tok "WHEN") (setf fields nil) (format s " ~A" word))
                   ((or (%soql-kw= tok "THEN") (%soql-kw= tok "ELSE"))
                    (setf fields t first-field t) (format s " ~A" word))
                   (fields (format s (if first-field " ~A" ", ~A") word)
                           (setf first-field nil))
                   (t (format s " ~A" word)))))
         (write-string " END" s))
       (nthcdr (1+ end) toks)))))

(defun %soql-compile-select-list (toks env)
  (let (items)
    (loop while toks
          do (let ((tok (pop toks)))
               (push
                (cond
                  ((%soql-kw= tok "TYPEOF")
                   (multiple-value-bind (text rest) (%soql-compile-typeof toks)
                     (setf toks rest)
                     (list text)))
                  ((and (consp tok) (%soql-kw= (car tok) "SELECT"))
                   (%soql-cat '("(") (%soql-compile-query (cdr tok) env) '(")")))
                  ((consp tok)
                   (let ((text (%soql-function-text tok)))
                     ;; a symbol right after a function is its alias
                     (if (and toks (symbolp (car toks)) (not (%soql-kw= (car toks) "TYPEOF")))
                         (list (format nil "~A ~A" text (symbol-name (pop toks))))
                         (list text))))
                  (t (list (%soql-item tok))))
                items)))
    (unless items (error "cl-sf: SELECT needs at least one field"))
    (%soql-interpose (nreverse items) ", ")))

(defun %soql-order-by-text (toks)
  (let (entries)
    (dolist (tok toks)
      (if (and entries (symbolp tok)
               (member (symbol-name tok) '("ASC" "DESC" "NULLS" "FIRST" "LAST")
                       :test #'string-equal))
          (setf (car entries) (format nil "~A ~A" (car entries) (symbol-name tok)))
          (push (%soql-item tok) entries)))
    (format nil "~{~A~^, ~}" (nreverse entries))))

(defun %soql-passthrough-text (toks)
  "WITH / SET OPTIONS arguments: words as-is, a list as \"(a, b)\"."
  (format nil "~{~A~^ ~}"
          (mapcar (lambda (tok)
                    (if (consp tok)
                        (format nil "(~{~A~^, ~})" (mapcar #'%soql-word tok))
                        (%soql-word tok)))
                  toks)))

(defun %soql-set-options-text (toks)
  "SET OPTIONS (name = value ...) -> \"SET OPTIONS (name = value, ...)\"."
  (let ((opts (first toks)))
    (unless (and (consp opts) (null (cdr toks)) (zerop (mod (length opts) 3)))
      (error "cl-sf: SET OPTIONS takes one list of name = value triples"))
    (format nil "SET OPTIONS (~{~A~^, ~})"
            (loop for (n eq v) on opts by #'cdddr
                  collect (format nil "~A ~A ~A" (%soql-word n) (%soql-word eq) (%soql-word v))))))

(defun %soql-single-token (kw toks)
  (unless (and toks (null (cdr toks)))
    (error "cl-sf: ~A takes exactly one argument, got ~S" kw toks))
  (first toks))

(defun %soql-compile-clause (kw toks env)
  (flet ((text (fmt &rest args) (list (apply #'format nil fmt args))))
    (cond
      ((string= kw "SELECT")   (%soql-cat (list "SELECT ") (%soql-compile-select-list toks env)))
      ((string= kw "FROM")     (text "FROM ~{~A~^ ~}" (mapcar #'%soql-word toks)))
      ((string= kw "USING SCOPE") (text "USING SCOPE ~A" (%soql-word (%soql-single-token kw toks))))
      ((string= kw "WHERE")    (%soql-cat '("WHERE ")  (%soql-compile-condition toks env)))
      ((string= kw "HAVING")   (%soql-cat '("HAVING ") (%soql-compile-condition toks env)))
      ((string= kw "WITH")     (text "WITH ~A" (%soql-passthrough-text toks)))
      ((string= kw "GROUP BY") (text "GROUP BY ~{~A~^, ~}" (mapcar #'%soql-item toks)))
      ((string= kw "ORDER BY") (text "ORDER BY ~A" (%soql-order-by-text toks)))
      ((string= kw "LIMIT")    (%soql-cat '("LIMIT ")  (%soql-value (%soql-single-token kw toks) env)))
      ((string= kw "OFFSET")   (%soql-cat '("OFFSET ") (%soql-value (%soql-single-token kw toks) env)))
      ((string= kw "FOR")      (text "FOR ~A" (%soql-word (%soql-single-token kw toks))))
      ((string= kw "UPDATE")   (text "UPDATE ~A" (%soql-word (%soql-single-token kw toks))))
      ((string= kw "SET OPTIONS") (list (%soql-set-options-text toks)))
      (t (error "cl-sf: unknown SOQL clause ~A" kw)))))

(defun %soql-clause-at (toks)
  "If TOKS starts a top-level clause, return (values keyword-string
   keyword-width-in-tokens takes-one-argument-token-p)."
  (let ((a (first toks)) (b (second toks)))
    (flet ((kw (x name) (%soql-kw= x name))
           (one (name) (values name 1 nil)))
      (cond
        ((kw a "FROM")  (values "FROM" 1 t))
        ((kw a "WHERE")  (one "WHERE"))
        ((kw a "WITH")   (one "WITH"))
        ((kw a "HAVING") (one "HAVING"))
        ((kw a "LIMIT")  (one "LIMIT"))
        ((kw a "OFFSET") (one "OFFSET"))
        ((and (kw a "USING") (kw b "SCOPE")) (values "USING SCOPE" 2 nil))
        ((and (kw a "GROUP") (kw b "BY"))    (values "GROUP BY" 2 nil))
        ((and (kw a "ORDER") (kw b "BY"))    (values "ORDER BY" 2 nil))
        ((and (kw a "SET") (kw b "OPTIONS")) (values "SET OPTIONS" 2 nil))
        ((and (kw a "FOR") (or (kw b "VIEW") (kw b "REFERENCE") (kw b "UPDATE")))
         (values "FOR" 1 t))
        ((and (kw a "UPDATE") (or (kw b "TRACKING") (kw b "VIEWSTAT")))
         (values "UPDATE" 1 t))))))

(defun %soql-split-clauses (toks)
  "TOKS (everything after the implied SELECT) -> ((\"SELECT\" . tokens)
   (\"FROM\" . tokens) ...) in source order.  Two-word keywords are matched
   as pairs, and the token after FROM is always the object type, so an
   sObject named Order or Group can't be mistaken for a clause."
  (let ((clauses (list (list "SELECT"))))
    (loop while toks
          do (multiple-value-bind (kw width arg-p) (%soql-clause-at toks)
               (cond (kw (push (list kw) clauses)
                         (setf toks (nthcdr width toks))
                         (when arg-p (push (pop toks) (cdr (first clauses)))))
                     (t (push (pop toks) (cdr (first clauses)))))))
    (mapcar (lambda (c) (cons (car c) (reverse (cdr c)))) (nreverse clauses))))

(defun %soql-compile-query (toks env)
  "TOKS — a query after its implied SELECT.  Returns a flat piece list; used
   for the top-level query and for nested subqueries."
  (let* ((clauses (%soql-split-clauses toks))
         (from    (first (cdr (assoc "FROM" clauses :test #'string=))))
         (*soql-from* (and (symbolp from) from (symbol-name from))))
    (%soql-interpose
     (loop for (kw . ctoks) in clauses
           collect (%soql-compile-clause kw ctoks env))
     " ")))

(defun %soql-string (toks env)
  "A form that evaluates to the query text for TOKS (a constant string when
   nothing in it is a runtime value)."
  (let ((pieces (%soql-compile-query toks env)))
    (if (every #'stringp pieces)
        (apply #'concatenate 'string pieces)
        `(concatenate 'string ,@pieces))))

(defmacro soql (&body clauses &environment env)
  "The SOQL text SELECT would run, without running it — for debugging and
   for tests that shouldn't touch the network.  Same syntax as SELECT, but
   written with its leading SELECT keyword, like the SOQL itself:
     (soql SELECT Id FROM Account WHERE Name = \"Acme\")"
  (unless (%soql-kw= (car clauses) "SELECT")
    (error "cl-sf: SOQL must start with SELECT"))
  (%soql-string (cdr clauses) env))

;;; ──────────────────────────────────────────────────────────────────
;;; Public API — SELECT
;;; ──────────────────────────────────────────────────────────────────

(defun %select (soql all-rows)
  "Run SOQL on *CONNECTION*, fetching every page.  Returns a SIMPLE-VECTOR
   of SOBJECTs."
  (let ((conn *connection*))
    (cl-sf/api::%query-records conn soql
                               :all all-rows
                               :coercer (lambda (h) (hash->sobject h nil conn)))))

(defmacro select (&body clauses &environment env)
  "Run a SOQL query on *CONNECTION* (see WITH-CONN) and return every
   matching record, all pages fetched, as a SIMPLE-VECTOR of SOBJECTs.
   Syntax: SOQL itself with the leading SELECT implied and no commas — see
   the grammar above.  A trailing ALL ROWS includes deleted/archived rows.

   Examples:
     (with-conn \"myorg\"
       (let ((min-revenue 1000000))
         (select Id Name
                 FROM Account
                 WHERE AnnualRevenue > min-revenue AND Industry IN (\"Tech\" \"Finance\")
                 ORDER BY Name
                 LIMIT 10)))

     (select (COUNT Id) n Industry FROM Account GROUP BY Industry)
     (select Id FROM Account WHERE Name = \"Acme\" ALL ROWS)"
  (let* ((all-rows (and (cdr clauses)
                        (%soql-kw= (car (last clauses 2)) "ALL")
                        (%soql-kw= (car (last clauses)) "ROWS")))
         (toks     (if all-rows (butlast clauses 2) clauses)))
    `(%select ,(%soql-string toks env) ,all-rows)))

;;; ──────────────────────────────────────────────────────────────────
;;; Public API — sObject CRUD
;;; ──────────────────────────────────────────────────────────────────

(defun sobject-get (type id &key fields (conn *connection*))
  "Fetch a single record by TYPE (string) and ID (string).
   FIELDS — optional list of field name strings to retrieve.
   Returns a typed SOBJECT instance (or plain SOBJECT if class not registered).

   Example:
     (sobject-get \"Account\" \"001xx000000001AAA\")"
  (hash->sobject (cl-sf/api::%sobject-get conn type id :fields fields) type conn))

(defvar *key-prefix-cache* (make-hash-table :test #'equal :synchronized t)
  "instance-url -> hash-table of 3-character Id key prefix -> sObject name.")

(defun %key-prefix-table (conn &optional refresh)
  "Key prefix -> sObject API name for CONN's org, from DESCRIBE-GLOBAL (one
   request per org, then cached).  REFRESH re-fetches, picking up custom
   objects created since."
  (let ((url (connection-instance-url conn)))
    (when refresh (remhash url *key-prefix-cache*))
    (or (gethash url *key-prefix-cache*)
        (setf (gethash url *key-prefix-cache*)
              (let ((table (make-hash-table :test #'equal)))
                (loop for sobject across (or (gethash "sobjects" (cl-sf/api::%describe-global conn)) #())
                      for prefix = (gethash "keyPrefix" sobject)
                      when (stringp prefix)
                        do (setf (gethash prefix table) (gethash "name" sobject)))
                table)))))

(defun %sobject-type-of-id (id conn)
  "The sObject API name for record ID, from its 3-character key prefix
   (every Salesforce Id starts with its object's unique prefix)."
  (flet ((lookup (refresh)
           (and (>= (length id) 15)
                (gethash (subseq id 0 3) (%key-prefix-table conn refresh)))))
    (or (lookup nil)
        (lookup t)          ; maybe a custom object created after the cache
        (error 'sf-api-error
               :message (format nil "Cannot determine the sObject type of ~S — \
its key prefix matches no object in this org." id)))))

(defun %resolve-type (object conn)
  "OBJECT's sObject type.  If it has none, infer it from OBJECT's Id key
   prefix and store it on OBJECT, so everything downstream (REST path,
   Bulk CSV) can just read SOBJECT-TYPE."
  (unless (typep object 'sobject)
    (error 'sf-api-error
           :message (format nil "DATABASE-* calls take SOBJECT instances, got ~S." object)))
  (let ((type (sobject-type object)))
    (cond ((and type (plusp (length type))) type)
          ((sobject-id object)
           (setf (sobject-type object) (%sobject-type-of-id (sobject-id object) conn)))
          (t (error 'sf-api-error
                    :message "SOBJECT instance has neither a type nor an Id.")))))

(defun %require-one-type (objects conn)
  "Signal SF-API-ERROR unless every SOBJECT in the list OBJECTS has the same
   sObject type (a Bulk job, and one DML call, covers a single type)."
  (when objects
    (let ((first-type (%resolve-type (first objects) conn)))
      (dolist (o (rest objects))
        (let ((type (%resolve-type o conn)))
          (unless (string= type first-type)
            (error 'sf-api-error
                   :message (format nil "A DATABASE-* call takes one sObject type, \
but got both ~A and ~A." first-type type))))))))

(defun %insert-one (object conn)
  "Create OBJECT in Salesforce.
   Serialises OBJECT to JSON via SOBJECT->HASH-TABLE, POSTs to the API,
   then sets SOBJECT-ID on OBJECT with the returned Id.
   Returns OBJECT (now with Id populated)."
  (let* ((type   (or (sobject-type object)
                     (error 'sf-api-error
                            :message "SOBJECT instance has no type set — \
cannot determine the Salesforce object type for create.")))
         (result (cl-sf/api::%sobject-create conn type (sobject->hash-table object))))
    (when (hash-table-p result)
      (setf (sobject-id  object) (gethash "id"   result)
            (sobject-raw object) (or (sobject-raw object)
                                     (make-hash-table :test #'equal))))
    object))

(defun %update-one (object conn)
  "Update an existing record in Salesforce.
   Serialises changed slots via SOBJECT->HASH-TABLE and PATCHes the API.
   SOBJECT-ID must be non-nil.  Returns OBJECT."
  (unless (sobject-id object)
    (error 'sf-api-error
           :message "SOBJECT instance has no Id — cannot update a record without an Id. \
Did you mean DATABASE-INSERT?"))
  (cl-sf/api::%sobject-update conn
                               (sobject-type object)
                               (sobject-id   object)
                               (sobject->hash-table object))
  object)

(defun %delete-one (object conn)
  "DELETE OBJECT.  Returns T."
  (unless (sobject-id object)
    (error 'sf-api-error
           :message "SOBJECT instance has no Id — cannot delete a record without an Id."))
  (cl-sf/api::%sobject-delete conn (sobject-type object) (sobject-id object))
  t)

(defun %upsert-one (object external-id-field conn)
  "Upsert OBJECT using EXTERNAL-ID-FIELD as the match key.
   EXTERNAL-ID-FIELD is the Salesforce API field name string (e.g. \"External_Id__c\").
   On insert: sets SOBJECT-ID from the response.  Returns OBJECT."
  (let* ((type         (sobject-type object))
         ;; Resolve the external Id value — look in slot map first, fall back to raw
         (ext-id-value
          (let* ((slot-map  (gethash (class-name (class-of object)) *sobject-slot-maps*))
                 (slot-name (when slot-map
                              (cdr (assoc external-id-field slot-map :test #'equal)))))
            (or (and slot-name (slot-boundp object slot-name)
                     (slot-value object slot-name))
                (and (sobject-raw object)
                     (gethash external-id-field (sobject-raw object)))
                (error 'sf-api-error
                       :message (format nil "Cannot find value for external Id field ~S on ~A"
                                        external-id-field object)))))
         (result (cl-sf/api::%sobject-upsert conn type
                                              external-id-field ext-id-value
                                              (sobject->hash-table object))))
    (when (hash-table-p result)
      (when (gethash "id" result)
        (setf (sobject-id object) (gethash "id" result))))
    object))

;;; ──────────────────────────────────────────────────────────────────
;;; Public API — metadata (pass-through; these return hash-tables)
;;; ──────────────────────────────────────────────────────────────────

(defun describe-global (&key (conn *connection*))
  "Return metadata for all sObject types available in the org.
   Returns a hash-table (raw jzon) — structural metadata, not records.
   Used internally by LOAD-SOBJECT-CLASSES."
  (cl-sf/api::%describe-global conn))

(defun describe-sobject (type &key (conn *connection*))
  "Return full field and relationship metadata for the sObject TYPE.
   Returns a hash-table (raw jzon).
   Used internally by LOAD-SOBJECT-CLASSES and DEFINE-SOBJECT-CLASS callers."
  (cl-sf/api::%describe-sobject conn type))

(defun limits (&key (conn *connection*))
  "Return current org API limits hash-table.
   Keys are limit names (e.g. \"DailyApiRequests\"); values are
   hash-tables with \"Max\" and \"Remaining\"."
  (cl-sf/api::%limits conn))

;;; ──────────────────────────────────────────────────────────────────
;;; Field name introspection (used by cl-sf/bulk for CSV generation)
;;; ──────────────────────────────────────────────────────────────────

(defun sobject-field-names (obj)
  "Return the list of Salesforce API field name strings registered for OBJ's type.
   Returns NIL for plain SOBJECT instances with no registered class.
   Used by cl-sf/bulk to determine CSV column headers without hard-coding names."
  (let ((slot-map (gethash (class-name (class-of obj)) *sobject-slot-maps*)))
    (when slot-map (mapcar #'car slot-map))))

;;; ──────────────────────────────────────────────────────────────────
;;; DML — Apex's Database.insert / update / upsert / delete
;;;
;;; Each takes one SOBJECT or a sequence of SOBJECTs, all of one sObject
;;; type (mixing types signals SF-API-ERROR).  Routing:
;;;   one object                       single-record REST call
;;;   sequence of <= BULK-THRESHOLD    sObject Collections API: 200 records
;;;                                    per request, requests sent in parallel
;;;   longer sequence                  Bulk 2.0
;;; An object with no SOBJECT-TYPE gets it from its Id's key prefix.
;;; ──────────────────────────────────────────────────────────────────

(defun %chunks (list size)
  "Split LIST into consecutive sublists of at most SIZE elements."
  (loop while list
        collect (loop repeat size while list collect (pop list))))

(defun %collection-record (object type &key id)
  "OBJECT as one Collections request record: its fields plus attributes.type
   (and, for updates, Id)."
  (let ((record (sobject->hash-table object))
        (attributes (make-hash-table :test #'equal)))
    (setf (gethash "type" attributes) type
          (gethash "attributes" record) attributes)
    (when id (setf (gethash "Id" record) id))
    record))

(defun %collection-error-text (result)
  "One line describing a failed Collections result's errors."
  (let ((errors (gethash "errors" result)))
    (if (and errors (plusp (length errors)))
        (format nil "~{~A~^; ~}"
                (map 'list (lambda (e)
                             (format nil "~A: ~A~@[ (~{~A~^, ~})~]"
                                     (gethash "statusCode" e) (gethash "message" e)
                                     (let ((f (gethash "fields" e)))
                                       (and f (plusp (length f)) (coerce f 'list)))))
                     errors))
        "unknown error")))

(defun %collection-request (operation chunk type external-id-field conn)
  "Send one chunk (<= 200 objects) through the Collections API.  Returns the
   vector of per-record results."
  (ecase operation
    (:insert (cl-sf/api::%collection-create
              conn (mapcar (lambda (o) (%collection-record o type)) chunk)))
    (:update (cl-sf/api::%collection-update
              conn (mapcar (lambda (o) (%collection-record o type :id (sobject-id o))) chunk)))
    (:upsert (cl-sf/api::%collection-upsert
              conn type external-id-field
              (mapcar (lambda (o) (%collection-record o type)) chunk)))
    (:delete (cl-sf/api::%collection-delete conn (mapcar #'sobject-id chunk)))))

(defun %collection-dml (operation objects conn &key external-id-field)
  "Run OPERATION (:insert :update :upsert :delete) over OBJECTS, a non-empty
   list of one sObject type, via the sObject Collections API: chunks of up
   to 200, sent concurrently when there is more than one chunk (only the
   HTTP calls run on workers; objects are touched afterwards, here).
   Inserted/upserted objects get their Ids.  Every chunk is attempted;
   afterwards, if any record failed, signals SF-DML-ERROR (records that
   succeeded stay committed)."
  (when (member operation '(:update :delete))
    (let ((missing (find-if-not #'sobject-id objects)))
      (when missing
        (error 'sf-api-error
               :message (format nil "SOBJECT instance has no Id — cannot ~(~A~) a record without an Id."
                                operation)))))
  (let* ((type    (sobject-type (first objects)))
         (chunks  (%chunks objects cl-sf/api::*collection-max-records*))
         ;; a chunk whose request itself fails (HTTP error) counts as all of
         ;; its records failing; the other chunks are still carried through
         (request (lambda (chunk)
                    (handler-case
                        (%collection-request operation chunk type external-id-field conn)
                      (error (e) e))))
         (results (if (cdr chunks)
                      (progn (cl-sf/auth::%ensure-lparallel-kernel)
                             (lparallel:pmapcar request chunks))
                      (list (funcall request (first chunks)))))
         (failures '())
         (succeeded 0))
    (loop for chunk in chunks
          for result-vector in results
          do (if (typep result-vector 'error)
                 (dolist (object chunk)
                   (push (cons object (princ-to-string result-vector)) failures))
                 (loop for object in chunk
                       for result across result-vector
                       do (cond ((gethash "success" result)
                                 (incf succeeded)
                                 (when (and (member operation '(:insert :upsert))
                                            (gethash "id" result))
                                   (setf (sobject-id object) (gethash "id" result))))
                                (t (push (cons object (%collection-error-text result))
                                         failures))))))
    (when failures
      (setf failures (nreverse failures))
      (error 'sf-dml-error
             :failures failures
             :succeeded succeeded
             :message (format nil "~D of ~D record~:P failed to ~(~A~) (~D succeeded). First error: ~A"
                              (length failures) (length objects) operation succeeded
                              (cdr (first failures)))))
    objects))

(defun %dml (objects conn bulk-threshold one-fn collection-fn bulk-fn)
  "Dispatch a DML call: ONE-FN for a single object; for a sequence,
   BULK-FN (lambda (list)) when it is longer than BULK-THRESHOLD, else
   COLLECTION-FN (lambda (list)).  Returns ONE-FN's result for a single
   object, BULK-FN's BULK-RESULT for bulk, else OBJECTS."
  (cond ((typep objects 'sobject)
         (%resolve-type objects conn)
         (funcall one-fn objects))
        ;; a string is a sequence, but never a sequence of SOBJECTs
        ((or (stringp objects) (not (typep objects 'sequence)))
         (%resolve-type objects conn))        ; signals the SF-API-ERROR
        (t (let ((list (coerce objects 'list)))
             (%require-one-type list conn)
             (cond ((null list) objects)
                   ((and bulk-threshold (> (length list) bulk-threshold))
                    (funcall bulk-fn list))
                   (t (funcall collection-fn list)
                      objects))))))

(defun database-insert (objects &key (conn *connection*)
                                  (bulk-threshold cl-sf/bulk:*bulk-threshold*))
  "Insert OBJECTS (one SOBJECT or a sequence of them, all one type) into
   Salesforce.  Each object's SOBJECT-ID is set from the response.

   A sequence of at most BULK-THRESHOLD (default *BULK-THRESHOLD*, 200) goes
   through the sObject Collections API — 200 records per request, several
   requests in parallel — and OBJECTS is returned.  A longer sequence is
   sent through Bulk 2.0 and a BULK-RESULT is returned.  BULK-THRESHOLD 0
   always uses Bulk; NIL never does.

   If any record is rejected, every other record is still processed, then
   SF-DML-ERROR is signalled; its SF-DML-ERROR-FAILURES lists each failed
   object with the reason.

   Example:
     (database-insert (make-instance 'account :name \"Acme\"))
     (database-insert accounts)"
  (%dml objects conn bulk-threshold
        (lambda (o) (%insert-one o conn))
        (lambda (list) (%collection-dml :insert list conn))
        (lambda (list) (cl-sf/bulk:bulk-insert list :conn conn))))

(defun database-update (objects &key (conn *connection*)
                                  (bulk-threshold cl-sf/bulk:*bulk-threshold*))
  "Update OBJECTS (one SOBJECT or a sequence, all one type), each of which
   must have an SOBJECT-ID.  Only non-NIL slots are sent.  Routing, return
   value and failures as for DATABASE-INSERT.

   Example:
     (setf (account-phone acct) \"555-9999\")
     (database-update acct)"
  (%dml objects conn bulk-threshold
        (lambda (o) (%update-one o conn))
        (lambda (list) (%collection-dml :update list conn))
        (lambda (list) (cl-sf/bulk:bulk-update list :conn conn))))

(defun database-upsert (objects external-id-field
                        &key (conn *connection*)
                          (bulk-threshold cl-sf/bulk:*bulk-threshold*))
  "Upsert OBJECTS (one SOBJECT or a sequence, all one type), matching on
   EXTERNAL-ID-FIELD (the API field name string, e.g. \"External_Id__c\").
   Routing, return value and failures as for DATABASE-INSERT.

   Example:
     (database-upsert acct \"My_External_Id__c\")"
  (%dml objects conn bulk-threshold
        (lambda (o) (%upsert-one o external-id-field conn))
        (lambda (list) (%collection-dml :upsert list conn :external-id-field external-id-field))
        (lambda (list) (cl-sf/bulk:bulk-upsert list external-id-field :conn conn))))

(defun database-delete (objects &key (conn *connection*)
                                  (bulk-threshold cl-sf/bulk:*bulk-threshold*))
  "Delete OBJECTS (one SOBJECT or a sequence, all one type), each of which
   must have an SOBJECT-ID.  Routing and failures as for DATABASE-INSERT;
   returns T unless the bulk path is used, which returns a BULK-RESULT.

   Example:
     (database-delete acct)
     (database-delete accounts)"
  (let ((result (%dml objects conn bulk-threshold
                      (lambda (o) (%delete-one o conn))
                      (lambda (list) (%collection-dml :delete list conn))
                      (lambda (list) (cl-sf/bulk:bulk-delete list :conn conn)))))
    (if (or (eq result objects) (eq result t)) t result)))

;;; ──────────────────────────────────────────────────────────────────
;;; Dynamic class generation
;;; ──────────────────────────────────────────────────────────────────

(defun load-sobject-classes (&key (conn *connection*) (types :all))
  "Generate and register CLOS classes for Salesforce sObject types at runtime.
   CONN  — authenticated CONNECTION; defaults to *CONNECTION*
   TYPES — :all, a list of API name strings, or a predicate (string → boolean)

   Calls DESCRIBE-SOBJECT for each type in parallel via LPARALLEL:PMAPCAR,
   then evaluates the generated DEFINE-SOBJECT-CLASS forms sequentially
   (CLOS class creation is not thread-safe).

   Returns the list of generated class-name symbols."
  (let* ((all-types   (let ((global (describe-global :conn conn)))
                        (coerce (gethash "sobjects" global) 'list)))
         (type-names  (mapcar (lambda (t-hash) (gethash "name" t-hash))
                              all-types))
         (target-names
          (etypecase types
            ((eql :all)  type-names)
            (list        (intersection type-names types :test #'equal))
            (function    (remove-if-not types type-names)))))
    ;; Fetch all describe results in parallel
    (cl-sf/auth::%ensure-lparallel-kernel)
    (let ((describes
           (lparallel:pmapcar
            (lambda (name)
              (handler-case (describe-sobject name :conn conn)
                (error (e)
                  (warn "cl-sf: describe-sobject failed for ~A: ~A" name e)
                  nil)))
            target-names)))
      ;; Generate and eval class definitions sequentially (CLOS class
      ;; creation is not thread-safe — see %EVAL-DEFINE-SOBJECT-CLASS,
      ;; shared with the lazy on-demand path in %REGISTER-SOBJECT-CLASS).
      (loop for name in target-names
            for desc in describes
            when desc
            collect (%eval-define-sobject-class name desc)))))

(defun load-sobject-classes/openapi (&key (conn *connection*) (types :all))
  "Like LOAD-SOBJECT-CLASSES but derives class definitions from the
   Salesforce OpenAPI 3.0 spec endpoint (Beta) rather than describe calls.

   NOTE: The OpenAPI endpoint is Salesforce Beta and subject to change.
   This function automatically falls back to LOAD-SOBJECT-CLASSES on any error.

   Requires: Setup → User Interface → Enable Salesforce Platform REST API
             OpenAPI Spec Generation (Beta)."
  (handler-case
      ;; TODO: implement OpenAPI spec fetching and parsing
      ;; 1. POST /openapi/0.1/jobs with desired sObject selectors
      ;; 2. Poll the returned job URL until state = Complete
      ;; 3. GET the spec document
      ;; 4. Parse with jzon and call %openapi-spec->class-defs
      ;; 5. Eval + register
      (progn
        (warn "cl-sf: load-sobject-classes/openapi not yet implemented; ~
               falling back to load-sobject-classes.")
        (load-sobject-classes :conn conn :types types))
    (error (e)
      (warn "cl-sf: OpenAPI spec generation failed (~A); ~
             falling back to load-sobject-classes." e)
      (load-sobject-classes :conn conn :types types))))
