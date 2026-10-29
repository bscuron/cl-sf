;;;; cl-sf/tests/model-tests.lisp
;;;; Timestamps, related records and accessors on generated classes, with a
;;;; stubbed org: describe metadata and HTTP are faked; nothing touches the network.

(in-package #:cl-sf/tests)

;;; ── Fixture org ───────────────────────────────────────────────────

(defun org-describe (name &key fields parents children)
  "A describe result: FIELDS plain field names; PARENTS (field relationship
   . referenceTo...) lookups; CHILDREN (relationship . child-sObject)."
  (let ((d (make-hash-table :test #'equal)))
    (flet ((field (spec &optional rel refs)
             (let ((f (make-hash-table :test #'equal))
                   (n (if (consp spec) (car spec) spec)))
               (setf (gethash "name" f) n
                     (gethash "type" f) (if (consp spec) (cdr spec) "string")
                     (gethash "relationshipName" f) (or rel 'null)
                     (gethash "referenceTo" f) (coerce refs 'vector))
               f))
           (child (rel sobject)
             (let ((c (make-hash-table :test #'equal)))
               (setf (gethash "relationshipName" c) rel
                     (gethash "childSObject" c) sobject)
               c)))
      (setf (gethash "name" d) name
            (gethash "fields" d)
            (coerce (append (mapcar #'field fields)
                            (mapcar (lambda (p) (field (first p) (second p) (cddr p))) parents))
                    'vector)
            (gethash "childRelationships" d)
            (coerce (mapcar (lambda (c) (child (car c) (cdr c))) children) 'vector)))
    d))

(defparameter *test-org*
  (list (org-describe "Account"
                      :fields '("Id" "Name" "Industry" "AnnualRevenue" "My_Field__c" "Active__c"
                                ("CreatedDate" . "datetime"))
                      :parents '(("OwnerId" "Owner" "User") ("ParentId" "Parent" "Account"))
                      :children '(("Contacts" . "Contact") ("Opportunities" . "Opportunity")))
        (org-describe "Contact"
                      :fields '("Id" "LastName" "Email")
                      :parents '(("AccountId" "Account" "Account")))
        (org-describe "Opportunity" :fields '("Id" "Name" ("CloseDate" . "date") ("Amount" . "currency")))
        (org-describe "User" :fields '("Id" "Name" "Email"))
        (org-describe "Group" :fields '("Id" "Name"))
        (org-describe "Lead" :fields '("Id" "Company")
                             :parents '(("OwnerId" "Owner" "Group" "User")))
        (org-describe "ModelThing__c" :fields '("Id" "Name" "Type" "Score__c" "CreatedById"
                                              ("Seen__c" . "datetime") ("Born__c" . "date")
                                              ("CreatedDate" . "datetime"))
                                    :parents '(("OwnerId" "Owner" "User"))
                                    :children '(("Notes__r" . "ModelNote__c")))
        (org-describe "ModelNote__c" :fields '("Id" "Name"))))

(defun json (string) (com.inuoe.jzon:parse string))

(defmacro with-test-org ((requests-var &key responses (describes (gensym "D"))) &body body)
  "Run BODY on a fresh connection to a stubbed org: DESCRIBE-SOBJECT answers
   from *TEST-ORG* (counting calls in DESCRIBES), API-REQUEST records each
   request body onto REQUESTS-VAR and answers from RESPONSES (JSON
   strings, in order).  Class registries are isolated."
  `(let* ((,requests-var '())
          (,describes 0)
          (answers (mapcar #'json ,responses))
          (cl-sf/auth:*connection*
            (cl-sf/auth:make-connection
             :instance-url (format nil "https://~A.example.com" (random most-positive-fixnum))))
          (cl-sf/objects:*sobject-classes* (make-hash-table :test #'equal))
          (cl-sf/objects:*sobject-slot-maps* (make-hash-table :test #'eq)))
     (declare (ignorable ,describes))
     (with-fdefinitions
         ((cl-sf/objects:describe-sobject
           (lambda (type &key conn)
             (declare (ignore conn))
             (incf ,describes)
             (or (find type *test-org* :key (lambda (d) (gethash "name" d)) :test #'string-equal)
                 (error 'cl-sf/api:sf-api-error :message "NOT_FOUND"))))
          (cl-sf/api:api-request
           (lambda (method path &key conn body params content-type raw)
             (declare (ignore conn params content-type raw))
             (push (list method path (gethash "query" body)) ,requests-var)
             (or (pop answers) (error "unexpected request")))))
       ,@body)))

;;; ── LOCAL-TIME timestamps in SELECT conditions ────────────────────

(define-test temporal
  :parent cl-sf/tests
  :description "LOCAL-TIME values for date/datetime/time fields in select.")

(defparameter *ts*
  (local-time:encode-timestamp 0 5 4 3 5 3 2024 :timezone local-time:+utc-zone+)
  "2024-03-05 03:04:05 UTC")

(defparameter *ts2*
  (local-time:encode-timestamp 0 0 0 0 6 3 2024 :timezone local-time:+utc-zone+))

(define-test temporal/select-renders-by-the-fields-type
  :parent temporal
  (with-test-org (requests)
    (let ((ts *ts*))
      ;; datetime field → datetime literal
      (is string= "SELECT ID FROM ACCOUNT WHERE CREATEDDATE > 2024-03-05T03:04:05.000Z"
          (cl-sf/objects:soql select id from account where createddate > ts))
      ;; date field → date literal
      (is string= "SELECT ID FROM OPPORTUNITY WHERE CLOSEDATE <= 2024-03-05"
          (cl-sf/objects:soql select id from opportunity where closedate <= ts))
      ;; a Lisp expression, not just a variable
      (is string= "SELECT ID FROM OPPORTUNITY WHERE CLOSEDATE = 2024-03-06"
          (cl-sf/objects:soql select id from opportunity where closedate = (identity *ts2*)))
      ;; through a relationship
      (is string= "SELECT ID FROM CONTACT WHERE ACCOUNT.CREATEDDATE >= 2024-03-05T03:04:05.000Z"
          (cl-sf/objects:soql select id from contact where account.createddate >= ts)))))

(define-test temporal/select-renders-lists-of-timestamps
  :parent temporal
  (with-test-org (requests)
    (let ((days (list *ts* *ts2*)))
      (is string= "SELECT ID FROM OPPORTUNITY WHERE CLOSEDATE IN (2024-03-05, 2024-03-06)"
          (cl-sf/objects:soql select id from opportunity where closedate in days)))))

(define-test temporal/select-falls-back-to-a-datetime-when-the-type-is-unknown
  :parent temporal
  (let ((ts *ts*) (cl-sf/auth:*connection* nil))
    ;; no connection at all (soql just builds text)
    (is string= "SELECT ID FROM ACCOUNT WHERE CREATEDDATE > 2024-03-05T03:04:05.000Z"
        (cl-sf/objects:soql select id from account where createddate > ts)))
  (with-test-org (requests)
    (let ((ts *ts*))
      ;; a field the org doesn't know, an object it doesn't know
      (is string= "SELECT ID FROM ACCOUNT WHERE NOSUCH__C > 2024-03-05T03:04:05.000Z"
          (cl-sf/objects:soql select id from account where nosuch__c > ts))
      (is string= "SELECT ID FROM NOSUCHOBJECT__C WHERE X > 2024-03-05T03:04:05.000Z"
          (cl-sf/objects:soql select id from nosuchobject__c where x > ts)))))

(define-test temporal/select-only-looks-up-metadata-for-timestamps
  :parent temporal
  (let ((describes 0))
    (with-test-org (requests :describes describes)
      (let ((name "Acme") (n 3) (ts *ts*))
        (cl-sf/objects:soql select id from account where name = name and annualrevenue > n)
        (is = 0 describes "strings and numbers never touch the schema")
        (cl-sf/objects:soql select id from account where createddate > ts)
        (cl-sf/objects:soql select id from account where createddate < ts)
        (is = 1 describes "one describe, then cached")))))

(define-test temporal/select-subquery-conditions-use-their-own-from
  :parent temporal
  (with-test-org (requests)
    (let ((ts *ts*))
      (is string= "SELECT ID FROM CONTACT WHERE ACCOUNTID IN (SELECT ID FROM OPPORTUNITY WHERE CLOSEDATE < 2024-03-05)"
          (cl-sf/objects:soql select id from contact
                              where accountid in (select id from opportunity where closedate < ts))))))

(define-test temporal/timestamp-text
  :parent temporal
  (is string= "2024-03-05" (cl-sf/objects::%timestamp-text *ts* "date"))
  (is string= "2024-03-05T03:04:05.000Z" (cl-sf/objects::%timestamp-text *ts* "datetime"))
  (is string= "03:04:05.000Z" (cl-sf/objects::%timestamp-text *ts* "time"))
  ;; always UTC: 12:04:05 at UTC+9 is the same instant as 03:04:05 UTC
  (let ((tokyo (local-time:encode-timestamp 0 5 4 12 5 3 2024 :offset 32400)))
    (is string= "2024-03-05T03:04:05.000Z" (cl-sf/objects::%timestamp-text tokyo "datetime"))))

;;; ── Dates and datetimes on returned objects are LOCAL-TIME timestamps ──

(defun ts (&rest args)
  "A UTC timestamp: (ts year month day [hour minute second [nsec]])."
  (destructuring-bind (year month day &optional (hour 0) (minute 0) (second 0) (nsec 0)) args
    (local-time:encode-timestamp nsec second minute hour day month year :offset 0)))

(define-test temporal/parses-what-salesforce-sends
  :parent temporal
  (flet ((parse (string &optional (kind "datetime")) (cl-sf/objects::%parse-temporal string kind)))
    ;; REST: +0000, also Z, fractions
    (is local-time:timestamp= (ts 2024 3 5 3 4 5) (parse "2024-03-05T03:04:05.000+0000"))
    (is local-time:timestamp= (ts 2024 3 5 3 4 5) (parse "2024-03-05T03:04:05.000Z"))
    (is local-time:timestamp= (ts 2024 3 5 3 4 5) (parse "2024-03-05T03:04:05Z"))
    (is local-time:timestamp= (ts 2024 3 5 3 4 5 123000000) (parse "2024-03-05T03:04:05.123Z"))
    (is local-time:timestamp= (ts 2024 3 5 3 4 5 123456789) (parse "2024-03-05T03:04:05.123456789Z"))
    ;; offsets are applied: 12:04:05 at UTC+9 is 03:04:05 UTC
    (is local-time:timestamp= (ts 2024 3 5 3 4 5) (parse "2024-03-05T12:04:05+09:00"))
    (is local-time:timestamp= (ts 2024 3 5 3 4 5) (parse "2024-03-05T12:04:05+0900"))
    (is local-time:timestamp= (ts 2024 3 5 8 34 5) (parse "2024-03-05T03:04:05-05:30"))
    ;; dates are midnight UTC
    (is local-time:timestamp= (ts 2024 3 5) (parse "2024-03-05" "date"))
    (is local-time:timestamp= (ts 2024 3 5) (parse "2024-03-05"))
    (is local-time:timestamp= (ts 2024 3 5) (parse "2024-03-05T03:04:05.000Z" "date")
        "a date field ignores a time part")
    ;; things that aren't dates are not parsed
    (false (parse "hello"))
    (false (parse ""))
    (false (parse "2024-13-05"))
    (false (parse "2024-03-05T25:00:00Z"))
    (false (parse "2024-03-05T03:04:05.Z"))
    (false (parse "2024-03-05T03:04:05Zjunk"))
    (false (parse "03:04:05.000Z"))))

(define-test temporal/text-and-parse-round-trip
  :parent temporal
  (dolist (string '("2024-03-05T03:04:05.000Z" "2024-03-05T03:04:05.123Z" "1999-12-31T23:59:59.999Z"))
    (is string= string
        (cl-sf/objects::%timestamp-text (cl-sf/objects::%parse-temporal string "datetime") "datetime")))
  (is string= "2024-03-05"
      (cl-sf/objects::%timestamp-text (cl-sf/objects::%parse-temporal "2024-03-05" "date") "date")))

(cl-sf/objects:define-sobject-class temporal-probe "TemporalProbe__c"
  (name    "Name")
  (seen    "Seen__c"        :sf-type "datetime")
  (born    "Born__c"        :sf-type "date")
  (created "CreatedDate"    :sf-type "datetime"))

(defun probe-hash (&rest pairs)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on pairs by #'cddr do (setf (gethash k h) v))
    h))

(define-test temporal/records-hold-timestamps-in-date-and-datetime-slots
  :parent temporal
  (let ((probe (cl-sf/objects:hash->sobject
                (probe-hash "Name" "n" "Seen__c" "2024-03-05T03:04:05.000+0000"
                            "Born__c" "1990-02-03" "CreatedDate" "2020-01-02T00:00:00.000Z")
                "TemporalProbe__c")))
    (is string= "n" (slot-value probe 'name))
    (is local-time:timestamp= (ts 2024 3 5 3 4 5) (slot-value probe 'seen))
    (is local-time:timestamp= (ts 1990 2 3) (slot-value probe 'born))
    (is local-time:timestamp= (ts 2020 1 2) (slot-value probe 'created))
    ;; the raw JSON is untouched
    (is string= "2024-03-05T03:04:05.000+0000"
        (gethash "Seen__c" (cl-sf/objects:sobject-raw probe)))))

(define-test temporal/unparseable-text-is-kept-and-null-is-empty
  :parent temporal
  (let ((probe (cl-sf/objects:hash->sobject
                (probe-hash "Name" 'null "Seen__c" "not a date" "Born__c" 'null)
                "TemporalProbe__c")))
    (is string= "not a date" (slot-value probe 'seen))
    (false (slot-value probe 'name) "a JSON null is an empty slot, not the symbol NULL")
    (false (slot-value probe 'born))))

(define-test temporal/timestamps-are-written-back-as-salesforce-text
  :parent temporal
  (let* ((probe (make-instance 'temporal-probe :name "n"
                                               :seen (ts 2024 3 5 3 4 5 123000000)
                                               :born (ts 1990 2 3)
                                               :created (ts 2020 1 2)))
         (hash  (cl-sf/objects:sobject->hash-table probe)))
    (is string= "2024-03-05T03:04:05.123Z" (gethash "Seen__c" hash))
    (is string= "1990-02-03" (gethash "Born__c" hash))
    (false (nth-value 1 (gethash "CreatedDate" hash)) "read-only: never sent"))
  ;; text a caller put in a slot themselves goes out untouched
  (let ((hash (cl-sf/objects:sobject->hash-table
               (make-instance 'temporal-probe :seen "2024-03-05T00:00:00Z"))))
    (is string= "2024-03-05T00:00:00Z" (gethash "Seen__c" hash))))

(define-test temporal/csv-writes-dates-and-datetimes-by-slot-kind
  :parent temporal
  (let ((probe (make-instance 'temporal-probe :name "n"
                                              :seen (ts 2024 3 5 3 4 5)
                                              :born (ts 1990 2 3))))
    (is string= (format nil "Name,Seen__c,Born__c~%n,2024-03-05T03:04:05.000Z,1990-02-03~%")
        (csv-text (cl-sf/bulk::%sobjects->csv (list probe) '("Name" "Seen__c" "Born__c"))))))

(define-test temporal/redefining-a-class-without-temporal-fields-forgets-them
  :parent temporal
  (cl-sf/objects:define-sobject-class kinds-probe "KindsProbe__c"
    (seen "Seen__c" :sf-type "datetime"))
  (true (gethash 'kinds-probe cl-sf/objects::*sobject-field-kinds*))
  (cl-sf/objects:define-sobject-class kinds-probe "KindsProbe__c"
    (seen "Seen__c"))
  (false (gethash 'kinds-probe cl-sf/objects::*sobject-field-kinds*)))

(define-test temporal/generated-classes-get-timestamp-slots-including-read-only-dates
  :parent temporal
  (with-test-org (requests)
    (let* ((desc (find "ModelThing__c" *test-org* :key (lambda (d) (gethash "name" d)) :test #'string=))
           (class-name (cl-sf/objects::%eval-define-sobject-class "ModelThing__c" desc))
           (slots (gethash class-name cl-sf/objects:*sobject-slot-maps*))
           (kinds (gethash class-name cl-sf/objects::*sobject-field-kinds*)))
      (true (assoc "Seen__c" slots :test #'equal))
      (true (assoc "CreatedDate" slots :test #'equal) "a read-only datetime is readable")
      (true (assoc "CreatedById" slots :test #'equal) "so is any other read-only field")
      (false (assoc "Id" slots :test #'equal) "Id lives in SOBJECT's own slot")
      (is string= "datetime" (gethash "CreatedDate" kinds))
      (is string= "date" (gethash "Born__c" kinds))
      (false (gethash "Name" kinds)))))

(define-test temporal/select-style-records-have-timestamps
  :parent temporal
  ;; the REST /query shape: JSON strings with +0000, parsed by HASH->SOBJECT
  (with-test-org (requests)
    (let* ((desc (find "ModelThing__c" *test-org* :key (lambda (d) (gethash "name" d)) :test #'string=)))
      (cl-sf/objects::%eval-define-sobject-class "ModelThing__c" desc)
      (let* ((attrs (probe-hash "type" "ModelThing__c"))
             (record (cl-sf/objects:hash->sobject
                      (probe-hash "attributes" attrs "Id" "a01" "Seen__c" "2024-03-05T03:04:05.000+0000"
                                  "CreatedDate" "2020-01-02T00:00:00.000+0000" "Born__c" 'null))))
        (let ((slots (gethash (class-name (class-of record)) cl-sf/objects:*sobject-slot-maps*)))
          (flet ((slot (field) (slot-value record (cdr (assoc field slots :test #'equal)))))
            (is local-time:timestamp= (ts 2024 3 5 3 4 5) (slot "Seen__c"))
            (is local-time:timestamp= (ts 2020 1 2) (slot "CreatedDate"))
            (false (slot "Born__c"))))))))

(define-test temporal/fast-timestamp-text-matches-local-times-formatter
  :parent temporal
  (let ((state (sb-ext:seed-random-state 7)) (mismatches '()))
    (flet ((check (timestamp)
             (dolist (kind '("date" "datetime" "time"))
               (let ((fast (cl-sf/objects::%timestamp-text timestamp kind))
                     (slow (cl-sf/objects::%timestamp-text-slow timestamp kind)))
                 (unless (string= fast slow) (push (list kind fast slow) mismatches))))))
      (dotimes (i 3000)                     ; instants from 1850 to 2400
        (check (local-time:make-timestamp :day (+ -54000 (random 220000 state))
                                          :sec (random 86400 state)
                                          :nsec (random 1000000000 state))))
      (loop for (year month day) in '((2000 2 29) (2000 3 1) (1999 12 31) (2100 2 28) (2100 3 1)
                                      (1900 2 28) (1900 3 1) (2024 2 29) (1970 1 1) (1 1 1) (9999 12 31))
            do (check (local-time:encode-timestamp 999999999 59 59 23 day month year :offset 0))
               (check (local-time:encode-timestamp 0 0 0 0 day month year :offset 0))))
    (is equal '() mismatches)))

;;; ── A field called Type must not collide with the sObject's own type ──

(define-test sobject/a-field-called-type-is-not-filled-with-the-object-name
  :parent temporal
  (with-test-org (requests)
    (let* ((desc (find "ModelThing__c" *test-org* :key (lambda (d) (gethash "name" d)) :test #'string=))
           (class-name (cl-sf/objects::%eval-define-sobject-class "ModelThing__c" desc))
           (slots (gethash class-name cl-sf/objects:*sobject-slot-maps*))
           (type-slot (cdr (assoc "Type" slots :test #'equal)))
           (thing (make-instance class-name)))
      (true type-slot "the Type field has a slot")
      (is string= "ModelThing__c" (cl-sf/objects:sobject-type thing) "the sObject type is still set")
      (false (slot-value thing type-slot) "...and the Type field is empty, not \"ModelThing__c\"")
      (false (nth-value 1 (gethash "Type" (cl-sf/objects:sobject->hash-table thing)))
             "an empty Type is not written back")
      ;; the field has its own initarg
      (let ((typed (make-instance class-name :type-field "Customer")))
        (is string= "Customer" (slot-value typed type-slot))
        (is string= "ModelThing__c" (cl-sf/objects:sobject-type typed))
        (is string= "Customer" (gethash "Type" (cl-sf/objects:sobject->hash-table typed))))
      ;; a fetched record whose Type is null keeps it empty
      (let ((fetched (cl-sf/objects:hash->sobject
                      (probe-hash "Id" "a01" "Name" "n" "Type" 'null) "ModelThing__c")))
        (false (slot-value fetched type-slot))
        (false (nth-value 1 (gethash "Type" (cl-sf/objects:sobject->hash-table fetched))))))))

(define-test sobject/define-sobject-class-renames-initargs-that-collide
  :parent temporal
  (cl-sf/objects:define-sobject-class collide-probe "CollideProbe__c"
    (type "Type")
    (name "Name"))
  (let ((o (make-instance 'collide-probe :name "n")))
    (is string= "CollideProbe__c" (cl-sf/objects:sobject-type o))
    (false (slot-value o 'type))
    (setf (slot-value o 'type) "x")
    (is string= "x" (collide-probe-type o))))

;;; ── Related records are CLOS objects ──────────────────────────────

(define-test related
  :parent cl-sf/tests
  :description "Parent records and child lists as CLOS objects, with a reader per relationship.")

(defun generated-class (name)
  "Generate (in the isolated registries of WITH-TEST-ORG) the class for the fixture object NAME."
  (cl-sf/objects::%eval-define-sobject-class
   name (find name *test-org* :key (lambda (d) (gethash "name" d)) :test #'string=)))

(defun slot-by-field (object field)
  (slot-value object (cdr (assoc field (gethash (class-name (class-of object)) cl-sf/objects:*sobject-slot-maps*)
                                 :test #'equal))))

(defparameter *soql-shaped-thing*
  "{\"attributes\":{\"type\":\"ModelThing__c\"},\"Id\":\"a01\",\"Name\":\"T\",
    \"Owner\":{\"attributes\":{\"type\":\"User\"},\"Id\":\"005\",\"Name\":\"Ada\"},
    \"Notes__r\":{\"totalSize\":2,\"done\":true,\"records\":[
        {\"attributes\":{\"type\":\"ModelNote__c\"},\"Name\":\"first\"},
        {\"attributes\":{\"type\":\"ModelNote__c\"},\"Name\":\"second\"}]},
    \"Parent\":null,
    \"BillingAddress\":{\"city\":\"Oslo\"}}")

(define-test related/parents-and-child-lists-are-clos-objects
  :parent related
  (with-test-org (requests)
    (generated-class "User") (generated-class "ModelNote__c") (generated-class "ModelThing__c")
    (let* ((thing (cl-sf/objects:hash->sobject (json *soql-shaped-thing*)))
           (owner (cl-sf/objects:sobject-related thing "Owner"))
           (notes (cl-sf/objects:sobject-related thing "Notes__r")))
      ;; the parent: one typed object with its own fields as slots
      (true (typep owner (gethash "User" cl-sf/objects:*sobject-classes*)))
      (is string= "User" (cl-sf/objects:sobject-type owner))
      (is string= "005" (cl-sf/objects:sobject-id owner))
      (is string= "Ada" (slot-by-field owner "Name"))
      ;; the child list: a vector of typed objects
      (true (simple-vector-p notes))
      (is = 2 (length notes))
      (true (every (lambda (n) (typep n (gethash "ModelNote__c" cl-sf/objects:*sobject-classes*))) notes))
      (is equal '("first" "second") (map 'list (lambda (n) (slot-by-field n "Name")) notes))
      ;; null, absent and compound values are not relationships
      (false (cl-sf/objects:sobject-related thing "Parent"))
      (false (cl-sf/objects:sobject-related thing "Nowhere"))
      (false (cl-sf/objects:sobject-related thing "BillingAddress"))
      (is equal '("Owner" "Notes__r") (mapcar #'car (cl-sf/objects:sobject-related thing)))
      ;; lookup ignores case; the raw JSON is untouched
      (is eq owner (cl-sf/objects:sobject-related thing "owner"))
      (true (hash-table-p (gethash "Owner" (cl-sf/objects:sobject-raw thing)))))))

(define-test related/each-relationship-has-a-reader-named-after-it
  :parent related
  (with-test-org (requests)
    (generated-class "User") (generated-class "ModelNote__c")
    (let* ((class-name (generated-class "ModelThing__c"))
           (thing (cl-sf/objects:hash->sobject (json *soql-shaped-thing*)))
           (owner-reader (find-symbol "MODELTHING__C-OWNER" (symbol-package class-name)))
           (notes-reader (find-symbol "MODELTHING__C-NOTES__R" (symbol-package class-name))))
      (true (and owner-reader (fboundp owner-reader)))
      (is string= "Ada" (slot-by-field (funcall owner-reader thing) "Name"))
      (is = 2 (length (funcall notes-reader thing)))
      ;; and a reader never replaces a field's own accessor
      (let ((name-accessor (find-symbol "MODELTHING__C-NAME" (symbol-package class-name))))
        (is string= "T" (funcall name-accessor thing))))))

(define-test related/a-nested-record-of-an-unknown-class-is-a-generic-object
  :parent related
  (with-test-org (requests)
    (generated-class "ModelThing__c")                ; the User and ModelNote__c classes are not registered
    (let* ((thing (cl-sf/objects:hash->sobject (json *soql-shaped-thing*)))
           (owner (cl-sf/objects:sobject-related thing "Owner")))
      (is eq 'cl-sf/objects:sobject (class-name (class-of owner)))
      (is string= "User" (cl-sf/objects:sobject-type owner))
      (is string= "Ada" (gethash "Name" (cl-sf/objects:sobject-raw owner))))))

(define-test related/nested-records-nest
  :parent related
  (with-test-org (requests)
    (let* ((record (json "{\"attributes\":{\"type\":\"Contact\"},\"LastName\":\"G\",
                           \"Account\":{\"attributes\":{\"type\":\"Account\"},\"Name\":\"Acme\",
                                        \"Owner\":{\"attributes\":{\"type\":\"User\"},\"Name\":\"Ada\"}}}"))
           (contact (cl-sf/objects:hash->sobject record))
           (account (cl-sf/objects:sobject-related contact "Account"))
           (owner   (cl-sf/objects:sobject-related account "Owner")))
      (is string= "Acme" (gethash "Name" (cl-sf/objects:sobject-raw account)))
      (is string= "Ada" (gethash "Name" (cl-sf/objects:sobject-raw owner))))))

(define-test related/printing-shows-the-related-objects
  :parent related
  (with-test-org (requests)
    (generated-class "User") (generated-class "ModelNote__c") (generated-class "ModelThing__c")
    (let ((text (princ-to-string (cl-sf/objects:hash->sobject (json *soql-shaped-thing*)))))
      (true (search "Owner=User(" text))
      (true (search "Name=Ada" text))
      (true (search "Notes__r=[ModelNote__c(" text))
      (false (search "NULL" text) "a JSON null prints as NIL, not NULL")
      (true (search "Parent=NIL" text)))))

(define-test related/the-query-in-the-report-is-correct-a-user-with-no-contact-is-just-nil
  :parent related
  (with-test-org (requests)
    (generated-class "ModelThing__c")
    (let ((user (cl-sf/objects:hash->sobject
                 (json "{\"attributes\":{\"type\":\"ModelThing__c\"},\"Name\":\"u\",\"Contact\":null}"))))
      (false (cl-sf/objects:sobject-related user "Contact"))
      (is equal "ModelThing__c(Name=u, Contact=NIL)" (princ-to-string user)))))

;;; ── Bulk: the two bugs the live run found ─────────────────────────

(define-test bulk-column-names/upsert-on-id-includes-the-id-column
  :parent bulk
  ;; Id is never in a generated slot map, so upserting on Id sent no Id at all
  (let ((cols (cl-sf/bulk::%bulk-column-names (list (make-widget :label "X")) :upsert "Id")))
    (is string= "Id" (first cols))
    (true (member "Label__c" cols :test #'equal)))
  ;; an external id that is an ordinary field is still just one column
  (let ((cols (cl-sf/bulk::%bulk-column-names (list (make-widget :label "X")) :upsert "Label__c")))
    (is = 1 (count "Label__c" cols :test #'equal))))

(define-test sobjects->csv/upsert-on-id-writes-the-ids
  :parent bulk
  (let ((existing (make-widget :label "A"))
        (fresh    (make-widget :label "B")))
    (setf (cl-sf/objects:sobject-id existing) "001EXISTING")
    (let ((columns (cl-sf/bulk::%bulk-column-names (list existing fresh) :upsert "Id")))
      (is string= (format nil "Id,Label__c,Amount__c,Active__c~%001EXISTING,A,,~%,B,,~%")
          (csv-text (cl-sf/bulk::%sobjects->csv (list existing fresh) columns))))))

(define-test build-bulk-result/a-failed-row-with-a-blank-result-id-is-matched-by-the-sent-id
  :parent bulk
  (let ((ghost (make-instance 'cl-sf/objects:sobject :id "006000000000000AAA" :type "Opportunity"))
        (real  (make-instance 'cl-sf/objects:sobject :id "006REAL" :type "Opportunity")))
    (with-stubbed-results
        (:success (format nil "\"sf__Id\",\"sf__Created\",\"Id\"~%\"006REAL\",\"false\",\"006REAL\"~%")
         :failed (format nil "\"sf__Id\",\"sf__Error\",\"Id\",\"Name\"~%\"\",\"INVALID_CROSS_REFERENCE_KEY:invalid cross reference id:--\",\"006000000000000AAA\",\"ghost\"~%")
         :unprocessed "")
      (let ((result (cl-sf/bulk::%build-bulk-result nil "J" (list real ghost) :update "Opportunity"
                                                    :status (job-status "JobComplete" :processed 2 :failed 1))))
        (is eq real (first (cl-sf/bulk:bulk-result-successful result)))
        (is = 1 (length (cl-sf/bulk:bulk-result-failed result)))
        (is eq ghost (car (first (cl-sf/bulk:bulk-result-failed result))))
        (true (search "INVALID_CROSS_REFERENCE_KEY" (cdr (first (cl-sf/bulk:bulk-result-failed result)))))))))

;;; ── Every field, read-only ones too, has a <class>-<field> accessor ──

(define-test accessors
  :parent cl-sf/tests
  :description "<class>-<field> readers for every field, including the Id and read-only fields.")

(defun reader (class-name suffix)
  (symbol-function (find-symbol (format nil "~A-~A" class-name suffix) (symbol-package class-name))))

(define-test accessors/read-only-fields-have-class-specific-readers
  :parent accessors
  (with-test-org (requests)
    (let* ((class-name (generated-class "ModelThing__c"))
           (thing (cl-sf/objects:hash->sobject
                   (json "{\"attributes\":{\"type\":\"ModelThing__c\"},\"Id\":\"a01xx\",\"Name\":\"T\",
                          \"CreatedById\":\"005xx\",\"CreatedDate\":\"2024-03-05T03:04:05.000+0000\"}"))))
      (is string= "a01xx" (funcall (reader class-name "ID") thing) "<class>-id")
      (is string= "T" (funcall (reader class-name "NAME") thing))
      (is string= "005xx" (funcall (reader class-name "CREATEDBYID") thing) "a read-only field")
      (is local-time:timestamp= (ts 2024 3 5 3 4 5) (funcall (reader class-name "CREATEDDATE") thing)
          "a read-only datetime, as a timestamp"))))

(define-test accessors/the-id-reader-is-the-base-slot-not-a-copy
  :parent accessors
  (with-test-org (requests)
    (let* ((class-name (generated-class "ModelThing__c"))
           (thing (make-instance class-name)))
      (false (funcall (reader class-name "ID") thing))
      (setf (cl-sf/objects:sobject-id thing) "a01first")
      (is string= "a01first" (funcall (reader class-name "ID") thing))
      (funcall (fdefinition (list 'setf (find-symbol (format nil "~A-ID" class-name) (symbol-package class-name))))
               "a01second" thing)
      (is string= "a01second" (cl-sf/objects:sobject-id thing) "setting <class>-id sets SOBJECT-ID")
      (false (assoc "Id" (gethash class-name cl-sf/objects:*sobject-slot-maps*) :test #'equal)))))

(define-test accessors/hand-written-classes-get-an-id-reader-too
  :parent accessors
  (let ((contact (make-instance 'test-contact :id "003xx")))
    (is string= "003xx" (test-contact-id contact))))

(define-test accessors/read-only-fields-are-never-written
  :parent accessors
  (with-test-org (requests)
    (let* ((class-name (generated-class "ModelThing__c"))
           (thing (make-instance class-name :name "T")))
      (setf (slot-value thing (cdr (assoc "CreatedById" (gethash class-name cl-sf/objects:*sobject-slot-maps*)
                                          :test #'equal)))
            "005xx")
      (setf (slot-value thing (cdr (assoc "CreatedDate" (gethash class-name cl-sf/objects:*sobject-slot-maps*)
                                          :test #'equal)))
            (ts 2024 1 1))
      ;; single save, Collections record, and Bulk CSV all leave them out
      (let ((body (cl-sf/objects:sobject->hash-table thing)))
        (is string= "T" (gethash "Name" body))
        (false (nth-value 1 (gethash "CreatedById" body)))
        (false (nth-value 1 (gethash "CreatedDate" body))))
      (let ((record (cl-sf/objects::%collection-record thing "ModelThing__c")))
        (false (nth-value 1 (gethash "CreatedById" record))))
      (false (member "CreatedById" (cl-sf/bulk::%bulk-column-names (list thing) :insert) :test #'equal))
      (false (member "CreatedDate" (cl-sf/bulk::%bulk-column-names (list thing) :update) :test #'equal)))))
