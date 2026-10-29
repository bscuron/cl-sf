;;;; cl-sf/src/bulk.lisp
;;;; Salesforce Bulk 2.0 API — large-volume DML operations.
;;;;
;;;; Design:
;;;;   CLOS-first: bulk-insert/update/upsert/delete accept lists of SOBJECT
;;;;   instances; type is inferred from the first element.  SOBJECT->HASH-TABLE
;;;;   and the slot map drive CSV generation so no field names are hard-coded.
;;;;
;;;;   Performance:
;;;;   - CSV generation is O(n * fields) with a single output stream per run.
;;;;   - For n > *BULK-CSV-CHUNK-SIZE*, rows are generated in parallel via
;;;;     LPARALLEL:PMAPCAR over chunks; fragments are joined with a single stream.
;;;;   - CSV field value lookup is O(1) via pre-computed slot-map alist.
;;;;   - %POLL-JOB runs inside a LPARALLEL:FUTURE so the calling thread is free.
;;;;   - Result correlation for update/delete is O(n) via Id → object hash-map.
;;;;
;;;; Bulk 2.0 job lifecycle:
;;;;   POST /jobs/ingest                     → create job
;;;;   PUT  /jobs/ingest/<id>/batches        → upload CSV data
;;;;   PATCH /jobs/ingest/<id>               → state=UploadComplete (begin processing)
;;;;   GET  /jobs/ingest/<id>                → poll status
;;;;   GET  /jobs/ingest/<id>/successfulResults
;;;;   GET  /jobs/ingest/<id>/failedResults
;;;;   GET  /jobs/ingest/<id>/unprocessedrecords

(in-package #:cl-sf/bulk)

;;; ──────────────────────────────────────────────────────────────────
;;; Configuration
;;; ──────────────────────────────────────────────────────────────────

(defparameter *bulk-threshold* 200
  "Record count above which DATABASE-INSERT etc. automatically route
   through Bulk 2.0 instead of the single-record REST API.
   Bind to NIL to disable auto-routing (always explicit).")

(defparameter *bulk-csv-chunk-size* 1000
  "Records per lparallel worker chunk during parallel CSV generation.
   Larger values reduce future overhead; smaller values increase parallelism.
   Tune based on field count and available cores.")

;;; ──────────────────────────────────────────────────────────────────
;;; Conditions
;;; ──────────────────────────────────────────────────────────────────

(define-condition bulk-error (error)
  ((message :initarg :message :reader bulk-error-message :initform "Bulk operation error"))
  (:report (lambda (c s)
             (format s "cl-sf bulk error: ~A" (bulk-error-message c)))))

(define-condition bulk-timeout-error (bulk-error)
  ((job-id  :initarg :job-id  :reader bulk-timeout-error-job-id)
   (timeout :initarg :timeout :reader bulk-timeout-error-timeout))
  (:report (lambda (c s)
             (format s "Bulk job ~A timed out after ~A seconds"
                     (bulk-timeout-error-job-id c)
                     (bulk-timeout-error-timeout c)))))

(define-condition bulk-job-failed-error (bulk-error)
  ((job-id :initarg :job-id :reader bulk-job-failed-error-job-id)
   (status :initarg :status :reader bulk-job-failed-error-status))
  (:report (lambda (c s)
             (format s "Bulk job ~A entered state: ~A"
                     (bulk-job-failed-error-job-id c)
                     (gethash "state" (bulk-job-failed-error-status c) "Unknown")))))

;;; ──────────────────────────────────────────────────────────────────
;;; Result struct
;;; ──────────────────────────────────────────────────────────────────

(defstruct bulk-result
  "The outcome of a completed Bulk 2.0 operation.

   SUCCESSFUL  — list of SOBJECT instances: the very objects that were sent.
                 Update/delete match by Id; insert (and upsert rows without an
                 Id) match by the columns Salesforce echoes back, and the
                 object receives its new Id.  A row that cannot be matched
                 is a fresh SOBJECT with only the Id.
   FAILED      — list of (sobject-or-nil . error-string) conses; the CAR is
                 the object that was sent (NIL only if it cannot be matched).
   UNPROCESSED — list of SOBJECT instances not processed (e.g. job aborted).
   JOB-ID      — the Salesforce Bulk 2.0 job Id string."
  (successful  nil :type list)
  (failed      nil :type list)
  (unprocessed nil :type list)
  (job-id      nil :type (or string null)))

;;; ──────────────────────────────────────────────────────────────────
;;; CSV generation — performance-critical
;;;
;;; Rows are written straight into a growable UTF-8 octet buffer (the form
;;; the HTTP body needs anyway: a Lisp string is 4 bytes per character and
;;; would be encoded again on send).  Each column's accessor is resolved
;;; once per job, not once per field; integers are written without
;;; consing; a field is quoted only if it contains , " CR or LF.
;;; ──────────────────────────────────────────────────────────────────

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defstruct (obuf (:constructor %make-obuf
                     (&optional (capacity 4096)
                      &aux (bytes (make-array capacity :element-type '(unsigned-byte 8))))))
  "A growable UTF-8 output buffer: bytes [0, FILL) are valid."
  (bytes (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (fill 0 :type fixnum))

(declaim (inline %obuf-reserve %obuf-byte))
(defun %obuf-reserve (buf n)
  "Make room for N more bytes."
  (declare (type obuf buf) (type fixnum n) (optimize (speed 3) (safety 0)))
  (let ((need (+ (obuf-fill buf) n))
        (bytes (obuf-bytes buf)))
    (declare (type fixnum need))
    (when (> need (length bytes))
      (let ((new (make-array (max need (* 2 (length bytes))) :element-type '(unsigned-byte 8))))
        (replace new bytes :end2 (obuf-fill buf))
        (setf (obuf-bytes buf) new)))))

(defun %obuf-byte (buf b)
  (declare (type obuf buf) (type (unsigned-byte 8) b) (optimize (speed 3) (safety 0)))
  (%obuf-reserve buf 1)
  (setf (aref (obuf-bytes buf) (obuf-fill buf)) b)
  (incf (obuf-fill buf)))

(defun %obuf-octets (buf)
  "A fresh exact-size octet vector holding BUF's contents."
  (subseq (obuf-bytes buf) 0 (obuf-fill buf)))

(defmacro %with-simple-string ((var string) &body body)
  "Run BODY with VAR bound to STRING (a SIMPLE-STRING) declared as its
   concrete type, so SCHAR compiles to a direct access."
  `(let ((,var ,string))
     (etypecase ,var
       ((simple-array character (*))
        (locally (declare (type (simple-array character (*)) ,var)) ,@body))
       (simple-base-string
        (locally (declare (type simple-base-string ,var)) ,@body)))))

(defun %obuf-write-text (buf s)
  "Write the string S as one CSV field in UTF-8: double-quoted, with inner
   quotes doubled, only if it contains a comma, quote, CR or LF."
  (declare (type obuf buf) (type simple-string s) (optimize (speed 3) (safety 0)))
  (%with-simple-string (s s)
    (let ((len (length s)) (quote-p nil) (wide nil))
      (declare (type fixnum len))
      (dotimes (i len)
        (let ((c (char-code (schar s i))))
          (cond ((or (= c 44) (= c 34) (= c 10) (= c 13)) (setf quote-p t))
                ((>= c 128) (setf wide t)))))
      ;; worst case per character: 4 bytes (a doubled quote is only 2)
      (%obuf-reserve buf (+ (* len (if wide 4 2)) 2))
      (let ((bytes (obuf-bytes buf)) (pos (obuf-fill buf)))
        (declare (type octets bytes) (type fixnum pos))
        (when quote-p
          (setf (aref bytes pos) 34) (incf pos))
        (dotimes (i len)
          (let ((c (char-code (schar s i))))
            (cond ((< c #x80)
                   (when (and quote-p (= c 34))
                     (setf (aref bytes pos) 34) (incf pos))
                   (setf (aref bytes pos) c) (incf pos))
                  ((< c #x800)
                   (setf (aref bytes pos)       (logior #xC0 (ash c -6))
                         (aref bytes (+ pos 1)) (logior #x80 (logand c #x3F)))
                   (incf pos 2))
                  ((< c #x10000)
                   (setf (aref bytes pos)       (logior #xE0 (ash c -12))
                         (aref bytes (+ pos 1)) (logior #x80 (logand (ash c -6) #x3F))
                         (aref bytes (+ pos 2)) (logior #x80 (logand c #x3F)))
                   (incf pos 3))
                  (t
                   (setf (aref bytes pos)       (logior #xF0 (ash c -18))
                         (aref bytes (+ pos 1)) (logior #x80 (logand (ash c -12) #x3F))
                         (aref bytes (+ pos 2)) (logior #x80 (logand (ash c -6) #x3F))
                         (aref bytes (+ pos 3)) (logior #x80 (logand c #x3F)))
                   (incf pos 4)))))
        (when quote-p
          (setf (aref bytes pos) 34) (incf pos))
        (setf (obuf-fill buf) pos)))))

(defun %obuf-write-integer (buf n)
  "Write the non-negative fixnum N in decimal, without consing."
  (declare (type obuf buf) (type (and fixnum unsigned-byte) n) (optimize (speed 3) (safety 0)))
  (%obuf-reserve buf 20)
  (let ((bytes (obuf-bytes buf)) (pos (obuf-fill buf)))
    (declare (type octets bytes) (type fixnum pos))
    (if (< n 10)
        (progn (setf (aref bytes pos) (+ 48 n)) (incf pos))
        (let ((digits 0) (m n))
          (declare (type fixnum digits m))
          (loop while (> m 0) do (setf m (floor m 10)) (incf digits))
          (let ((end (+ pos digits)) (p (+ pos digits)))
            (declare (type fixnum end p))
            (loop while (> n 0)
                  do (multiple-value-bind (q r) (floor n 10)
                       (decf p)
                       (setf (aref bytes p) (+ 48 r) n q)))
            (setf pos end))))
    (setf (obuf-fill buf) pos)))

(defun %field-text (value)
  "VALUE (anything but NIL, a string or a small non-negative integer) as
   the text Salesforce expects in a CSV cell."
  (typecase value
    (double-float (let ((*read-default-float-format* 'double-float)) (prin1-to-string value)))
    (single-float (let ((*read-default-float-format* 'single-float)) (prin1-to-string value)))
    ((eql t)      "true")
    (symbol       (symbol-name value))
    (local-time:timestamp (local-time:format-timestring nil value))
    (t            (princ-to-string value))))

(defun %write-field (buf value)
  "Write VALUE as one CSV cell (NIL is an empty cell)."
  (declare (type obuf buf))
  (typecase value
    (null nil)
    (simple-string (%obuf-write-text buf value))
    (string        (%obuf-write-text buf (coerce value 'simple-string)))
    ((and fixnum unsigned-byte) (%obuf-write-integer buf value))
    (t             (%obuf-write-text buf (coerce (%field-text value) 'simple-string)))))

;;; — Column plan: resolve each field's accessor once per job —

(defun %slot-location (class slot-name)
  "The instance-storage index of SLOT-NAME in CLASS, or NIL (class slots,
   unknown slot)."
  (let ((def (find slot-name (sb-mop:class-slots class) :key #'sb-mop:slot-definition-name)))
    (and def
         (let ((loc (sb-mop:slot-definition-location def)))
           (and (integerp loc) loc)))))

(defun %csv-column-plan (record fields)
  "Return (values class plan kinds): for each of FIELDS (Salesforce API names), how
   to read it from RECORD-shaped objects — the dedicated Id accessor (always
   works, including generic result objects with no raw data), a typed slot
   (by storage index when RECORD's class allows it), or SOBJECT-RAW.  The
   slot map is taken from RECORD's class, as every record in a job is the
   same type.  KINDS holds, per column, \"date\" or \"datetime\" for a slot that
   holds timestamps (so they are written the way Salesforce wants), else NIL."
  (let* ((class    (class-of record))
         (slot-map (gethash (class-name class) *sobject-slot-maps*))
         (kind-map (gethash (class-name class) cl-sf/objects::*sobject-field-kinds*)))
    (values class
            (map 'simple-vector
                 (lambda (field)
                   (let ((pair (and slot-map (assoc field slot-map :test #'equal))))
                     (cond ((equal field "Id") (list :id))
                           (pair (let ((loc (%slot-location class (cdr pair))))
                                   (if loc
                                       (list :slot loc (cdr pair) field)
                                       (list :slot-name (cdr pair) field))))
                           (t (list :raw field)))))
                 fields)
            (map 'simple-vector (lambda (field) (and kind-map (gethash field kind-map))) fields))))

(declaim (inline %column-value))
(defun %column-value (entry obj class)
  "OBJ's value for a plan ENTRY.  The by-index slot read is only valid for
   instances of exactly CLASS; any other object (e.g. a generic SOBJECT of
   the same sObject type) uses the slot if it has one, else its raw data."
  (declare (type cons entry) (optimize (speed 3) (safety 0)))
  (labels ((raw (field)
             (let ((raw (sobject-raw obj)))
               (and raw (gethash field raw))))
           (slot (name field)
             (cond ((not (slot-exists-p obj name)) (raw field))
                   ((slot-boundp obj name) (slot-value obj name))
                   (t nil))))
    (case (car entry)
      (:id (sobject-id obj))
      (:slot (if (eq (class-of obj) class)
                 (let ((v (sb-mop:standard-instance-access obj (the fixnum (cadr entry)))))
                   (if (eq v sb-pcl::+slot-unbound+) nil v))
                 (slot (caddr entry) (cadddr entry))))
      (:slot-name (slot (cadr entry) (caddr entry)))
      (t (raw (cadr entry))))))

(defun %write-csv-header (buf fields)
  (loop for field in fields
        for first = t then nil
        do (unless first (%obuf-byte buf 44))
           (%obuf-write-text buf (coerce field 'simple-string)))
  (%obuf-byte buf 10))

(defun %write-csv-rows (buf records plan class kinds)
  (declare (type obuf buf) (type simple-vector plan kinds))
  (dolist (obj records)
    (dotimes (i (length plan))
      (when (> i 0) (%obuf-byte buf 44))
      (let ((value (%column-value (svref plan i) obj class)))
        (%write-field buf (if (typep value 'local-time:timestamp)
                              (cl-sf/objects::%timestamp-text value (or (svref kinds i) "datetime"))
                              value))))
    (%obuf-byte buf 10)))

(defun %bulk-column-names (records operation &optional external-id-field)
  "Determine the ordered list of CSV column headers for an ingest job.
   :INSERT  — all non-read-only fields, excluding Id
   :UPDATE  — Id (required) + all non-read-only data fields
   :UPSERT  — all fields excluding read-only ones EXCEPT external-id-field
   :DELETE  — only Id"
  (let* ((first     (car records))
         (slot-map  (gethash (class-name (class-of first)) *sobject-slot-maps*))
         (all-fields
          (if slot-map
              (mapcar #'car slot-map)
              ;; Generic: derive from sf-raw keys (preserve insertion order)
              (when (sobject-raw first)
                (loop for k being the hash-keys of (sobject-raw first)
                      unless (equal k "attributes")
                      collect k)))))
    (ecase operation
      (:delete
       '("Id"))
      (:insert
       (remove-if (lambda (f)
                    (member f *sf-read-only-fields* :test #'equal))
                  all-fields))
      (:update
       (let ((data-fields
              (remove-if (lambda (f)
                           (or (equal f "Id")
                               (member f *sf-read-only-fields* :test #'equal)))
                         all-fields)))
         (list* "Id" data-fields)))
      (:upsert
       ;; The external-id field must be a column even if it would normally be
       ;; excluded — and \"Id\" never is in the slot map, so upserting on Id
       ;; used to send no Id at all and insert every row as new.
       (let ((columns (remove-if (lambda (f)
                                   (and (member f *sf-read-only-fields* :test #'equal)
                                        (not (equal f external-id-field))))
                                 all-fields)))
         (if (or (null external-id-field) (member external-id-field columns :test #'equal))
             columns
             (cons external-id-field columns)))))))

(defun %sobjects->csv (records fields)
  "Convert the list RECORDS to a Bulk 2.0 CSV with columns FIELDS, as an
   exact-size UTF-8 octet vector.  Sequential for n <= *BULK-CSV-CHUNK-SIZE*;
   larger lists are split into chunks written concurrently into their own
   buffers on lparallel workers, then joined with a single copy."
  (declare (type list records fields))
  (multiple-value-bind (class plan kinds)
      (if records
          (%csv-column-plan (car records) fields)
          (values nil (vector) (vector)))
    (let* ((n          (length records))
           (ncols      (length fields))
           (chunk-size *bulk-csv-chunk-size*)
           ;; ~12 bytes a cell is a cheap guess; buffers grow if it's low
           (head       (%make-obuf (max 4096 (* (min n chunk-size) ncols 12)))))
      (%write-csv-header head fields)
      (if (<= n chunk-size)
          (progn (%write-csv-rows head records plan class kinds)
                 (%obuf-octets head))
          (let* ((rest   records)
                 (chunks (loop while rest
                               collect (loop repeat chunk-size while rest collect (pop rest)))))
            (cl-sf/auth::%ensure-lparallel-kernel)
            (let* ((bufs  (lparallel:pmapcar
                           (lambda (chunk)
                             (let ((buf (%make-obuf (max 4096 (* (length chunk) ncols 12)))))
                               (%write-csv-rows buf chunk plan class kinds)
                               buf))
                           chunks))
                   (total (+ (obuf-fill head) (reduce #'+ bufs :key #'obuf-fill)))
                   (out   (make-array total :element-type '(unsigned-byte 8)))
                   (pos   0))
              (dolist (buf (cons head bufs) out)
                (replace out (obuf-bytes buf) :start1 pos :end2 (obuf-fill buf))
                (incf pos (obuf-fill buf)))))))))

;;; ──────────────────────────────────────────────────────────────────
;;; CSV parser — for Bulk 2.0 result downloads
;;;
;;; Works on the raw UTF-8 bytes of the HTTP response (no decode to a Lisp
;;; string first) in one pass, and builds strings ONLY for the columns the
;;; caller names — a result file has ~25 columns and correlation needs
;;; two or three, so most of the (otherwise dominant) string allocation
;;; never happens.  RFC 4180: quoted fields, "" escapes, commas and
;;; newlines inside quotes, LF or CRLF row ends, blank lines skipped.
;;; (Measured at ~100 ms for 16 MB with every column built, ~18 ms for 3 of
;;; 20; an AVX2 scan found the delimiters in 7 ms but changed the total
;;; not at all, since string construction dominates.)
;;; ──────────────────────────────────────────────────────────────────

(defun %field-string (buf start end)
  "BUF[START,END) as a string: ASCII bytes copied directly, anything else
   decoded as UTF-8."
  (declare (type octets buf) (type fixnum start end) (optimize (speed 3) (safety 0)))
  (let* ((len (- end start))
         (s   (make-string len)))
    (declare (type fixnum len))
    (dotimes (k len s)
      (let ((b (aref buf (+ start k))))
        (when (>= b 128)
          (return (sb-ext:octets-to-string buf :start start :end end :external-format :utf-8)))
        (setf (schar s k) (code-char b))))))

(defun %field-string-unescaped (buf start end)
  "Like %FIELD-STRING, collapsing each doubled quote to a single one."
  (declare (type octets buf) (type fixnum start end) (optimize (speed 3) (safety 0)))
  (let ((tmp (make-array (- end start) :element-type '(unsigned-byte 8)))
        (m 0) (k start))
    (declare (type fixnum m k))
    (loop while (< k end)
          do (let ((b (aref buf k)))
               (setf (aref tmp m) b)
               (incf m) (incf k)
               (when (and (= b 34) (< k end) (= (aref buf k) 34))
                 (incf k))))
    (%field-string tmp 0 m)))

(declaim (inline %csv-field))
(defun %csv-field (buf start end quoted escaped)
  (cond ((not quoted) (%field-string buf start end))
        (escaped      (%field-string-unescaped buf start end))
        (t            (%field-string buf start end))))

(declaim (inline %csv-scan-row))
(defun %csv-scan-row (buf i n emit)
  "Scan the row starting at byte I of BUF (length N), calling
   (EMIT column start end quoted escaped) for each field — START/END exclude
   the surrounding quotes of a quoted field.  Returns the index just past
   the row's line terminator."
  (declare (type octets buf) (type fixnum i n) (type function emit)
           (optimize (speed 3) (safety 0)))
  (let ((col 0))
    (declare (type fixnum col))
    (loop
      (if (and (< i n) (= (aref buf i) 34))
          (let ((j (1+ i)) (escaped nil))
            (declare (type fixnum j))
            (loop (cond ((>= j n) (return))
                        ((= (aref buf j) 34)
                         (if (and (< (1+ j) n) (= (aref buf (1+ j)) 34))
                             (setf escaped t j (+ j 2))
                             (return)))
                        (t (incf j))))
            (funcall emit col (1+ i) j t escaped)
            (setf i (min n (1+ j))))
          (let ((j i))
            (declare (type fixnum j))
            (loop while (and (< j n)
                             (let ((c (aref buf j)))
                               (not (or (= c 44) (= c 10) (= c 13)))))
                  do (incf j))
            (funcall emit col i j nil nil)
            (setf i j)))
      (incf col)
      (cond ((>= i n) (return i))
            ((= (aref buf i) 44)
             (incf i)
             (when (>= i n)                 ; a comma right at EOF: one more, empty field
               (funcall emit col n n nil nil)
               (return n)))
            (t                              ; LF, CR or CRLF
             (when (and (= (aref buf i) 13) (< (1+ i) n) (= (aref buf (1+ i)) 10))
               (incf i))
             (return (1+ i)))))))

(defun %csv-select (octets names)
  "Parse the CSV in OCTETS (UTF-8 bytes) whose first row is a header.
   Returns (values rows header): ROWS is a list with one SIMPLE-VECTOR per
   data row, holding that row's value for each of NAMES in order (\"\" when
   the row is too short, NIL when NAMES' column is not in the header);
   HEADER is the list of all column names.  Only NAMES' columns are built
   as strings."
  (declare (type octets octets))
  (let ((n (length octets)))
    (when (zerop n)
      (return-from %csv-select (values nil nil)))
    (let* ((i 0) (header '()))
      (declare (type fixnum i))
      (setf i (%csv-scan-row octets 0 n
                             (lambda (col start end quoted escaped)
                               (declare (ignore col))
                               (push (%csv-field octets start end quoted escaped) header))))
      (setf header (nreverse header))
      (let* ((ncols       (length header))
             (slot-of-col (make-array ncols :initial-element nil))
             (template    (make-array (length names) :initial-element nil))
             (rows        '()))
        (declare (type fixnum ncols))
        (loop for name in names
              for out from 0
              do (let ((col (position name header :test #'string=)))
                   (when col
                     (setf (svref slot-of-col col) out
                           (svref template out) ""))))
        (loop while (< i n)
              do (if (let ((c (aref octets i))) (or (= c 10) (= c 13)))
                     (incf i)                ; blank line
                     (let ((row (copy-seq template)))
                       (flet ((emit (col start end quoted escaped)
                                (declare (type fixnum col))
                                (when (< col ncols)
                                  (let ((out (svref slot-of-col col)))
                                    (when out
                                      (setf (svref row out)
                                            (%csv-field octets start end quoted escaped)))))))
                         (declare (dynamic-extent #'emit))
                         (setf i (%csv-scan-row octets i n #'emit)))
                       (push row rows))))
        (values (nreverse rows) header)))))

;;; ──────────────────────────────────────────────────────────────────
;;; Job lifecycle
;;; ──────────────────────────────────────────────────────────────────

(defun %bulk-path (&optional (suffix ""))
  "Return the relative Bulk 2.0 ingest jobs path."
  (format nil "jobs/ingest~A" suffix))

(defun %bulk-job-terminal-p (state)
  "Return T if STATE is a terminal Bulk 2.0 job state."
  (member state '("JobComplete" "Failed" "Aborted") :test #'equal))

(defun %poll-job (conn job-id &key (interval 5) (timeout 3600) (sleep #'sleep))
  "Poll JOB-ID until it reaches a terminal state, or TIMEOUT seconds pass
   (BULK-TIMEOUT-ERROR).  Waits between checks grow 0.5 s, 1 s, 2 s, ... up
   to INTERVAL, so a job that finishes in seconds is noticed in seconds,
   while a long one is polled every INTERVAL.  SLEEP is the wait function
   (for tests).  Returns the final job status hash-table."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second)))
        (delay 0.5))
    (loop
      (when (> (get-internal-real-time) deadline)
        (error 'bulk-timeout-error
               :job-id  job-id
               :timeout timeout
               :message (format nil "Bulk job ~A timed out after ~A seconds"
                                 job-id timeout)))
      (let* ((status (get-bulk-job job-id :conn conn))
             (state  (gethash "state" status "")))
        (when (%bulk-job-terminal-p state)
          (return status))
        (funcall sleep (min delay interval))
        (setf delay (* delay 2))))))

(defun create-bulk-job (sobject-type operation
                        &key (conn *connection*) (line-ending "LF") external-id-field-name)
  "Create a Bulk 2.0 ingest job.
   SOBJECT-TYPE         — Salesforce API object name string (e.g. \"Account\")
   OPERATION            — \"insert\" \"update\" \"upsert\" \"delete\" \"hardDelete\"
   LINE-ENDING          — \"LF\" (default) or \"CRLF\"
   EXTERNAL-ID-FIELD-NAME — required for upsert operations
   Returns the job status hash-table (contains \"id\", \"state\", etc.)."
  (let ((body (let ((h (make-hash-table :test #'equal)))
                (setf (gethash "object"          h) sobject-type
                      (gethash "operation"       h) operation
                      (gethash "contentType"     h) "CSV"
                      (gethash "lineEnding"      h) line-ending)
                (when external-id-field-name
                  (setf (gethash "externalIdFieldName" h) external-id-field-name))
                h)))
    (api-request :post (%bulk-path) :body body :conn conn)))

(defun close-bulk-job (job-id &key (conn *connection*))
  "Signal UploadComplete for JOB-ID, triggering Salesforce to begin processing.
   Returns the updated job status hash-table."
  (let ((body (let ((h (make-hash-table :test #'equal)))
                (setf (gethash "state" h) "UploadComplete")
                h)))
    (api-request :patch (%bulk-path (format nil "/~A" job-id)) :body body :conn conn)))

(defun abort-bulk-job (job-id &key (conn *connection*))
  "Abort JOB-ID. Returns the updated job status hash-table."
  (let ((body (let ((h (make-hash-table :test #'equal)))
                (setf (gethash "state" h) "Aborted")
                h)))
    (api-request :patch (%bulk-path (format nil "/~A" job-id)) :body body :conn conn)))

(defun get-bulk-job (job-id &key (conn *connection*))
  "Return the current status hash-table for JOB-ID."
  (api-request :get (%bulk-path (format nil "/~A" job-id)) :conn conn))

(defun upload-job-data (job-id data &key (conn *connection*) (operation :insert) external-id-field)
  "Upload DATA to JOB-ID via HTTP PUT with Content-Type: text/csv.
   DATA may be:
     - A string or octet vector (CSV already serialized — used as-is)
     - A list of SOBJECT instances (serialized automatically using OPERATION
       to pick the right CSV columns — must match the operation JOB-ID was
       created with)
   OPERATION — :insert (default), :update, :upsert, or :delete.
   EXTERNAL-ID-FIELD — required when OPERATION is :upsert.
   Returns T on success."
  (let ((csv (etypecase data
               (string data)
               (octets data)
               (list   (%sobjects->csv data (%bulk-column-names data operation external-id-field))))))
    (api-request :put
                 (%bulk-path (format nil "/~A/batches" job-id))
                 :body         csv
                 ;; NOTE: must be ">= 10 chars" — dexador's WinHTTP backend
                 ;; does (string= content-type "multipart/" :end1 10)
                 ;; unconditionally, with no length guard, so a short
                 ;; Content-Type like plain "text/csv" (8 chars) blows up
                 ;; with a bounding-indices error on Windows.  "; charset=
                 ;; UTF-8" is also the more correct thing to send anyway.
                 :content-type "text/csv; charset=UTF-8"
                 :conn         conn)))

;;; ──────────────────────────────────────────────────────────────────
;;; Raw result accessors (return CSV strings)
;;; ──────────────────────────────────────────────────────────────────

(defun get-successful-results (job-id &key (conn *connection*))
  "Return the raw CSV of successfully processed records for JOB-ID, as UTF-8 bytes."
  (api-request :get
               (%bulk-path (format nil "/~A/successfulResults" job-id))
               :raw :octets :conn conn))

(defun get-failed-results (job-id &key (conn *connection*))
  "Return the raw CSV of failed records for JOB-ID, as UTF-8 bytes."
  (api-request :get
               (%bulk-path (format nil "/~A/failedResults" job-id))
               :raw :octets :conn conn))

(defun get-unprocessed-results (job-id &key (conn *connection*))
  "Return the raw CSV of unprocessed records for JOB-ID, as UTF-8 bytes."
  (api-request :get
               (%bulk-path (format nil "/~A/unprocessedrecords" job-id))
               :raw :octets :conn conn))

;;; ──────────────────────────────────────────────────────────────────
;;; Result correlation
;;; ──────────────────────────────────────────────────────────────────

(defun %results-to-fetch (status)
  "Which result files of a finished job are worth downloading, from its
   STATUS counts: successful only if some record succeeded
   (numberRecordsProcessed counts the failed ones too), failed only if some
   failed.  Unprocessed records exist only for an aborted/failed job, which
   never gets this far.  Without counts, fetch everything."
  (let ((processed (and status (gethash "numberRecordsProcessed" status)))
        (failed    (and status (gethash "numberRecordsFailed" status))))
    (if (and (integerp processed) (integerp failed))
        (append (when (> processed failed) '(:successful))
                (when (> failed 0)         '(:failed)))
        '(:successful :failed :unprocessed))))

(defun %fetch-result-rows (conn job-id kinds columns)
  "Download and parse the result files named by KINDS (:successful :failed
   :unprocessed), concurrently when there is more than one.  Returns an
   alist (kind . (names . rows)): each row is a vector of the file's needed
   columns, NAMES (in order).  COLUMNS are the uploaded columns, which the
   successful and failed files echo back; they are read too, for matching
   inserted rows to the objects that were sent."
  (flet ((names (&rest base)
           (remove-duplicates (append base columns) :test #'string= :from-end t)))
    (let* ((specs  `((:successful get-successful-results ,(names "sf__Id" "sf__Created"))
                     (:failed     get-failed-results     ,(names "sf__Id" "sf__Error" "Id"))
                     (:unprocessed get-unprocessed-results ("Id"))))
           (wanted (remove-if-not (lambda (spec) (member (first spec) kinds)) specs))
           (fetch  (lambda (spec)
                     (list* (first spec) (third spec)
                            (%csv-select (funcall (second spec) job-id :conn conn)
                                         (third spec))))))
      (cond ((null wanted) nil)
            ((null (cdr wanted)) (list (funcall fetch (first wanted))))
            (t (cl-sf/auth::%ensure-lparallel-kernel)
               (lparallel:pmapcar fetch wanted))))))

(defun %row-key (cells)
  "One string identifying a row by its CELLS (length-prefixed, so no value can
   run into the next)."
  (with-output-to-string (out)
    (dolist (cell cells)
      (let ((cell (or cell "")))
        (princ (length cell) out)
        (write-char #\: out)
        (write-string cell out)))))

(defun %upload-index (upload columns)
  "UPLOAD (the CSV we sent, as octets) → a hash-table from each row's key to
   the list of its row numbers, in upload order.  Rows are the records, in
   the order they were given."
  (let* ((rows  (coerce (%csv-select upload columns) 'simple-vector))
         (index (make-hash-table :test #'equal :size (max 7 (length rows)))))
    (loop for i from (1- (length rows)) downto 0
          do (push i (gethash (%row-key (coerce (svref rows i) 'list)) index)))
    index))

(defun %build-bulk-result (conn job-id input-records operation sf-type
                           &key status upload columns)
  "Fetch the result CSVs and correlate them back to the objects that were sent.
   STATUS, the finished job's status, lets files that must be empty be
   skipped (see %RESULTS-TO-FETCH); without it all three are downloaded.

   UPDATE/DELETE: rows match their input object by Id (O(n) hash-map).
   INSERT (and UPSERT rows with no Id of their own): the result files echo
   the columns we uploaded, so UPLOAD and COLUMNS — the CSV and its header —
   let each result row be matched to the object whose row has the same cells.
   Identical rows are paired in order, which is harmless.  The object gets
   the new Id and is what the result holds.  A row that cannot be matched
   (or any row, if UPLOAD is NIL) is a fresh SOBJECT with only the Id.
   Failed rows are (object . error-string), the object NIL if unmatched.
   Unprocessed rows are fresh generic sobjects."
  (let* ((fetched      (%fetch-result-rows conn job-id (%results-to-fetch status) columns))
         (success      (cdr (assoc :successful fetched)))
         (failure      (cdr (assoc :failed fetched)))
         (unproc-rows  (cddr (assoc :unprocessed fetched)))
         (inputs       (coerce input-records 'simple-vector))
         (index        (when (and upload columns (member operation '(:insert :upsert)))
                         (%upload-index upload columns)))
         ;; Id → input-object map for update/delete/upsert
         (id->obj (when (member operation '(:update :delete :upsert))
                    (let ((h (make-hash-table :test #'equal :size (max 7 (length inputs)))))
                      (loop for obj across inputs
                            do (when (sobject-id obj)
                                 (setf (gethash (sobject-id obj) h) obj)))
                      h))))
    (labels ((cell (row names name)
               (let ((i (position name names :test #'string=)))
                 (and i (svref row i))))
             (by-content (row names)
               ;; the object whose uploaded row has the same cells, claimed once
               (when index
                 (let* ((key (%row-key (mapcar (lambda (column) (cell row names column)) columns)))
                        (i   (pop (gethash key index))))
                   (and i (svref inputs i)))))
             (nonblank (id) (and id (plusp (length id)) id)))
      (make-bulk-result
       :successful
       (loop with names = (car success)
             for row in (cdr success)
             for sf-id   = (cell row names "sf__Id")
             for created = (cell row names "sf__Created")
             collect
             (let ((object
                     (or (and id->obj sf-id (not (and (eq operation :upsert) (equal created "true")))
                              (gethash sf-id id->obj))
                         (by-content row names))))
               (cond (object
                      ;; an inserted (or upsert-created) object learns its Id
                      (unless (nonblank (sobject-id object))
                        (setf (sobject-id object) sf-id))
                      object)
                     (t (make-instance 'sobject :id sf-id :type sf-type)))))
       ;; Failed rows: (object-or-nil . error-string).  sf__Id is blank when
       ;; the record never got an Id (a failed insert, or a nonexistent Id);
       ;; the row still echoes the Id we sent, in the original Id column.
       :failed
       (loop with names = (car failure)
             for row in (cdr failure)
             for sf-id = (or (nonblank (cell row names "sf__Id")) (nonblank (cell row names "Id")))
             collect (cons (or (and id->obj sf-id (gethash sf-id id->obj))
                               (by-content row names))
                           (or (cell row names "sf__Error") "Unknown error")))
       ;; Unprocessed: fresh generic sobjects
       :unprocessed
       (loop for row in unproc-rows
             collect (make-instance 'sobject :id (svref row 0) :type sf-type))
       :job-id job-id))))

;;; ──────────────────────────────────────────────────────────────────
;;; High-level CLOS-first operations
;;; ──────────────────────────────────────────────────────────────────

(defun %infer-sobject-type (records)
  "Return the Salesforce API type from the first record in RECORDS.
   Signals BULK-ERROR if RECORDS is empty or the type is unset."
  (when (null records)
    (error 'bulk-error :message "RECORDS list is empty"))
  (let ((type (sobject-type (car records))))
    (unless type
      (error 'bulk-error
             :message "Cannot infer sObject type — ensure SOBJECT-TYPE is set on \
all records (happens automatically for instances created by HASH->SOBJECT or \
DEFINE-SOBJECT-CLASS)."))
    type))

(defun %run-bulk-operation (conn operation records
                            &key external-id-field
                                 (interval 5) (timeout 3600))
  "Orchestrate a complete Bulk 2.0 ingest operation.
   1. Determine columns; start creating the job (a LPARALLEL:FUTURE) while
      the records are serialized to CSV — the two are independent.
   2. Upload the CSV → signal UploadComplete.
   3. %POLL-JOB until the job finishes (backing off from 0.5 s to INTERVAL).
   4. On job failure: signal BULK-JOB-FAILED-ERROR.
   5. Download the needed result files in parallel; return a BULK-RESULT.
   On any error: attempt to abort the job before re-signaling."
  (let* ((sf-type  (%infer-sobject-type records))
         (columns  (%bulk-column-names records operation
                                       (when (eq operation :upsert)
                                         external-id-field)))
         (op-str   (string-downcase (symbol-name operation)))
         (job-future
           (progn
             (cl-sf/auth::%ensure-lparallel-kernel)
             (lparallel:future
               (create-bulk-job sf-type op-str
                                :conn conn
                                :external-id-field-name
                                (when (eq operation :upsert) external-id-field)))))
         (csv      (handler-case (%sobjects->csv records columns)
                     (error (e)
                       ;; the job is being created anyway: don't leave it behind
                       (handler-case
                           (abort-bulk-job (gethash "id" (lparallel:force job-future))
                                           :conn conn)
                         (error ()))
                       (error e))))
         (job      (lparallel:force job-future))
         (job-id   (gethash "id" job)))
    (handler-case
        (progn
          ;; 2. Upload CSV data
          ;; NOTE: ">= 10 chars" — see UPLOAD-JOB-DATA for why plain
          ;; "text/csv" crashes dexador's WinHTTP backend on Windows.
          (api-request :put
                       (%bulk-path (format nil "/~A/batches" job-id))
                       :body         csv
                       :content-type "text/csv; charset=UTF-8"
                       :conn         conn)
          ;; Signal upload complete → Salesforce begins processing
          (close-bulk-job job-id :conn conn)
          ;; 3. Wait for the job to finish
          (let* ((final-status (%poll-job conn job-id
                                          :interval interval
                                          :timeout  timeout))
                 (final-state  (gethash "state" final-status "")))
            ;; 4. Check for job-level failure
            (when (member final-state '("Failed" "Aborted") :test #'equal)
              (error 'bulk-job-failed-error
                     :job-id  job-id
                     :status  final-status
                     :message (format nil "Bulk job ~A entered state ~A"
                                      job-id final-state)))
            ;; 5. Fetch + correlate results
            (%build-bulk-result conn job-id records operation sf-type
                                :status final-status :upload csv :columns columns)))
      ;; On any error, attempt a clean abort so the job doesn't linger
      (error (e)
        (handler-case (abort-bulk-job job-id :conn conn) (error ()))
        (error e)))))

;;; ── Public API ───────────────────────────────────────────────────

(defun bulk-insert (objects &key (conn *connection*) (interval 5) (timeout 3600))
  "Insert OBJECTS (a list of SOBJECT instances) via Salesforce Bulk 2.0.
   The Salesforce API type is inferred from the first element.
   All objects should be the same type.

   Returns a BULK-RESULT:
     - SUCCESSFUL  — the objects you passed, each now holding its Salesforce Id
                     (matched by content; see %BUILD-BULK-RESULT)
     - FAILED      — (object . error-string) conses for each failed record
     - UNPROCESSED — SOBJECT instances not yet processed

   For automatic REST/Bulk routing see DATABASE-INSERT.

   Example:
     (let ((result (bulk-insert (list a1 a2 a3))))
       (mapc (lambda (s) (format t \"Created: ~A~%\" (sobject-id s)))
             (bulk-result-successful result)))"
  (%run-bulk-operation conn :insert objects
                       :interval interval :timeout timeout))

(defun bulk-update (objects &key (conn *connection*) (interval 5) (timeout 3600))
  "Update OBJECTS via Bulk 2.0.  All objects must have SOBJECT-ID set.
   Returns a BULK-RESULT where SUCCESSFUL contains the original input
   objects (matched back by Id).

   Example:
     (dolist (acct accounts) (setf (account-phone acct) \"555-0000\"))
     (bulk-update accounts)"
  (%run-bulk-operation conn :update objects
                       :interval interval :timeout timeout))

(defun bulk-upsert (objects external-id-field
                    &key (conn *connection*) (interval 5) (timeout 3600))
  "Upsert OBJECTS via Bulk 2.0 using EXTERNAL-ID-FIELD as the match key.
   EXTERNAL-ID-FIELD is the Salesforce API field name string
   (e.g. \"My_External_Id__c\").
   Returns a BULK-RESULT."
  (%run-bulk-operation conn :upsert objects
                       :external-id-field external-id-field
                       :interval interval :timeout timeout))

(defun bulk-delete (objects &key (conn *connection*) (interval 5) (timeout 3600))
  "Delete OBJECTS via Bulk 2.0.  Only SOBJECT-ID is used from each object.
   Returns a BULK-RESULT."
  (%run-bulk-operation conn :delete objects
                       :interval interval :timeout timeout))
