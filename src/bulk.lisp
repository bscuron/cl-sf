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
  "Record count above which SOBJECT-CREATE-MANY etc. automatically route
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

   SUCCESSFUL  — list of SOBJECT instances.
                 For update/delete: the original input objects (matched by Id).
                 For insert/upsert: fresh SOBJECT instances with only Id set.
   FAILED      — list of (sobject-or-nil . error-string) conses.
                 CAR is NIL for insert failures (no Id to match back).
   UNPROCESSED — list of SOBJECT instances not processed (e.g. job aborted).
   JOB-ID      — the Salesforce Bulk 2.0 job Id string."
  (successful  nil :type list)
  (failed      nil :type list)
  (unprocessed nil :type list)
  (job-id      nil :type (or string null)))

;;; ──────────────────────────────────────────────────────────────────
;;; CSV generation — performance-critical
;;; ──────────────────────────────────────────────────────────────────

(declaim (ftype (function (string) boolean) %csv-needs-quoting-p)
         (inline %csv-needs-quoting-p))
(defun %csv-needs-quoting-p (s)
  "Return T if string S contains any CSV special character.
   Inlined; tight loop for minimal per-field overhead."
  (declare (type simple-string s)
           (optimize (speed 3) (safety 1) (debug 0)))
  (dotimes (i (length s) nil)
    (let ((c (schar s i)))
      (when (or (char= c #\,) (char= c #\") (char= c #\newline) (char= c #\return))
        (return t)))))

(defun %write-csv-field (value stream)
  "Write VALUE as a single CSV field to STREAM.
   Allocates only for fields that require quoting (rare in practice)."
  (declare (optimize (speed 3) (safety 1)))
  (let ((s (etypecase value
             (null     "")
             (string   value)
             (integer  (write-to-string value))
             (float    (write-to-string value))
             (double-float (write-to-string value))
             (symbol   (symbol-name value))
             (t        (princ-to-string value)))))
    (declare (type simple-string s))
    (if (%csv-needs-quoting-p s)
        ;; Quoted: wrap in double-quotes, escape internal double-quotes as ""
        (progn
          (write-char #\" stream)
          (dotimes (i (length s))
            (let ((c (schar s i)))
              (when (char= c #\") (write-char #\" stream))
              (write-char c stream)))
          (write-char #\" stream))
        ;; Unquoted: write directly, zero allocation
        (write-string s stream))))

(defun %sobject-field-value (obj sf-field-name slot-map)
  "Return the value of SF-FIELD-NAME in OBJ.
   Id is checked first via the dedicated SOBJECT-ID accessor — Id is
   always excluded from generated slot-maps (it's read-only) and a
   bulk-result object may have neither a slot-map entry nor a populated
   SF-RAW (e.g. a fresh insert result, (make-instance 'sobject :id ...)
   with no :raw) — so Id must never depend on either of those.  This
   matters for round-tripping bulk-insert's own results straight into a
   later bulk-update/delete/upsert call, which is a normal thing to do.
   Otherwise looks up via SLOT-MAP first (O(1) with small alists), falls
   back to SF-RAW."
  (declare (optimize (speed 3) (safety 1)))
  (cond
    ((equal sf-field-name "Id") (sobject-id obj))
    (slot-map
     (let ((pair (assoc sf-field-name slot-map :test #'equal)))
       (if pair
           (let ((slot (cdr pair)))
             (if (slot-boundp obj slot) (slot-value obj slot) nil))
           ;; Field not in slot map — check raw data
           (let ((raw (sobject-raw obj)))
             (when raw (gethash sf-field-name raw))))))
    (t
     ;; Generic sobject: all data lives in sf-raw
     (let ((raw (sobject-raw obj)))
       (when raw (gethash sf-field-name raw))))))

(defun %write-csv-header (fields stream)
  "Write the CSV header row to STREAM."
  (loop for field in fields
        for i from 0
        when (> i 0) do (write-char #\, stream)
        do (write-string field stream))
  (write-char #\newline stream))

(defun %write-csv-row (obj fields slot-map stream)
  "Write one SOBJECT as a CSV row to STREAM."
  (loop for field in fields
        for i from 0
        when (> i 0) do (write-char #\, stream)
        do (%write-csv-field (%sobject-field-value obj field slot-map) stream))
  (write-char #\newline stream))

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
       ;; Keep external-id-field even if it would normally be excluded
       (remove-if (lambda (f)
                    (and (member f *sf-read-only-fields* :test #'equal)
                         (not (equal f external-id-field))))
                  all-fields)))))

(defun %sobjects->csv (records fields)
  "Convert list of RECORDS to a Bulk 2.0 CSV string.
   Sequential for n ≤ *BULK-CSV-CHUNK-SIZE*; parallel via LPARALLEL:PMAPCAR
   for larger lists.  Final assembly always uses a single stream write."
  (declare (type list records fields))
  (let* ((first     (car records))
         (slot-map  (when first
                      (gethash (class-name (class-of first)) *sobject-slot-maps*)))
         (n         (length records)))
    (if (<= n *bulk-csv-chunk-size*)
        ;; ── Sequential: small payloads, minimal overhead ─────────────
        (with-output-to-string (out)
          (%write-csv-header fields out)
          (dolist (obj records)
            (%write-csv-row obj fields slot-map out)))
        ;; ── Parallel: large payloads, rows generated concurrently ─────
        (progn
         (cl-sf/auth::%ensure-lparallel-kernel)
         (let* ((chunks
                (loop for i from 0 below n by *bulk-csv-chunk-size*
                      collect (subseq records i (min (+ i *bulk-csv-chunk-size*) n))))
               ;; Each worker serializes its chunk to a string fragment
               (fragments
                (lparallel:pmapcar
                 (lambda (chunk)
                   (with-output-to-string (s)
                     (dolist (obj chunk)
                       (%write-csv-row obj fields slot-map s))))
                 chunks)))
          ;; Join: header + fragments in order, single stream write
          (with-output-to-string (out)
            (%write-csv-header fields out)
            (dolist (frag fragments)
              (write-string frag out))))))))

;;; ──────────────────────────────────────────────────────────────────
;;; CSV parser — for Bulk 2.0 result downloads
;;; ──────────────────────────────────────────────────────────────────

(defun %csv-parse-line (line)
  "Parse one CSV line into a list of field strings.
   Handles RFC-4180: double-quoted fields, \"\" for escaped quote, CRLF."
  (declare (type string line)
           (optimize (speed 3) (safety 1)))
  (let ((result '())
        (i      0)
        (len    (length line)))
    (loop while (< i len) do
      (cond
        ;; Quoted field
        ((char= (char line i) #\")
         (incf i)  ; skip opening quote
         (let ((buf (make-array 32 :element-type 'character
                                   :adjustable t :fill-pointer 0)))
           (loop while (< i len)
                 for c = (char line i)
                 do (cond
                      ((char= c #\")
                       (incf i)
                       ;; "" → escaped quote; otherwise end of field
                       (if (and (< i len) (char= (char line i) #\"))
                           (progn (vector-push-extend #\" buf) (incf i))
                           (return)))
                      (t
                       (vector-push-extend c buf)
                       (incf i))))
           (push (copy-seq buf) result))
         ;; Consume comma separator after closing quote
         (when (and (< i len) (char= (char line i) #\,))
           (incf i)))
        ;; Comma → empty field
        ((char= (char line i) #\,)
         (push "" result)
         (incf i))
        ;; Unquoted field
        (t
         (let ((start i))
           (loop while (and (< i len) (char/= (char line i) #\,))
                 do (incf i))
           (push (subseq line start i) result)
           (when (and (< i len) (char= (char line i) #\,))
             (incf i))))))
    ;; Trailing comma means one more empty field
    (when (and (> len 0) (char= (char line (1- len)) #\,))
      (push "" result))
    (nreverse result)))

(defun %csv-parse (csv-string)
  "Parse a Salesforce Bulk 2.0 CSV result string.
   Returns (values header row-alists) where:
     HEADER    — list of column name strings from the first row
     ROW-ALISTS — list of alists (column-name . value) for each data row"
  (let* ((raw-lines (cl-ppcre:split "\\r?\\n" csv-string))
         (lines     (remove-if (lambda (l) (zerop (length l))) raw-lines)))
    (if (null lines)
        (values nil nil)
        (let* ((header (%csv-parse-line (first lines)))
               (rows   (loop for line in (rest lines)
                             for fields = (%csv-parse-line line)
                             ;; Zip header with field values; pad shorter rows
                             collect (loop for h in header
                                           for f in (append fields
                                                            (make-list
                                                             (max 0 (- (length header)
                                                                       (length fields)))
                                                             :initial-element ""))
                                           collect (cons h f)))))
          (values header rows)))))

;;; ──────────────────────────────────────────────────────────────────
;;; Job lifecycle
;;; ──────────────────────────────────────────────────────────────────

(defun %bulk-path (&optional (suffix ""))
  "Return the relative Bulk 2.0 ingest jobs path."
  (format nil "jobs/ingest~A" suffix))

(defun %bulk-job-terminal-p (state)
  "Return T if STATE is a terminal Bulk 2.0 job state."
  (member state '("JobComplete" "Failed" "Aborted") :test #'equal))

(defun %poll-job (conn job-id &key (interval 5) (timeout 3600))
  "Poll JOB-ID every INTERVAL seconds until a terminal state or TIMEOUT.
   Called inside a LPARALLEL:FUTURE by %RUN-BULK-OPERATION.
   Returns the final job status hash-table."
  (let ((deadline (+ (get-universal-time) timeout)))
    (loop
      (when (> (get-universal-time) deadline)
        (error 'bulk-timeout-error
               :job-id  job-id
               :timeout timeout
               :message (format nil "Bulk job ~A timed out after ~A seconds"
                                 job-id timeout)))
      (let* ((status (get-bulk-job job-id :conn conn))
             (state  (gethash "state" status "")))
        (when (%bulk-job-terminal-p state)
          (return status))
        (sleep interval)))))

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
     - A string (CSV already serialized — used as-is, zero-copy)
     - A list of SOBJECT instances (serialized automatically using OPERATION
       to pick the right CSV columns — must match the operation JOB-ID was
       created with)
   OPERATION — :insert (default), :update, :upsert, or :delete.
   EXTERNAL-ID-FIELD — required when OPERATION is :upsert.
   Returns T on success."
  (let ((csv (etypecase data
               (string data)
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
  "Return the raw CSV string of successfully processed records for JOB-ID."
  (api-request :get
               (%bulk-path (format nil "/~A/successfulResults" job-id))
               :raw t :conn conn))

(defun get-failed-results (job-id &key (conn *connection*))
  "Return the raw CSV string of failed records for JOB-ID."
  (api-request :get
               (%bulk-path (format nil "/~A/failedResults" job-id))
               :raw t :conn conn))

(defun get-unprocessed-results (job-id &key (conn *connection*))
  "Return the raw CSV string of unprocessed records for JOB-ID."
  (api-request :get
               (%bulk-path (format nil "/~A/unprocessedrecords" job-id))
               :raw t :conn conn))

;;; ──────────────────────────────────────────────────────────────────
;;; Result correlation
;;; ──────────────────────────────────────────────────────────────────

(defun %build-bulk-result (conn job-id input-records operation sf-type)
  "Fetch all three result CSVs and correlate them back to input sobjects.

   For INSERT: successful rows → fresh sobjects with only Id set.
   For UPDATE/DELETE: successful rows → input sobjects matched by Id (O(n) hash-map).
   For UPSERT: mix of above based on sf__Created field.
   Failed rows → (nil . error-string) conses (no reliable back-matching for insert).
   Unprocessed rows → fresh generic sobjects."
  ;; ── Fetch all three result CSVs ─────────────────────────────────
  (let* ((success-csv     (get-successful-results job-id :conn conn))
         (failed-csv      (get-failed-results     job-id :conn conn))
         (unproc-csv      (get-unprocessed-results job-id :conn conn)))
    ;; ── Parse CSVs ───────────────────────────────────────────────
    (multiple-value-bind (_sh success-rows)  (%csv-parse success-csv)  (declare (ignore _sh))
    (multiple-value-bind (_fh failed-rows)   (%csv-parse failed-csv)   (declare (ignore _fh))
    (multiple-value-bind (_uh unproc-rows)   (%csv-parse unproc-csv)   (declare (ignore _uh))
      ;; ── Build Id → input-object map for update/delete/upsert ──
      (let ((id->obj (when (member operation '(:update :delete :upsert))
                       (let ((h (make-hash-table :test #'equal
                                                 :size (length input-records))))
                         (dolist (obj input-records)
                           (when (sobject-id obj)
                             (setf (gethash (sobject-id obj) h) obj)))
                         h))))
        ;; ── Correlate successful results ───────────────────────
        (let ((successful
               (loop for row in success-rows
                     for sf-id    = (cdr (assoc "sf__Id"      row :test #'equal))
                     for created  = (cdr (assoc "sf__Created" row :test #'equal))
                     collect
                     (cond
                       ;; Insert or new upsert: no input object to match
                       ((or (eq operation :insert)
                            (and (eq operation :upsert)
                                 (equal created "true")))
                        (make-instance 'sobject :id sf-id :type sf-type))
                       ;; Update/delete/upsert-update: look up by Id
                       ((and id->obj sf-id)
                        (or (gethash sf-id id->obj)
                            ;; Fallback: fresh object
                            (make-instance 'sobject :id sf-id :type sf-type)))
                       (t
                        (make-instance 'sobject :id sf-id :type sf-type)))))
              ;; ── Failed rows: (nil-or-obj . error-string) ─────
              (failed
               (loop for row in failed-rows
                     for sf-id  = (cdr (assoc "sf__Id"    row :test #'equal))
                     for err    = (cdr (assoc "sf__Error" row :test #'equal))
                     for obj    = (when (and id->obj (not (equal sf-id "")))
                                    (gethash sf-id id->obj))
                     collect (cons obj (or err "Unknown error"))))
              ;; ── Unprocessed: fresh generic sobjects ───────────
              (unprocessed
               (loop for row in unproc-rows
                     for sf-id = (cdr (assoc "Id" row :test #'equal))
                     collect (make-instance 'sobject :id sf-id :type sf-type))))
          (make-bulk-result :successful  successful
                            :failed      failed
                            :unprocessed unprocessed
                            :job-id      job-id))))))))

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
   1. Determine columns and serialize records to CSV.
   2. Create job → upload CSV → signal UploadComplete.
   3. Dispatch %POLL-JOB in a LPARALLEL:FUTURE (non-blocking poll loop).
   4. FORCE the future (blocks until job finishes or times out).
   5. On failure: signal BULK-JOB-FAILED-ERROR.
   6. Fetch and correlate results; return BULK-RESULT.
   On any error: attempt to abort the job before re-signaling."
  (let* ((sf-type  (%infer-sobject-type records))
         (columns  (%bulk-column-names records operation
                                       (when (eq operation :upsert)
                                         external-id-field)))
         (csv      (%sobjects->csv records columns))
         (op-str   (string-downcase (symbol-name operation)))
         ;; 1. Create job
         (job      (create-bulk-job sf-type op-str
                                    :conn conn
                                    :external-id-field-name
                                    (when (eq operation :upsert) external-id-field)))
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
          ;; 3. Signal upload complete → Salesforce begins processing
          (close-bulk-job job-id :conn conn)
          ;; 4. Poll in a lparallel future — caller thread is free
          (cl-sf/auth::%ensure-lparallel-kernel)
          (let* ((poll-future (lparallel:future
                                (%poll-job conn job-id
                                           :interval interval
                                           :timeout  timeout)))
                 ;; 5. Block until done
                 (final-status (lparallel:force poll-future))
                 (final-state  (gethash "state" final-status "")))
            ;; 6. Check for job-level failure
            (when (member final-state '("Failed" "Aborted") :test #'equal)
              (error 'bulk-job-failed-error
                     :job-id  job-id
                     :status  final-status
                     :message (format nil "Bulk job ~A entered state ~A"
                                      job-id final-state)))
            ;; 7. Fetch + correlate results
            (%build-bulk-result conn job-id records operation sf-type)))
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
     - SUCCESSFUL  — fresh SOBJECT instances with Salesforce-assigned Ids
     - FAILED      — (nil . error-string) conses for each failed record
     - UNPROCESSED — SOBJECT instances not yet processed

   For automatic REST/Bulk routing see SOBJECT-CREATE-MANY.

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
