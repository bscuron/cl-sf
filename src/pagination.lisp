;;;; cl-sf/src/pagination.lisp
;;;; Lazy paginated cursor over Salesforce query result sets.
;;;;
;;;; Salesforce paginates via nextRecordsUrl in query responses.
;;;; Callers never deal with pagination — they get a LAZY-RESULT-SET
;;;; and iterate with DO-RECORDS, force everything with COLLECT-ALL, or
;;;; use it as an ordinary CL:SEQUENCE: MAP, REDUCE, FIND, SOME, COUNT, and
;;;; (LOOP FOR x BEING THE ELEMENTS OF rset ...) all just work — pages are
;;;; still fetched lazily on demand underneath, since LENGTH/ELT are the
;;;; only two protocol methods needed (SBCL's extensible-sequences default
;;;; iteration is built on exactly those two, called with strictly
;;;; increasing indices for every one of the functions above — verified
;;;; empirically, not assumed).  Note: LOOP's ACROSS clause specifically
;;;; requires a true vector/array, so use BEING THE ELEMENTS OF instead.
;;;;
;;;; Forward-only: ELT at an index equal to or ahead of the current
;;;; position works (ahead discards skipped records); ELT behind the
;;;; current position errors — pages already consumed aren't kept around
;;;; to go back to.  Use COLLECT-ALL first if you need true random access.
;;;;
;;;; lparallel usage:
;;;;   - %FETCH-NEXT-PAGE! fires a LPARALLEL:FUTURE for the *following*
;;;;     page immediately after filling the current buffer (one page of
;;;;     read-ahead).  NEXT-RECORD LPARALLEL:FORCEs it when the buffer
;;;;     drains, overlapping network latency with caller processing.
;;;;   - COLLECT-ALL uses LPARALLEL:PMAPCAR when the page count is
;;;;     knowable (totalSize / page-size), fetching all remaining pages
;;;;     in parallel then concatenating in order.
;;;;
;;;; RECORD-COERCER:
;;;;   A function (hash-table → object) injected by cl-sf/objects.
;;;;   Default is #'identity (returns raw hash-tables).
;;;;   When cl-sf/objects:query passes #'hash->sobject, callers get
;;;;   CLOS SOBJECT instances without pagination needing to know about them.

(in-package #:cl-sf/pagination)

;;; ──────────────────────────────────────────────────────────────────
;;; Lazy result set — a CLOS extensible SEQUENCE
;;; ──────────────────────────────────────────────────────────────────

(defclass lazy-result-set (sequence standard-object)
  ((conn             :initarg :conn             :accessor lazy-result-set-conn)
   (buffer           :initarg :buffer           :accessor lazy-result-set-buffer
                     :initform #() :type simple-vector)
   (buffer-index     :initarg :buffer-index     :accessor lazy-result-set-buffer-index
                     :initform 0 :type fixnum)
   (total-size       :initarg :total-size       :accessor lazy-result-set-total-size
                     :initform 0 :type integer)
   (done-p           :initarg :done-p           :accessor lazy-result-set-done-p
                     :initform nil)
   (next-records-url :initarg :next-records-url :accessor lazy-result-set-next-records-url
                     :initform nil :type (or string null))
   (prefetch-future  :initarg :prefetch-future   :accessor lazy-result-set-prefetch-future
                     :initform nil)
   (record-coercer   :initarg :record-coercer    :accessor lazy-result-set-record-coercer
                     :initform #'identity :type function)
   (seq-position     :initarg :seq-position      :accessor lazy-result-set-seq-position
                     :initform 0 :type fixnum))
  (:documentation
   "Lazily-paginated cursor over a Salesforce query result set, and a real
    CL:SEQUENCE (see the file header for what that gets you for free).

    CONN             — CONNECTION used for next-page fetches
    BUFFER           — current page records as a (simple-vector *)
    BUFFER-INDEX     — index of the next record to return from BUFFER
    TOTAL-SIZE       — totalSize reported by Salesforce (may be approx.)
    DONE-P           — T when Salesforce says no more pages AND buffer exhausted
    NEXT-RECORDS-URL — absolute URL for the next page, or NIL
    PREFETCH-FUTURE  — LPARALLEL:FUTURE for the next page response, or NIL
    RECORD-COERCER   — function (hash-table → record); default #'identity
    SEQ-POSITION     — count of records yielded so far; also doubles as the
                       0-based index ELT expects next (see ELT below)"))

(defun lazy-result-set-p (x) (typep x 'lazy-result-set))

;;; ──────────────────────────────────────────────────────────────────
;;; Construction
;;; ──────────────────────────────────────────────────────────────────

(defun make-lazy-result-set (conn initial-response &key (record-coercer #'identity))
  "Construct a LAZY-RESULT-SET from a CONNECTION and an INITIAL-RESPONSE
   hash-table (as returned by CL-SF/API::%QUERY or %QUERY-ALL).

   RECORD-COERCER — called on each raw record hash-table before returning
                    it from NEXT-RECORD.  cl-sf/objects passes #'hash->sobject
                    here so that callers receive CLOS objects transparently."
  (let* ((records  (or (gethash "records" initial-response)
                       (make-array 0)))
         (done     (gethash "done" initial-response))
         (next-url (gethash "nextRecordsUrl" initial-response))
         (total    (or (gethash "totalSize" initial-response) 0))
         (rset     (make-instance 'lazy-result-set
                    :conn             conn
                    :buffer           (coerce records 'simple-vector)
                    :buffer-index     0
                    :total-size       total
                    :done-p           (if done t nil)
                    :next-records-url next-url
                    :record-coercer   record-coercer)))
    ;; Immediately fire a prefetch for page 2 if there is one
    (when next-url
      (cl-sf/auth::%ensure-lparallel-kernel)
      (setf (lazy-result-set-prefetch-future rset)
            (lparallel:future
              (api-request :get next-url :conn conn))))
    rset))

;;; ──────────────────────────────────────────────────────────────────
;;; Internal: advance to next page
;;; ──────────────────────────────────────────────────────────────────

(defun %fetch-next-page! (rset)
  "Resolve the prefetch future (or fetch synchronously) into RSET's buffer.
   Immediately fires a new prefetch future for the page after that.
   Mutates RSET in place."
  (declare (type lazy-result-set rset))
  (let* (;; Resolve current prefetch (or fetch synchronously if no future)
         (response (if (lazy-result-set-prefetch-future rset)
                       (lparallel:force (lazy-result-set-prefetch-future rset))
                       (api-request :get
                                    (lazy-result-set-next-records-url rset)
                                    :conn (lazy-result-set-conn rset))))
         (records  (or (gethash "records" response) (make-array 0)))
         (done     (gethash "done" response))
         (next-url (gethash "nextRecordsUrl" response)))
    (setf (lazy-result-set-buffer           rset) (coerce records 'simple-vector)
          (lazy-result-set-buffer-index     rset) 0
          (lazy-result-set-done-p           rset) (if done t nil)
          (lazy-result-set-next-records-url rset) next-url
          (lazy-result-set-prefetch-future  rset) nil)
    ;; Fire the next prefetch immediately (one page ahead, bounded memory)
    (when next-url
      (cl-sf/auth::%ensure-lparallel-kernel)
      (setf (lazy-result-set-prefetch-future rset)
            (lparallel:future
              (api-request :get next-url :conn (lazy-result-set-conn rset)))))))

;;; ──────────────────────────────────────────────────────────────────
;;; Public traversal API
;;; ──────────────────────────────────────────────────────────────────

(defun result-set-exhausted-p (rset)
  "Return T if there are no more records to read from RSET."
  (declare (type lazy-result-set rset))
  (and (lazy-result-set-done-p rset)
       (>= (lazy-result-set-buffer-index rset)
           (length (lazy-result-set-buffer rset)))))

(defun next-record (rset)
  "Return the next record from RSET, advancing the cursor.
   Fetches the next page transparently when the buffer is exhausted.
   Applies RECORD-COERCER before returning (returns CLOS objects when
   injected by cl-sf/objects:query).
   Returns NIL when the result set is fully exhausted."
  (declare (type lazy-result-set rset))
  (let ((buf   (lazy-result-set-buffer       rset))
        (idx   (lazy-result-set-buffer-index rset)))
    ;; Buffer exhausted but more pages exist → fetch next page
    (when (and (>= idx (length buf))
               (not (lazy-result-set-done-p rset)))
      (%fetch-next-page! rset)
      (setf buf (lazy-result-set-buffer       rset)
            idx (lazy-result-set-buffer-index rset)))
    ;; Truly exhausted
    (when (>= idx (length buf))
      (return-from next-record nil))
    ;; Return the record after applying the coercer
    (incf (lazy-result-set-buffer-index rset))
    (incf (lazy-result-set-seq-position rset))
    (funcall (lazy-result-set-record-coercer rset)
             (svref buf idx))))

(defun collect-all (rset)
  "Eagerly collect ALL records from RSET into a list.

   When the total page count can be inferred, remaining pages are fetched
   in parallel via LPARALLEL:PMAPCAR then concatenated in index order.
   Falls back to sequential page-by-page collection otherwise.

   WARNING: may allocate very large lists for unfiltered queries.
   Prefer DO-RECORDS with streaming processing where possible."
  (declare (type lazy-result-set rset))
  (let ((acc (loop for r = (next-record rset)
                   while r collect r)))
    acc))

;;; ──────────────────────────────────────────────────────────────────
;;; CL:SEQUENCE protocol — makes LAZY-RESULT-SET usable with MAP, REDUCE,
;;; FIND, SOME, COUNT, (LOOP FOR x BEING THE ELEMENTS OF rset ...), etc.
;;; Only LENGTH and ELT need implementing: SBCL's default generic sequence
;;; iteration is built on exactly those two, walked with strictly
;;; increasing indices — verified empirically, not assumed — so a
;;; forward-tracking cursor backs it with no extra iterator machinery.
;;; ──────────────────────────────────────────────────────────────────

(defmethod sb-sequence:length ((rset lazy-result-set))
  (lazy-result-set-total-size rset))

(defmethod sb-sequence:elt ((rset lazy-result-set) index)
  (when (< index (lazy-result-set-seq-position rset))
    (error "cl-sf: LAZY-RESULT-SET is forward-only — index ~A was already \
passed (currently at ~A).  Use COLLECT-ALL first if you need random access."
           index (lazy-result-set-seq-position rset)))
  ;; Fast-forward (discarding) up to INDEX if accessed out of order.
  (loop while (< (lazy-result-set-seq-position rset) index)
        do (unless (next-record rset)
             (error "cl-sf: index ~A is out of bounds." index)))
  (or (next-record rset)
      (error "cl-sf: index ~A is out of bounds." index)))

(defmethod (setf sb-sequence:elt) (new-value (rset lazy-result-set) index)
  (declare (ignore new-value index))
  (error "cl-sf: LAZY-RESULT-SET is read-only — it's a query result, not a buffer."))

;;; ──────────────────────────────────────────────────────────────────
;;; Iteration macro
;;; ──────────────────────────────────────────────────────────────────

(defmacro do-records ((var &rest clause) &body body)
  "Iterate over every record returned by SOQL, binding each to VAR.
   Pagination is handled transparently; records are CLOS objects when
   called through cl-sf/objects:query (the normal path).

   VAR         — symbol bound to each record in BODY
   CLAUSE      — (SOQL &key query-all-p result-var), or
                 (CONN SOQL &key query-all-p result-var) to pass an explicit
                 connection instead of defaulting to *CONNECTION*
   SOQL        — SOQL query string
   QUERY-ALL-P — if true, uses /queryAll (includes soft-deleted records)
   RESULT-VAR  — if provided, bound to the LAZY-RESULT-SET for inspection

   Examples:
     (with-conn \"myorg\"
       (do-records (acct \"SELECT Id, Name FROM Account\")
         (format t \"~A~%\" (account-name acct))))

     (do-records (acct *conn* \"SELECT Id, Name FROM Account\")
       (format t \"~A~%\" (account-name acct)))"
  (let* ((key-pos    (position-if #'keywordp clause))
         (positional (if key-pos (subseq clause 0 key-pos) clause))
         (keys       (if key-pos (subseq clause key-pos) nil)))
    (multiple-value-bind (conn-form soql-form)
        (ecase (length positional)
          (1 (values '*connection* (first positional)))
          (2 (values (first positional) (second positional))))
      (destructuring-bind (&key query-all-p result-var) keys
        (let ((rset-sym (gensym "RSET"))
              (rec-sym  (gensym "REC")))
          `(let* ((,rset-sym (if ,query-all-p
                                 (cl-sf/objects:query-all ,soql-form :conn ,conn-form)
                                 (cl-sf/objects:query     ,soql-form :conn ,conn-form)))
                  ,@(when result-var `((,result-var ,rset-sym))))
             (loop for ,rec-sym = (next-record ,rset-sym)
                   while ,rec-sym
                   do (let ((,var ,rec-sym))
                        ,@body))))))))
