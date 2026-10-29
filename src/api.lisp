;;;; cl-sf/src/api.lisp
;;;; Raw Salesforce REST API HTTP layer.
;;;;
;;;; Architecture note:
;;;;   This file is the bottom of the stack — it speaks HTTP and jzon.
;;;;   ALL %-prefixed functions return raw jzon data (hash-tables / vectors /
;;;;   primitives) and are package-internal.  cl-sf/objects wraps them with
;;;;   CLOS conversion and owns the public API surface.
;;;;
;;;;   The only exported symbol is API-REQUEST — an authenticated raw HTTP
;;;;   escape hatch for callers who need something the CLOS layer doesn't cover.

(in-package #:cl-sf/api)

;;; ──────────────────────────────────────────────────────────────────
;;; Conditions
;;; ──────────────────────────────────────────────────────────────────

(define-condition sf-api-error (error)
  ((message     :initarg :message     :reader sf-api-error-message     :initform "Unknown API error")
   (http-status :initarg :http-status :reader sf-api-error-http-status :initform nil)
   (error-code  :initarg :error-code  :reader sf-api-error-code        :initform nil)
   (fields      :initarg :fields      :reader sf-api-error-fields      :initform nil))
  (:report (lambda (c s)
             (format s "Salesforce API error~@[ (HTTP ~A)~]~@[ [~A]~]: ~A~@[~%  Fields: ~{~A~^, ~}~]"
                     (sf-api-error-http-status c)
                     (sf-api-error-code c)
                     (sf-api-error-message c)
                     (sf-api-error-fields c)))))

;;; ──────────────────────────────────────────────────────────────────
;;; Internal helpers
;;; ──────────────────────────────────────────────────────────────────

(define-condition sf-dml-error (sf-api-error)
  ((failures  :initarg :failures  :reader sf-dml-error-failures :initform nil
              :documentation "List of (SOBJECT . error-string), one per failed record.")
   (succeeded :initarg :succeeded :reader sf-dml-error-succeeded :initform 0
              :documentation "How many records of the call did succeed (and are committed)."))
  (:documentation
   "A DATABASE-* call finished, but some records failed.  Records that
    succeeded stay committed; FAILURES says which did not and why."))

(defun %absolute-url-p (path)
  "Return T if PATH is an absolute URL (starts with http:// or https://)."
  (or (and (>= (length path) 7)  (string= path "http://"  :end1 7))
      (and (>= (length path) 8)  (string= path "https://" :end1 8))))

(defun %build-url (conn path &optional params)
  "Construct a full REST API URL.
   PATH is either an absolute URL (used as-is) or a relative path under
   /services/data/<version>/ (prepended automatically).
   PARAMS is an optional alist of (string . string) query parameters."
  (let ((base (if (%absolute-url-p path)
                  path
                  (format nil "~A/services/data/~A/~A"
                          (connection-instance-url conn)
                          (connection-api-version  conn)
                          path))))
    (if (null params)
        base
        ;; Append query string via quri so values are percent-encoded
        (let ((uri (quri:uri base)))
          (setf (quri:uri-query-params uri)
                (append (quri:uri-query-params uri)
                        (mapcar (lambda (p)
                                  (cons (car p) (princ-to-string (cdr p))))
                                params)))
          (quri:render-uri uri)))))

(defun %auth-headers (conn)
  "Return an alist of HTTP headers for an authenticated request."
  `(("Authorization" . ,(format nil "Bearer ~A" (connection-access-token conn)))
    ("Accept"        . "application/json")))

(defun %parse-sf-errors (body)
  "Parse a Salesforce error response body.
   Returns (values message error-code fields) from the first error object.
   Salesforce returns either [{message,errorCode,fields}] or {message,errorCode}."
  (handler-case
      (let* ((parsed (com.inuoe.jzon:parse body))
             (first  (etypecase parsed
                       (vector     (if (plusp (length parsed)) (aref parsed 0) nil))
                       (hash-table parsed))))
        (if first
            (values (gethash "message"   first)
                    (gethash "errorCode" first)
                    (let ((f (gethash "fields" first)))
                      (when (and f (vectorp f)) (coerce f 'list))))
            (values nil nil nil)))
    (error () (values nil nil nil))))

(defun %signal-api-error (body status)
  "Parse BODY as a Salesforce error response and signal SF-API-ERROR."
  (multiple-value-bind (message code fields) (%parse-sf-errors body)
    (error 'sf-api-error
           :http-status status
           :error-code  code
           :fields      fields
           :message     (or message (format nil "HTTP ~A" status)))))

;;; ──────────────────────────────────────────────────────────────────
;;; api-request — the core engine (exported)
;;; ──────────────────────────────────────────────────────────────────

(defun api-request (method path &key (conn *connection*) body params
                                      (content-type "application/json")
                                      raw)
  "Make an authenticated request to the Salesforce REST API.

   CONN         — a CL-SF/AUTH:CONNECTION; defaults to *CONNECTION* (see WITH-CONN)
   METHOD       — :GET :POST :PATCH :DELETE or :PUT
   PATH         — relative path (e.g. \"sobjects/Account\") or absolute URL
   BODY         — optional body to send.  If a string, sent as-is (e.g. CSV).
                  Any other Lisp object is serialized with jzon:stringify.
   PARAMS       — optional alist of (string . value) query parameters
   CONTENT-TYPE — HTTP Content-Type header for the body (default \"application/json\").
                  Pass \"text/csv\" for Bulk 2.0 data upload.
   RAW          — if T, return the raw response body string without JSON parsing.
                  Use for endpoints that return CSV (Bulk 2.0 result downloads).

   Behaviour:
     • On HTTP 401: refreshes the access token and retries exactly once.
     • On HTTP 204 / empty body: returns T.
     • On 2xx with body + RAW nil: returns parsed jzon data.
     • On 2xx with body + RAW T:  returns the raw body string.
     • On 4xx/5xx: signals SF-API-ERROR with parsed Salesforce error details.

   This is the raw escape hatch.  Normal usage goes through the CLOS wrappers
   in cl-sf/objects (select, sobject-get, database-insert, …)."
  (unless conn
    (error 'sf-auth-error
           :message "No connection — pass :conn explicitly or wrap the call in WITH-CONN."))
  (flet ((attempt ()
           "Execute one HTTP request, returning (cons body status)."
           (let* ((url     (%build-url conn path params))
                  ;; String bodies (CSV) pass through; everything else → JSON
                  (content (when body
                             (if (stringp body)
                                 body
                                 (com.inuoe.jzon:stringify body))))
                  (headers (if content
                               (cons (cons "Content-Type" content-type)
                                     (%auth-headers conn))
                               (%auth-headers conn))))
             (multiple-value-bind (resp-body status)
                 (handler-case
                     (dex:request url
                                  :method  method
                                  :headers headers
                                  :content content)
                   ;; dexador signals this on 4xx/5xx; we capture it to
                   ;; handle 401 (refresh + retry) vs other errors uniformly
                   (dex:http-request-failed (e)
                     (values (dex:response-body e) (dex:response-status e))))
               (cons resp-body status)))))
    (let* ((used    (connection-access-token conn))
           (result  (attempt))
           ;; On 401: refresh token (once, even if many threads hit this
           ;; together — see REFRESH-TOKEN's IF-STALE) and try once more
           (result  (if (/= (cdr result) 401)
                        result
                        (progn (refresh-token :conn conn :if-stale used) (attempt))))
           (body    (car result))
           (status  (cdr result)))
      (cond
        ;; Success — no body (DELETE, successful PATCH/PUT)
        ((or (= status 204)
             (and (>= status 200) (< status 300)
                  (or (null body) (zerop (length body)))))
         t)
        ;; Success — return raw string or parse as JSON
        ((and (>= status 200) (< status 300))
         (if raw
             (if (typep body '(array (unsigned-byte 8) (*)))
                 (babel:octets-to-string body :encoding :utf-8)
                 body)
             (com.inuoe.jzon:parse body)))
        ;; Error
        (t
         (%signal-api-error body status))))))

;;; ──────────────────────────────────────────────────────────────────
;;; Internal SOQL functions
;;; ──────────────────────────────────────────────────────────────────

(defun %query (conn soql)
  "GET /query?q=SOQL — returns the raw jzon response hash-table.
   %QUERY-RECORDS is the paginating wrapper SELECT uses."
  (api-request :get "query" :params `(("q" . ,soql)) :conn conn))

(defun %query-all (conn soql)
  "GET /queryAll?q=SOQL — includes soft-deleted records."
  (api-request :get "queryAll" :params `(("q" . ,soql)) :conn conn))

(defun %query-next-page (conn next-records-url)
  "Fetch the next page using the absolute NEXT-RECORDS-URL from a prior response."
  (api-request :get next-records-url :conn conn))

(defun %query-records (conn soql &key all (coercer #'identity))
  "Run SOQL (via /queryAll when ALL) and follow nextRecordsUrl until the
   result set is exhausted.  Returns a SIMPLE-VECTOR of (funcall COERCER
   record) — pages are collected first, then copied once into an exactly
   sized vector.  Pages are chained by cursor URL, so they fetch serially."
  (let ((resp  (if all (%query-all conn soql) (%query conn soql)))
        (pages '())
        (total 0))
    (loop
      (let ((records (or (gethash "records" resp) #())))
        (push records pages)
        (incf total (length records)))
      (let ((next (gethash "nextRecordsUrl" resp)))
        (unless (stringp next) (return))
        (setf resp (%query-next-page conn next))))
    (let ((out (make-array total)) (i 0))
      (dolist (records (nreverse pages) out)
        (map nil (lambda (r) (setf (svref out i) (funcall coercer r)) (incf i))
             records)))))

(defparameter *collection-max-records* 200
  "Salesforce's limit on records per sObject Collections request.")

(defun %collection-body (records)
  (let ((h (make-hash-table :test #'equal)))
    (setf (gethash "allOrNone" h) nil
          (gethash "records" h) (coerce records 'vector))
    h))

(defun %collection-create (conn records)
  "POST /composite/sobjects — insert up to 200 RECORDS (hash-tables that
   each carry \"attributes\": {\"type\": ...}).  Returns a vector with one
   result hash-table per record, in order: {\"id\", \"success\", \"errors\"}."
  (api-request :post "composite/sobjects" :conn conn :body (%collection-body records)))

(defun %collection-update (conn records)
  "PATCH /composite/sobjects — update up to 200 RECORDS (each with an Id)."
  (api-request :patch "composite/sobjects" :conn conn :body (%collection-body records)))

(defun %collection-upsert (conn type external-id-field records)
  "PATCH /composite/sobjects/TYPE/FIELD — upsert up to 200 RECORDS of one TYPE."
  (api-request :patch (format nil "composite/sobjects/~A/~A" type external-id-field)
               :conn conn :body (%collection-body records)))

(defun %collection-delete (conn ids)
  "DELETE /composite/sobjects?ids=... — delete up to 200 records by Id."
  (api-request :delete "composite/sobjects" :conn conn
               :params `(("ids" . ,(format nil "~{~A~^,~}" ids))
                         ("allOrNone" . "false"))))

;;; ──────────────────────────────────────────────────────────────────
;;; Internal sObject CRUD functions
;;; ──────────────────────────────────────────────────────────────────

(defun %sobject-get (conn type id &key fields)
  "GET /sobjects/TYPE/ID — returns record as a hash-table.
   FIELDS is an optional list of field name strings to fetch."
  (api-request :get
               (format nil "sobjects/~A/~A" type id)
               :params (when fields
                         `(("fields" . ,(format nil "~{~A~^,~}" fields))))
               :conn conn))

(defun %sobject-create (conn type fields-hash)
  "POST /sobjects/TYPE — create a record from FIELDS-HASH.
   Returns a hash-table with keys \"id\", \"success\", \"errors\"."
  (api-request :post
               (format nil "sobjects/~A" type)
               :body fields-hash
               :conn conn))

(defun %sobject-update (conn type id fields-hash)
  "PATCH /sobjects/TYPE/ID — update a record.  Returns T on success."
  (api-request :patch
               (format nil "sobjects/~A/~A" type id)
               :body fields-hash
               :conn conn))

(defun %sobject-delete (conn type id)
  "DELETE /sobjects/TYPE/ID.  Returns T on success."
  (api-request :delete
               (format nil "sobjects/~A/~A" type id)
               :conn conn))

(defun %sobject-upsert (conn type external-id-field external-id-value fields-hash)
  "PATCH /sobjects/TYPE/EXT-FIELD/EXT-VALUE — upsert a record.
   Returns a hash-table with keys \"id\", \"success\", \"created\" (on insert),
   or T (on update with no body)."
  (api-request :patch
               (format nil "sobjects/~A/~A/~A"
                       type external-id-field external-id-value)
               :body fields-hash
               :conn conn))

;;; ──────────────────────────────────────────────────────────────────
;;; Internal metadata / utility functions
;;; ──────────────────────────────────────────────────────────────────

(defun %describe-global (conn)
  "GET /sobjects — returns metadata for all sObject types in the org."
  (api-request :get "sobjects" :conn conn))

(defun %describe-sobject (conn type)
  "GET /sobjects/TYPE/describe — returns full field/relationship metadata."
  (api-request :get (format nil "sobjects/~A/describe" type) :conn conn))

(defun %limits (conn)
  "GET /limits — returns org API limits hash-table."
  (api-request :get "limits" :conn conn))
