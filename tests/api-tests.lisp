;;;; cl-sf/tests/api-tests.lisp
;;;; Unit tests for cl-sf/api — pure functions, no HTTP calls.

(in-package #:cl-sf/tests)

(define-test api
  :parent cl-sf/tests
  :description "API layer — URL construction, error parsing, condition types.")

;;; ──────────────────────────────────────────────────────────────────
;;; %build-url
;;; ──────────────────────────────────────────────────────────────────

(defun make-test-conn (&key (instance "https://myorg.salesforce.com")
                             (version  "v62.0"))
  (cl-sf/auth:make-connection :instance-url instance :api-version version))

(define-test build-url/relative
  :parent api
  (let* ((conn (make-test-conn))
         (url  (cl-sf/api::%build-url conn "query")))
    (is string= "https://myorg.salesforce.com/services/data/v62.0/query" url)))

(define-test build-url/relative-with-subpath
  :parent api
  (let* ((conn (make-test-conn))
         (url  (cl-sf/api::%build-url conn "sobjects/Account/001")))
    (is string= "https://myorg.salesforce.com/services/data/v62.0/sobjects/Account/001"
        url)))

(define-test build-url/absolute-http
  :parent api
  (let* ((conn (make-test-conn))
         (url  (cl-sf/api::%build-url conn "http://other.salesforce.com/path")))
    (is string= "http://other.salesforce.com/path" url
        "Absolute http:// URL returned unchanged")))

(define-test build-url/absolute-https
  :parent api
  (let* ((conn (make-test-conn))
         (url  (cl-sf/api::%build-url conn "https://other.salesforce.com/path")))
    (is string= "https://other.salesforce.com/path" url
        "Absolute https:// URL returned unchanged")))

(define-test build-url/with-params
  :parent api
  (let* ((conn (make-test-conn))
         (url  (cl-sf/api::%build-url conn "query" '(("q" . "SELECT Id FROM Account")))))
    ;; URL must start with the base and contain the q param
    (true (cl-ppcre:scan "^https://myorg\\.salesforce\\.com/services/data/v62\\.0/query\\?" url)
          "URL has correct base and query separator")
    (true (cl-ppcre:scan "q=" url) "q param present")))

(define-test build-url/api-version-in-path
  :parent api
  (let* ((conn (make-test-conn :version "v55.0"))
         (url  (cl-sf/api::%build-url conn "limits")))
    (true (cl-ppcre:scan "v55\\.0" url) "API version reflected in URL")))

;;; ──────────────────────────────────────────────────────────────────
;;; %parse-sf-errors
;;; ──────────────────────────────────────────────────────────────────

(define-test parse-sf-errors/array-format
  :parent api
  (multiple-value-bind (msg code fields)
      (cl-sf/api::%parse-sf-errors
       "[{\"message\":\"Field required\",\"errorCode\":\"REQUIRED_FIELD_MISSING\",\"fields\":[\"Name\"]}]")
    (is string= "Field required"          msg)
    (is string= "REQUIRED_FIELD_MISSING"  code)
    (is equal   '("Name")                fields)))

(define-test parse-sf-errors/object-format
  :parent api
  (multiple-value-bind (msg code fields)
      (cl-sf/api::%parse-sf-errors
       "{\"message\":\"Session expired\",\"errorCode\":\"INVALID_SESSION_ID\"}")
    (is string= "Session expired"   msg)
    (is string= "INVALID_SESSION_ID" code)
    (false fields "no fields in object format")))

(define-test parse-sf-errors/empty-body
  :parent api
  (finish
   (multiple-value-bind (msg code fields)
       (cl-sf/api::%parse-sf-errors "")
     (false msg)
     (false code)
     (false fields)))
  "Empty body → all nils, no condition signaled")

(define-test parse-sf-errors/invalid-json
  :parent api
  (finish
   (cl-sf/api::%parse-sf-errors "not json at all {{"))
  "Invalid JSON → returns gracefully without error")

(define-test parse-sf-errors/empty-array
  :parent api
  (multiple-value-bind (msg code fields)
      (cl-sf/api::%parse-sf-errors "[]")
    (false msg)
    (false code)
    (false fields)))

(define-test parse-sf-errors/multiple-errors-uses-first
  :parent api
  (multiple-value-bind (msg code _fields)
      (cl-sf/api::%parse-sf-errors
       "[{\"message\":\"First\",\"errorCode\":\"FIRST\"},\
{\"message\":\"Second\",\"errorCode\":\"SECOND\"}]")
    (declare (ignore _fields))
    (is string= "First" msg  "First error's message used")
    (is string= "FIRST" code "First error's code used")))

;;; ──────────────────────────────────────────────────────────────────
;;; sf-api-error condition
;;; ──────────────────────────────────────────────────────────────────

(define-test sf-api-error/can-signal-and-catch
  :parent api
  (handler-case
      (error 'cl-sf/api:sf-api-error
             :message     "test error"
             :http-status 400
             :error-code  "TEST_CODE"
             :fields      '("Name" "Phone"))
    (cl-sf/api:sf-api-error (e)
      (is string= "test error" (cl-sf/api:sf-api-error-message e))
      (is =       400          (cl-sf/api:sf-api-error-http-status e))
      (is string= "TEST_CODE"  (cl-sf/api:sf-api-error-code e))
      (is equal   '("Name" "Phone") (cl-sf/api:sf-api-error-fields e)))))

(define-test sf-api-error/is-a-condition
  :parent api
  (true (subtypep 'cl-sf/api:sf-api-error 'error)
        "sf-api-error is a subtype of error"))
