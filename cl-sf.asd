(defsystem "cl-sf"
  :description "A Common Lisp client for the Salesforce REST and Bulk APIs."
  :version "0.1.0"
  :author "bscur"
  :license "MIT"
  :depends-on (#:com.inuoe.jzon  ; JSON parsing — fastest + most RFC-8259-compliant CL JSON library
               #:dexador          ; HTTP client
               #:alexandria       ; General utilities
               #:quri             ; URI construction/encoding
               #:local-time       ; OAuth token expiry timestamps
               #:cl-ppcre         ; Regex (SOQL/URL helpers)
               #:lparallel        ; Parallelism — bulk job polling, parallel describe calls, prefetch
               #:cl-cpus          ; Detect actual processor count for *lparallel-worker-count*
               #:cl-base64        ; Base64url encoding for JWT header/claims/signature
               #:ironclad         ; RSA-SHA256 signing for JWT Bearer flow
               #:babel)           ; Octet ↔ string conversion (used by :raw api-request path)
  :pathname "src"
  :serial t
  :components ((:file "packages")
               ;; TODO #1 — Authentication
               (:file "auth")
               ;; TODO #2 — Core REST API endpoints (internal % layer, returns raw jzon)
               (:file "api")
               ;; TODO #5 — CLOS sObject definitions + public API wrappers
               (:file "objects")
               ;; TODO #4 — Bulk 2.0 API support
               (:file "bulk")
               ;; Top-level package re-exporting the public API surface
               (:file "cl-sf"))
  :in-order-to ((test-op (test-op "cl-sf/tests"))))

(defsystem "cl-sf/tests"
  :description "Test suite for cl-sf."
  :depends-on (#:cl-sf
               #:parachute)
  :pathname "tests"
  :serial t
  :components ((:file "packages")   ; test package + root suite + helpers
               (:file "auth-tests")
               (:file "api-tests")
               (:file "bulk-tests")
               (:file "objects-tests"))
  :perform (test-op (op c)
             (uiop:symbol-call '#:parachute '#:test '#:cl-sf/tests)))
