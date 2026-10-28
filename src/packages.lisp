;;;; cl-sf/src/packages.lisp
;;;; Package definitions for the cl-sf Salesforce client library.
;;;;
;;;; Dependency / load order:
;;;;   packages → auth → api → objects → bulk → cl-sf
;;;;
;;;; Layer responsibilities:
;;;;   cl-sf/auth       — OAuth flows, CONNECTION struct
;;;;   cl-sf/api        — Raw HTTP layer, returns jzon hash-tables.
;;;;                      Internal %-prefixed functions only; api-request is
;;;;                      the sole public escape hatch.
;;;;   cl-sf/objects    — PUBLIC API: CLOS sobject classes, conversions,
;;;;                      and wrappers over cl-sf/api that return/accept
;;;;                      CLOS objects instead of raw hash-tables.
;;;;   cl-sf/bulk       — Bulk 2.0 operations (uses api + objects layers).
;;;;   cl-sf            — User-facing re-export of cl-sf/objects + auth.

;;; ──────────────────────────────────────────────────────────────────
;;; cl-sf/auth
;;; ──────────────────────────────────────────────────────────────────

(defpackage #:cl-sf/auth
  (:use #:cl)
  (:documentation
   "Salesforce authentication subsystem.
    Supports OAuth 2.0 flows without shelling out to the sf CLI.")
  (:export
   ;; Conditions
   #:sf-auth-error
   #:sf-auth-error-message
   #:sf-token-error
   #:sf-token-error-http-status
   #:sf-token-error-code
   #:sf-token-error-description
   ;; Connection struct + accessors
   #:connection
   #:make-connection
   #:connection-p
   #:connection-instance-url
   #:connection-login-url
   #:connection-access-token
   #:connection-refresh-token
   #:connection-client-id
   #:connection-client-secret
   #:connection-api-version
   #:connection-expires-at
   #:connection-username
   ;; Configuration
   #:*token-expiry-buffer*
   #:*sf-dir*
   #:*sfdx-dir*
   #:*lparallel-worker-count*
   ;; Auth flows
   #:connect/jwt
   #:connect/web-server
   #:connect/web-server-callback
   #:connect/username-password
   #:connect/sfdx-url
   #:connect/cached-credentials
   ;; Token management
   #:refresh-token
   #:token-valid-p
   ;; Dynamic connection ergonomics
   #:*connection*
   #:with-conn))

;;; ──────────────────────────────────────────────────────────────────
;;; cl-sf/api  — internal HTTP layer
;;; ──────────────────────────────────────────────────────────────────

(defpackage #:cl-sf/api
  (:use #:cl #:cl-sf/auth)
  (:documentation
   "Raw Salesforce REST API HTTP layer.
    All sObject / SOQL functions are %-prefixed and not exported — they
    return plain jzon hash-tables and are only called from cl-sf/objects.
    The sole exported symbol is API-REQUEST (the escape hatch for callers
    who need a raw request not covered by the CLOS layer).")
  (:export
   ;; Error condition
   #:sf-api-error
   #:sf-api-error-message
   #:sf-api-error-http-status
   #:sf-api-error-code
   #:sf-api-error-fields
   ;; Raw escape hatch
   #:api-request))

;;; ──────────────────────────────────────────────────────────────────
;;; cl-sf/objects  — PUBLIC API (CLOS layer on top of cl-sf/api)
;;; ──────────────────────────────────────────────────────────────────

(defpackage #:cl-sf/objects
  (:use #:cl #:cl-sf/auth #:cl-sf/api)
  (:documentation
   "Public Salesforce API surface.
    CLOS sObject classes + conversions + wrappers over the raw HTTP layer.
    All record-returning functions return SOBJECT instances; all
    record-accepting functions accept SOBJECT instances and serialize
    them to JSON transparently.")
  (:export
   ;; Base class
   #:sobject
   #:sobject-id
   #:sobject-type
   #:sobject-raw
   #:sobject-field-names
   ;; Class registry
   #:*sobject-classes*
   #:*sobject-slot-maps*
   ;; Conversion
   #:hash->sobject
   #:sobject->hash-table
   ;; Class definition macro
   #:define-sobject-class
   ;; Dynamic class generation
   #:load-sobject-classes
   #:load-sobject-classes/openapi
   ;; SOQL
   #:soql
   #:select
   ;; sObject CRUD (accept/return SOBJECT instances)
   #:sobject-get
   #:sobject-create
   #:sobject-update
   #:sobject-delete
   #:sobject-upsert
   #:sobject-create-many
   #:sobject-update-many
   #:sobject-delete-many
   #:sobject-upsert-many
   #:*sf-read-only-fields*
   ;; Client-side join (SOQL has no arbitrary JOIN)
   #:sobject-join
   ;; Metadata (return raw hash-tables — structural, not record data)
   #:describe-global
   #:describe-sobject
   ;; Org limits
   #:limits))

;;; ──────────────────────────────────────────────────────────────────
;;; cl-sf/bulk
;;; ──────────────────────────────────────────────────────────────────

(defpackage #:cl-sf/bulk
  (:use #:cl #:cl-sf/auth #:cl-sf/api #:cl-sf/objects)
  (:documentation
   "Salesforce Bulk 2.0 API.
    Accepts and returns SOBJECT instances; auto-routes large DML
    operations through Bulk 2.0 when count > *BULK-THRESHOLD*.")
  (:export
   ;; Conditions
   #:bulk-error
   #:bulk-error-message
   #:bulk-timeout-error
   #:bulk-timeout-error-job-id
   #:bulk-timeout-error-timeout
   #:bulk-job-failed-error
   #:bulk-job-failed-error-job-id
   #:bulk-job-failed-error-status
   ;; Result struct
   #:bulk-result
   #:make-bulk-result
   #:bulk-result-p
   #:bulk-result-successful
   #:bulk-result-failed
   #:bulk-result-unprocessed
   #:bulk-result-job-id
   ;; Configuration
   #:*bulk-threshold*
   #:*bulk-csv-chunk-size*
   ;; Job lifecycle
   #:create-bulk-job
   #:close-bulk-job
   #:abort-bulk-job
   #:get-bulk-job
   #:upload-job-data
   ;; Results
   #:get-successful-results
   #:get-failed-results
   #:get-unprocessed-results
   ;; High-level CLOS-first operations
   #:bulk-insert
   #:bulk-update
   #:bulk-upsert
   #:bulk-delete))

;;; ──────────────────────────────────────────────────────────────────
;;; cl-sf  — user-facing re-export package
;;; ──────────────────────────────────────────────────────────────────

(defpackage #:cl-sf
  (:use #:cl)
  (:documentation
   "cl-sf — A Common Lisp client for the Salesforce REST and Bulk APIs.

    Quick start:
      (ql:quickload :cl-sf)
      ;; From sf CLI cache (easiest):
      (defvar *conn* (cl-sf:connect/cached-credentials))
      ;; Or JWT (headless):
      (defvar *conn* (cl-sf:connect/jwt :client-id \"...\"
                                        :key-file  #p\"server.key\"
                                        :username  \"me@example.com\"))
      ;; Query — returns a vector of SOBJECT instances:
      (cl-sf:with-conn *conn*
        (map nil (lambda (rec) (format t \"~A~%\" (account-name rec)))
             (cl-sf:select Id Name FROM Account LIMIT 5)))")
  ;; No :export here — cl-sf.lisp imports the real symbols from the
  ;; subsystem packages and exports those same (identical) symbol objects.
  ;; Pre-declaring exports here would intern distinct CL-SF::FOO symbols
  ;; that then conflict with the imported ones of the same name.
  )
