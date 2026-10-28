;;;; cl-sf/src/cl-sf.lisp
;;;; Top-level re-export shim.
;;;;
;;;; All real implementation lives in the subsystem packages.
;;;; This file exists solely to import their public symbols into #:cl-sf
;;;; so users only ever (use-package :cl-sf) or use the cl-sf: prefix.

(in-package #:cl-sf)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)

  ;; ── Auth ────────────────────────────────────────────────────────
  (cl:import '(cl-sf/auth:sf-auth-error
               cl-sf/auth:sf-auth-error-message
               cl-sf/auth:sf-token-error
               cl-sf/auth:sf-token-error-http-status
               cl-sf/auth:sf-token-error-code
               cl-sf/auth:sf-token-error-description
               cl-sf/auth:connection
               cl-sf/auth:connection-instance-url
               cl-sf/auth:connection-login-url
               cl-sf/auth:connection-access-token
               cl-sf/auth:connection-refresh-token
               cl-sf/auth:connection-client-id
               cl-sf/auth:connection-client-secret
               cl-sf/auth:connection-api-version
               cl-sf/auth:connection-expires-at
               cl-sf/auth:connection-username
               cl-sf/auth:*token-expiry-buffer*
               cl-sf/auth:*sf-dir*
               cl-sf/auth:*sfdx-dir*
               cl-sf/auth:connect/jwt
               cl-sf/auth:connect/web-server
               cl-sf/auth:connect/web-server-callback
               cl-sf/auth:connect/username-password
               cl-sf/auth:connect/sfdx-url
               cl-sf/auth:connect/cached-credentials
               cl-sf/auth:refresh-token
               cl-sf/auth:token-valid-p
               cl-sf/auth:*connection*
               cl-sf/auth:with-conn)
             '#:cl-sf)

  ;; ── API (raw escape hatch + error condition only) ───────────────
  (cl:import '(cl-sf/api:sf-api-error
               cl-sf/api:sf-api-error-message
               cl-sf/api:sf-api-error-http-status
               cl-sf/api:sf-api-error-code
               cl-sf/api:sf-api-error-fields
               cl-sf/api:api-request)
             '#:cl-sf)

  ;; ── Objects / public API (CLOS layer) ───────────────────────────
  (cl:import '(cl-sf/objects:sobject
               cl-sf/objects:sobject-id
               cl-sf/objects:sobject-type
               cl-sf/objects:sobject-raw
               cl-sf/objects:sobject-field-names
               cl-sf/objects:*sobject-classes*
               cl-sf/objects:hash->sobject
               cl-sf/objects:sobject->hash-table
               cl-sf/objects:define-sobject-class
               cl-sf/objects:load-sobject-classes
               cl-sf/objects:load-sobject-classes/openapi
               cl-sf/objects:soql
               cl-sf/objects:select
               cl-sf/objects:sobject-get
               cl-sf/objects:database-insert
               cl-sf/objects:database-update
               cl-sf/objects:database-upsert
               cl-sf/objects:database-delete
               cl-sf/objects:sobject-join
               cl-sf/objects:describe-global
               cl-sf/objects:describe-sobject
               cl-sf/objects:limits)
             '#:cl-sf)

  ;; ── Bulk ────────────────────────────────────────────────────────
  (cl:import '(cl-sf/bulk:bulk-error
               cl-sf/bulk:bulk-error-message
               cl-sf/bulk:bulk-timeout-error
               cl-sf/bulk:bulk-job-failed-error
               cl-sf/bulk:bulk-result
               cl-sf/bulk:bulk-result-successful
               cl-sf/bulk:bulk-result-failed
               cl-sf/bulk:bulk-result-unprocessed
               cl-sf/bulk:bulk-result-job-id
               cl-sf/bulk:bulk-insert
               cl-sf/bulk:bulk-update
               cl-sf/bulk:bulk-upsert
               cl-sf/bulk:bulk-delete
               cl-sf/bulk:*bulk-threshold*
               cl-sf/bulk:*bulk-csv-chunk-size*)
             '#:cl-sf))

;;; This MUST be a separate top-level form from the IMPORT eval-when above:
;;; compile-file reads a whole top-level form before evaluating any of it,
;;; so bare symbol names here would otherwise get freshly interned into
;;; #:cl-sf by the reader before the imports above even run, conflicting
;;; with the symbols IMPORT is bringing in from the subsystem packages.
(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  ;; ── Export the public surface ─────────────────────────────────
  (cl:export '(sf-auth-error
               sf-token-error
               sf-api-error
               bulk-error
               bulk-timeout-error
               bulk-job-failed-error
               connection
               connection-username
               connect/jwt
               connect/web-server
               connect/web-server-callback
               connect/username-password
               connect/sfdx-url
               connect/cached-credentials
               refresh-token
               token-valid-p
               *token-expiry-buffer*
               *connection*
               with-conn
               api-request
               sobject
               sobject-id
               sobject-type
               sobject-raw
               sobject-field-names
               define-sobject-class
               load-sobject-classes
               load-sobject-classes/openapi
               hash->sobject
               sobject->hash-table
               soql
               select
               sobject-get
               database-insert
               database-update
               database-upsert
               database-delete
               sobject-join
               describe-global
               describe-sobject
               limits
               bulk-result
               bulk-result-successful
               bulk-result-failed
               bulk-result-unprocessed
               bulk-result-job-id
               bulk-insert
               bulk-update
               bulk-upsert
               bulk-delete
               *bulk-threshold*
               *bulk-csv-chunk-size*)
             '#:cl-sf))
