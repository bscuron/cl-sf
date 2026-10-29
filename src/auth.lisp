;;;; cl-sf/src/auth.lisp
;;;; TODO #1 — Authentication
;;;;
;;;; Implements all Salesforce OAuth 2.0 flows without shelling out to the sf CLI:
;;;;   - connect/cached-credentials  read ~/.sf/credentials.json (sf CLI cache)
;;;;   - connect/sfdx-url            parse a sfdxAuthUrl string
;;;;   - connect/username-password   OAuth password flow (dev/sandbox only)
;;;;   - connect/jwt                 JWT Bearer Token (headless / CI)
;;;;   - connect/web-server          Authorization Code flow (interactive)
;;;;
;;;; All flows return a CONNECTION struct.  API calls auto-refresh tokens on 401.

#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-bsd-sockets))

(in-package #:cl-sf/auth)

;;; ──────────────────────────────────────────────────────────────────
;;; Conditions
;;; ──────────────────────────────────────────────────────────────────

(define-condition sf-auth-error (error)
  ((message :initarg :message :reader sf-auth-error-message))
  (:report (lambda (c s)
             (format s "cl-sf auth error: ~A" (sf-auth-error-message c)))))

(define-condition sf-token-error (sf-auth-error)
  ((http-status :initarg :http-status :reader sf-token-error-http-status :initform nil)
   (error-code  :initarg :error-code  :reader sf-token-error-code        :initform nil)
   (description :initarg :description :reader sf-token-error-description :initform nil))
  (:report (lambda (c s)
             (format s "Salesforce token error~@[ (HTTP ~A)~]~@[ [~A]~]: ~A"
                     (sf-token-error-http-status c)
                     (sf-token-error-code c)
                     (sf-auth-error-message c)))))

;;; ──────────────────────────────────────────────────────────────────
;;; Connection struct
;;; ──────────────────────────────────────────────────────────────────

(defstruct connection
  "An authenticated session with a Salesforce org.

   instance-url   — e.g. \"https://myorg.my.salesforce.com\"
   login-url      — e.g. \"https://login.salesforce.com\" (used for token refresh)
   access-token   — current Bearer token
   refresh-token  — OAuth refresh token (nil for JWT connections)
   client-id      — Connected App consumer key
   client-secret  — Connected App consumer secret (optional)
   api-version    — REST API version string, e.g. \"v62.0\"
   expires-at     — local-time:timestamp of token expiry, or nil if unknown
   username       — Salesforce username, set by CONNECT/CACHED-CREDENTIALS so
                    REFRESH-TOKEN knows which ~/.sf and ~/.sfdx files to
                    update after refreshing; nil for other auth flows"
  (instance-url  nil                          :type (or string null))
  (login-url     "https://login.salesforce.com" :type string)
  (access-token  nil                          :type (or string null))
  (refresh-token nil                          :type (or string null))
  (client-id     "PlatformCLI"               :type string)
  (client-secret nil                          :type (or string null))
  (api-version   "v62.0"                     :type string)
  (expires-at    nil)
  (username      nil                          :type (or string null)))

;;; ──────────────────────────────────────────────────────────────────
;;; Dynamic connection — lets callers omit :CONN entirely
;;; ──────────────────────────────────────────────────────────────────

(defvar *connection* nil
  "The CONNECTION used by default when a public cl-sf function's :CONN
   argument is omitted.  Bound dynamically by WITH-CONN (see bottom of
   this file for the macro, defined after CONNECT/CACHED-CREDENTIALS).")

;;; ──────────────────────────────────────────────────────────────────
;;; lparallel kernel — lazily initialized, never required of callers
;;; ──────────────────────────────────────────────────────────────────

(defparameter *lparallel-worker-count* (cl-cpus:get-number-of-processors)
  "Worker thread count for cl-sf's internal LPARALLEL:*KERNEL* (page
   parallel bulk-job polling in cl-sf/bulk,
   parallel DESCRIBE calls in LOAD-SOBJECT-CLASSES).  Defaults to the
   running user's actual processor count via CL-CPUS.  Rebind before the
   first call that needs parallelism to change it — these are
   network-bound tasks, not CPU-bound, so a higher count can still help.")

(defvar *kernel-lock* (sb-thread:make-mutex :name "cl-sf kernel creation"))

(defun %ensure-lparallel-kernel ()
  "Lazily create LPARALLEL:*KERNEL* on first use, so callers never need to
   know cl-sf uses lparallel internally.  Never overrides a kernel the
   host application already set up itself.  Safe to call from several
   threads at once."
  (unless lparallel:*kernel*
    (sb-thread:with-mutex (*kernel-lock*)
      (unless lparallel:*kernel*
        (setf lparallel:*kernel* (lparallel:make-kernel *lparallel-worker-count*))))))

;;; ──────────────────────────────────────────────────────────────────
;;; Token validity
;;; ──────────────────────────────────────────────────────────────────

(defparameter *token-expiry-buffer* 60
  "Seconds before the nominal expiry at which TOKEN-VALID-P returns NIL.
   Prevents using a token that expires mid-request.")

(defun token-valid-p (conn)
  "Return T if CONN has an access token that has not (nearly) expired."
  (declare (type connection conn))
  (and (connection-access-token conn)
       (let ((exp (connection-expires-at conn)))
         (or (null exp)   ; no expiry tracked → assume valid until 401
             (local-time:timestamp>
              exp
              (local-time:timestamp+ (local-time:now)
                                     *token-expiry-buffer* :sec))))))

;;; ──────────────────────────────────────────────────────────────────
;;; Internal — low-level HTTP + JSON helpers
;;; ──────────────────────────────────────────────────────────────────

(defun %token-endpoint (login-url)
  (format nil "~A/services/oauth2/token" login-url))

(defun %string->bytes (s)
  "Convert ASCII string S to a (simple-array (unsigned-byte 8) (*))."
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

(defun %base64url-encode (bytes)
  "Base64url-encode BYTES (usb8 vector) → string, no padding."
  (let ((b64 (cl-base64:usb8-array-to-base64-string bytes :uri nil)))
    ;; standard alphabet → URL-safe alphabet, strip = padding
    (cl-ppcre:regex-replace-all
     "=+$"
     (cl-ppcre:regex-replace-all
      "/" (cl-ppcre:regex-replace-all "\\+" b64 "-") "_")
     "")))

(defun %pkce-verifier ()
  "A fresh PKCE code verifier: 32 random bytes, base64url (43 characters)."
  (%base64url-encode (ironclad:random-data 32)))

(defun %pkce-challenge (verifier)
  "The S256 code challenge for VERIFIER: base64url(SHA-256(verifier))."
  (%base64url-encode
   (ironclad:digest-sequence :sha256 (babel:string-to-octets verifier :encoding :ascii))))

(defun %parse-token-response (body &key (api-version "v62.0")
                                        (login-url "https://login.salesforce.com")
                                        (client-id "PlatformCLI")
                                        client-secret)
  "Parse a Salesforce OAuth token endpoint JSON response.
   Returns a CONNECTION on success; signals SF-TOKEN-ERROR on a Salesforce error body."
  (let* ((data      (com.inuoe.jzon:parse body))
         (err       (gethash "error"             data))
         (err-desc  (gethash "error_description" data))
         (acc-tok   (gethash "access_token"      data))
         (inst-url  (gethash "instance_url"      data))
         (ref-tok   (gethash "refresh_token"     data))
         (issued-at (gethash "issued_at"         data)))  ; epoch ms, string
    (when err
      (error 'sf-token-error
             :message     (or err-desc err)
             :error-code  err
             :description err-desc))
    (unless acc-tok
      (error 'sf-auth-error
             :message "Token response missing access_token"))
    (make-connection
     :access-token  acc-tok
     :instance-url  (or inst-url "")
     :login-url     login-url
     :refresh-token ref-tok
     :client-id     client-id
     :client-secret client-secret
     :api-version   api-version
     ;; issued_at is epoch milliseconds as a string.  Salesforce access tokens
     ;; default to ~2 hours; we store issued_at + 115 min as a conservative bound.
     :expires-at    (when issued-at
                      (handler-case
                          (local-time:unix-to-timestamp
                           (+ (floor (parse-integer
                                      (etypecase issued-at
                                        (string  issued-at)
                                        (integer (write-to-string issued-at))))
                                     1000)
                              (* 115 60)))
                        (error () nil))))))

(defun %do-token-request (endpoint form-params
                          &key (api-version "v62.0")
                               (login-url   "https://login.salesforce.com")
                               client-id client-secret)
  "POST form-encoded FORM-PARAMS to ENDPOINT; return a CONNECTION.
   Signals SF-TOKEN-ERROR on any non-200 response."
  (multiple-value-bind (body status)
      (handler-case
          (dex:post endpoint
                    :content form-params
                    :headers '(("Accept" . "application/json")))
        ;; dexador raises this condition on 4xx/5xx
        (dex:http-request-failed (e)
          (values (dex:response-body e) (dex:response-status e))))
    (when (/= status 200)
      (let ((err-code nil) (err-desc nil))
        (handler-case
            (let ((d (com.inuoe.jzon:parse body)))
              (setf err-code (gethash "error"             d)
                    err-desc (gethash "error_description" d)))
          (error ()))
        (error 'sf-token-error
               :http-status status
               :error-code  err-code
               :message     (or err-desc
                                (format nil "HTTP ~A from ~A" status endpoint))
               :description err-desc)))
    (%parse-token-response body
                           :api-version   api-version
                           :login-url     login-url
                           :client-id     (or client-id "PlatformCLI")
                           :client-secret client-secret)))

;;; ──────────────────────────────────────────────────────────────────
;;; Token refresh (public)
;;; ──────────────────────────────────────────────────────────────────

(defvar *refresh-lock* (sb-thread:make-mutex :name "cl-sf token refresh")
  "Serializes token refreshes: concurrent requests that all get a 401 must
   not each spend (and possibly rotate) the same refresh token.
   ponytail: one global lock, not one per connection — refreshes are rare.")

(defun refresh-token (&key (conn *connection*) if-stale)
  "Exchange the refresh token in CONN for a fresh access token.
   Mutates CONN in place and returns it.  Thread-safe: refreshes are
   serialized, and if IF-STALE (the access token a failed request used) is
   no longer CONN's current token, another thread already refreshed it and
   this returns CONN without another round trip.  If CONN came from
   CONNECT/CACHED-CREDENTIALS (CONNECTION-USERNAME is set), also writes the
   refreshed tokens back to ~/.sf and ~/.sfdx so the real sf/sfdx CLI and
   other tools observe the same session.
   Signals SF-AUTH-ERROR for JWT connections (no refresh token)."
  (declare (type connection conn))
  (sb-thread:with-mutex (*refresh-lock*)
   (when (and if-stale (not (equal if-stale (connection-access-token conn))))
     (return-from refresh-token conn))
   (%refresh-token-locked conn)))

(defun %refresh-token-locked (conn)
  (unless (connection-refresh-token conn)
    (error 'sf-auth-error
           :message "Connection has no refresh token. \
JWT connections re-authenticate by calling CONNECT/JWT again."))
  (let* ((params `(("grant_type"    . "refresh_token")
                   ("client_id"     . ,(connection-client-id conn))
                   ("refresh_token" . ,(connection-refresh-token conn))
                   ,@(when (connection-client-secret conn)
                       `(("client_secret" . ,(connection-client-secret conn))))))
         (fresh  (%do-token-request
                  (%token-endpoint (connection-login-url conn))
                  params
                  :api-version   (connection-api-version conn)
                  :login-url     (connection-login-url conn)
                  :client-id     (connection-client-id conn)
                  :client-secret (connection-client-secret conn))))
    (setf (connection-access-token  conn) (connection-access-token  fresh)
          (connection-instance-url  conn) (or (connection-instance-url fresh)
                                              (connection-instance-url conn))
          (connection-expires-at    conn) (connection-expires-at    fresh)
          ;; Honour refresh token rotation: if a new token was issued, store it
          (connection-refresh-token conn) (or (connection-refresh-token fresh)
                                              (connection-refresh-token conn)))
    (%persist-refreshed-credentials conn)
    conn))

;;; ──────────────────────────────────────────────────────────────────
;;; connect/cached-credentials — real sf/sfdx CLI on-disk state
;;;
;;; Layout (verified against an actual sf/sfdx install, not assumed):
;;;   ~/.sf/config.json         — {"orgs": {alias: username}, "target-org": ...}
;;;   ~/.sf/orgs/<user>.json    — {username, orgId, instanceUrl, loginUrl,
;;;                                accessToken}  (plaintext; NO refresh token —
;;;                                the new sf CLI keeps that in the OS keychain)
;;;   ~/.sfdx/sfdx-config.json  — {"orgs": {alias: username}, "defaultusername"}
;;;   ~/.sfdx/alias.json        — {"orgs": {alias: username}}
;;;   ~/.sfdx/key.json          — {"key": <32-char string>} — local AES key,
;;;                                used only when the OS keychain is unavailable
;;;   ~/.sfdx/<user>.json       — legacy per-org file; accessToken/refreshToken
;;;                                are AES-256-GCM-encrypted with the key above.
;;;
;;; None of this shells out to sf/sfdx — it reads/writes the same files the
;;; CLI itself reads/writes.
;;; ──────────────────────────────────────────────────────────────────

(defparameter *sf-dir*
  (uiop:subpathname (user-homedir-pathname) ".sf/")
  "Root of the sf CLI's on-disk state.  Override for testing.")

(defparameter *sfdx-dir*
  (uiop:subpathname (user-homedir-pathname) ".sfdx/")
  "Root of the legacy sfdx CLI's on-disk state.  Override for testing.")

(defun %try-parse-json-file (path)
  "Parse PATH as JSON.  Returns NIL (never signals) if PATH is missing,
   unreadable, or not valid JSON — callers treat absence as 'not configured'."
  (handler-case (com.inuoe.jzon:parse (uiop:read-file-string path))
    (error () nil)))

(defun %alias-map (config-data)
  "Return the alias→username hash-table from a parsed sf/sfdx config or
   alias JSON file, or NIL if it has none."
  (and config-data (gethash "orgs" config-data)))

(defun %resolve-org-username (alias-or-username)
  "Resolve ALIAS-OR-USERNAME to a concrete Salesforce username.

   ALIAS-OR-USERNAME may be a CLI alias, a literal username, or NIL (meaning
   'use the CLI's configured default org').  Checks, in order: ~/.sf's
   config.json orgs map, ~/.sfdx's sfdx-config.json orgs map, ~/.sfdx's
   alias.json orgs map.  Falls back to the input itself (treated as already
   a literal username) if nothing matches."
  (let* ((sf-cfg     (%try-parse-json-file (uiop:subpathname *sf-dir* "config.json")))
         (sfdx-cfg   (%try-parse-json-file (uiop:subpathname *sfdx-dir* "sfdx-config.json")))
         (sfdx-alias (%try-parse-json-file (uiop:subpathname *sfdx-dir* "alias.json")))
         (target     (or alias-or-username
                        (and sf-cfg   (gethash "target-org"      sf-cfg))
                        (and sfdx-cfg (gethash "defaultusername" sfdx-cfg)))))
    (unless target
      (error 'sf-auth-error
             :message "No org alias/username given, and no default org is \
configured in ~/.sf/config.json (\"target-org\") or \
~/.sfdx/sfdx-config.json (\"defaultusername\")."))
    (or (and sf-cfg     (gethash target (%alias-map sf-cfg)))
        (and sfdx-cfg   (gethash target (%alias-map sfdx-cfg)))
        (and sfdx-alias (gethash target (%alias-map sfdx-alias)))
        target)))

;;; — sfdx file-fallback AES-256-GCM (matches sfdx-core's crypto.ts exactly:
;;;   both the key and the IV are used as their literal ASCII bytes, never
;;;   hex-decoded; only the ciphertext and the auth tag are hex.) ——————————

(defparameter +sfdx-gcm-iv-bytes+ 6
  "Byte count of the random IV sfdx generates (BYTE_COUNT_FOR_IV in crypto.ts).
   Represented as 12 hex characters, which — per the quirk above — are also
   the literal 12-byte IV fed to the cipher.")

(defun %sfdx-encryption-key ()
  "Read the local AES key from ~/.sfdx/key.json — the fallback sfdx/sf CLI
   use when the OS keychain is unavailable.  Signals SF-AUTH-ERROR if absent,
   meaning this org's secrets live in the OS keychain instead, which cl-sf
   does not read."
  (let ((data (%try-parse-json-file (uiop:subpathname *sfdx-dir* "key.json"))))
    (or (and data (gethash "key" data))
        (error 'sf-auth-error
               :message "No ~/.sfdx/key.json found — this org's refresh \
token is stored in the OS keychain, which cl-sf does not integrate with. \
Re-authenticate with CONNECT/JWT or CONNECT/SFDX-URL instead."))))

(defun %sfdx-decrypt (encrypted key-string)
  "Decrypt an sfdx/sf CLI secret field (format \"<iv><cipher>:<tag>\", all hex
   except that IV and KEY-STRING are consumed as literal ASCII bytes)."
  (when (or (null encrypted) (zerop (length encrypted)))
    (return-from %sfdx-decrypt nil))
  (let* ((colon      (or (position #\: encrypted)
                         (error 'sf-auth-error
                                :message "Malformed sfdx encrypted value (no ':')")))
         (head       (subseq encrypted 0 colon))
         (tag-hex    (subseq encrypted (1+ colon)))
         (iv-str     (subseq head 0 (* 2 +sfdx-gcm-iv-bytes+)))
         (secret-hex (subseq head (* 2 +sfdx-gcm-iv-bytes+)))
         (ae (ironclad:make-authenticated-encryption-mode
              :gcm :cipher-name :aes
              :key                   (%string->bytes key-string)
              :initialization-vector (%string->bytes iv-str)
              :tag                   (ironclad:hex-string-to-byte-array tag-hex))))
    (babel:octets-to-string
     (ironclad:decrypt-message ae (ironclad:hex-string-to-byte-array secret-hex))
     :encoding :utf-8)))

(defun %sfdx-encrypt (plaintext key-string)
  "Encrypt PLAINTEXT in the same format %SFDX-DECRYPT reads, with a fresh
   random IV, so a refreshed token can be written back into ~/.sfdx."
  (let* ((iv-str       (ironclad:byte-array-to-hex-string
                        (ironclad:random-data +sfdx-gcm-iv-bytes+)))
         (ae           (ironclad:make-authenticated-encryption-mode
                        :gcm :cipher-name :aes
                        :key                   (%string->bytes key-string)
                        :initialization-vector (%string->bytes iv-str)))
         (cipher-bytes (ironclad:encrypt-message
                        ae (babel:string-to-octets plaintext :encoding :utf-8)))
         (tag-hex      (ironclad:byte-array-to-hex-string (ironclad:produce-tag ae))))
    (format nil "~A~A:~A"
            iv-str (ironclad:byte-array-to-hex-string cipher-bytes) tag-hex)))

;;; — Per-org file loaders ——————————————————————————————————————————————

(defun %load-sf-org-file (username)
  "Read ~/.sf/orgs/<USERNAME>.json.  Returns a plist, or NIL if absent."
  (let ((data (%try-parse-json-file
               (uiop:subpathname *sf-dir* (format nil "orgs/~A.json" username)))))
    (when data
      (list :org-id       (gethash "orgId"       data)
            :instance-url (gethash "instanceUrl" data)
            :login-url    (gethash "loginUrl"    data)
            :access-token (gethash "accessToken" data)))))

(defun %load-sfdx-org-file (username)
  "Read ~/.sfdx/<USERNAME>.json, decrypting accessToken/refreshToken.
   Returns a plist, or NIL if absent."
  (let ((data (%try-parse-json-file
               (uiop:subpathname *sfdx-dir* (format nil "~A.json" username)))))
    (when data
      (let ((key (%sfdx-encryption-key)))
        (list :org-id        (gethash "orgId"       data)
              :instance-url  (gethash "instanceUrl" data)
              :login-url     (gethash "loginUrl"    data)
              :client-id     (gethash "clientId"    data)
              :access-token  (%sfdx-decrypt (gethash "accessToken"  data) key)
              :refresh-token (%sfdx-decrypt (gethash "refreshToken" data) key))))))

(defun %load-cached-org (username)
  "Merge ~/.sf and ~/.sfdx entries for USERNAME into one plist.
   Prefers ~/.sf's access-token/instance-url (the actively-maintained
   store); always takes refresh-token/client-id from ~/.sfdx, since the new
   sf CLI keeps those in the OS keychain rather than on disk.
   Signals SF-AUTH-ERROR if neither file exists."
  (let ((sf   (%load-sf-org-file   username))
        (sfdx (%load-sfdx-org-file username)))
    (unless (or sf sfdx)
      (error 'sf-auth-error
             :message (format nil "No cached credentials found for ~S in \
~~/.sf/orgs/ or ~~/.sfdx/.  Authenticate first with the sf/sfdx CLI \
(sf org login web), or use CONNECT/JWT." username)))
    (list :org-id        (or (getf sf :org-id)       (getf sfdx :org-id))
          :instance-url  (or (getf sf :instance-url) (getf sfdx :instance-url))
          :login-url     (or (getf sf :login-url)    (getf sfdx :login-url)
                             "https://login.salesforce.com")
          :access-token  (or (getf sf :access-token) (getf sfdx :access-token))
          :refresh-token (getf sfdx :refresh-token)
          :client-id     (or (getf sfdx :client-id) "PlatformCLI"))))

;;; — Writing refreshed tokens back (keeps the real CLI's state in sync) ——

(defun %write-json-file (path data)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :external-format :utf-8
                            :if-does-not-exist :create)
    (write-string (com.inuoe.jzon:stringify data) out)))

(defun %persist-refreshed-credentials (conn)
  "Write CONN's current access/refresh tokens back into ~/.sf/orgs/<user>.json
   and ~/.sfdx/<user>.json (whichever exist), exactly as the sf/sfdx CLI
   would after its own refresh, so other tools see the same session.
   Best-effort: a write failure is WARNed, not signalled — the in-memory
   CONN remains perfectly usable either way."
  (let ((username (connection-username conn)))
    (when username
      (let ((sf-path (uiop:subpathname *sf-dir* (format nil "orgs/~A.json" username))))
        (when (uiop:file-exists-p sf-path)
          (handler-case
              (let ((data (com.inuoe.jzon:parse (uiop:read-file-string sf-path))))
                (setf (gethash "accessToken" data) (connection-access-token conn))
                (%write-json-file sf-path data))
            (error (e) (warn "cl-sf: failed to update ~A: ~A" sf-path e)))))
      (let ((sfdx-path (uiop:subpathname *sfdx-dir* (format nil "~A.json" username))))
        (when (uiop:file-exists-p sfdx-path)
          (handler-case
              (let ((data (com.inuoe.jzon:parse (uiop:read-file-string sfdx-path)))
                    (key  (%sfdx-encryption-key)))
                (setf (gethash "accessToken" data)
                      (%sfdx-encrypt (connection-access-token conn) key))
                (when (connection-refresh-token conn)
                  (setf (gethash "refreshToken" data)
                        (%sfdx-encrypt (connection-refresh-token conn) key)))
                (%write-json-file sfdx-path data))
            (error (e) (warn "cl-sf: failed to update ~A: ~A" sfdx-path e))))))))

(defun connect/cached-credentials (&key username (api-version "v62.0") refresh)
  "Authenticate by reusing a session the sf/sfdx CLI already created —
   never shells out; reads the same ~/.sf and ~/.sfdx files the CLI does.

   USERNAME    — an sf/sfdx CLI alias, a literal username, or NIL to use
                 the CLI's configured default org.
   API-VERSION — defaults to v62.0.
   REFRESH     — if T, exchange the stored refresh token for a fresh access
                 token immediately (requires a ~/.sfdx entry for this org —
                 the new sf CLI alone does not expose a refresh token on
                 disk).  Default NIL: use the stored access token as-is;
                 cl-sf/api auto-refreshes on 401 anyway.

   Returns a CONNECTION.

   Quick start:
     ;; After `sf org login web --alias myorg`
     (cl-sf:connect/cached-credentials :username \"myorg\")"
  (let* ((resolved (%resolve-org-username username))
         (cred     (%load-cached-org resolved))
         (conn     (make-connection
                    :username      resolved
                    :access-token  (getf cred :access-token)
                    :refresh-token (getf cred :refresh-token)
                    :instance-url  (getf cred :instance-url)
                    :login-url     (getf cred :login-url)
                    :client-id     (getf cred :client-id)
                    :api-version   api-version)))
    (if (and refresh (connection-refresh-token conn))
        (refresh-token :conn conn)
        conn)))

;;; ──────────────────────────────────────────────────────────────────
;;; connect/sfdx-url
;;; ──────────────────────────────────────────────────────────────────

(defun connect/sfdx-url (sfdx-auth-url &key (api-version "v62.0") (refresh t))
  "Authenticate from a sfdxAuthUrl string.

   Format:  force://<clientId>:<clientSecret>:<refreshToken>@<instanceUrl>
   Example: force://PlatformCLI::5Aep8...@https://myorg.my.salesforce.com

   Obtain from: sf org display --target-org <alias> --verbose --json
                (look for the 'sfdxAuthUrl' field)

   REFRESH — Exchange the refresh token immediately.  Default: T.
   Returns a CONNECTION."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings
       "^force://([^:]*):([^:]*):([^@]+)@(.+)$"
       sfdx-auth-url)
    (unless match
      (error 'sf-auth-error
             :message
             (format nil "Cannot parse sfdxAuthUrl: ~S~%~
                          Expected format: force://<clientId>:<clientSecret>:<refreshToken>@<instanceUrl>"
                     sfdx-auth-url)))
    (let* ((client-id      (aref groups 0))
           (client-secret  (aref groups 1))
           (refresh-token  (aref groups 2))
           (instance-url   (aref groups 3))
           ;; Infer login URL from instance URL
           (login-url      (cond ((search "sandbox"           instance-url) "https://test.salesforce.com")
                                 ((search "test.salesforce.com" instance-url) "https://test.salesforce.com")
                                 (t "https://login.salesforce.com")))
           (conn (make-connection
                  :instance-url  instance-url
                  :login-url     login-url
                  :refresh-token refresh-token
                  :client-id     (if (string= client-id "") "PlatformCLI" client-id)
                  :client-secret (unless (string= client-secret "") client-secret)
                  :api-version   api-version)))
      (if refresh
          (refresh-token :conn conn)
          conn))))

;;; ──────────────────────────────────────────────────────────────────
;;; connect/username-password
;;; ──────────────────────────────────────────────────────────────────

(defun connect/username-password (&key client-id client-secret username password
                                       (instance    "https://login.salesforce.com")
                                       (api-version "v62.0"))
  "Authenticate using the OAuth 2.0 Username + Password flow.

   ⚠ Salesforce has deprecated this flow for production orgs.
     Use CONNECT/JWT for headless production authentication instead.

   PASSWORD should have the security token appended if required:
     \"mypassword\" + \"securitytoken\" → \"mypasswordABCDEFG123\"

   Returns a CONNECTION."
  (let ((params `(("grant_type"    . "password")
                  ("client_id"     . ,client-id)
                  ("client_secret" . ,client-secret)
                  ("username"      . ,username)
                  ("password"      . ,password))))
    (%do-token-request (%token-endpoint instance) params
                       :api-version   api-version
                       :login-url     instance
                       :client-id     client-id
                       :client-secret client-secret)))

;;; ──────────────────────────────────────────────────────────────────
;;; connect/jwt — JWT Bearer Token flow
;;; Helpers: DER parser, PEM loader, JWT builder
;;; ──────────────────────────────────────────────────────────────────

;;; — DER / ASN.1 primitives ————————————————————————————————————————

(defun %der-read-length (bytes pos)
  "Parse a DER length field at POS in BYTES.
   Returns (values length next-pos)."
  (let ((first (aref bytes pos)))
    (if (< first #x80)
        ;; Short form: length is the byte itself
        (values first (1+ pos))
        ;; Long form: first byte encodes how many bytes the length occupies
        (let* ((n-len-bytes (logand first #x7F))
               (len (loop for i from 0 below n-len-bytes
                          with acc = 0
                          do (setf acc (+ (ash acc 8)
                                          (aref bytes (+ pos 1 i))))
                          finally (return acc))))
          (values len (+ pos 1 n-len-bytes))))))

(defun %der-read-integer (bytes pos)
  "Parse a DER INTEGER TLV at POS.
   Returns (values big-integer next-pos)."
  (unless (= (aref bytes pos) #x02)
    (error 'sf-auth-error
           :message (format nil "DER parse error at offset ~A: ~
                                 expected INTEGER tag (0x02), got 0x~2,'0X"
                            pos (aref bytes pos))))
  (multiple-value-bind (len data-start)
      (%der-read-length bytes (1+ pos))
    ;; DER encodes positive integers with a leading 0x00 when the MSB is set.
    ;; Skip that padding byte so it doesn't corrupt the value.
    (let* ((skip  (if (and (> len 1) (zerop (aref bytes data-start))) 1 0))
           (value (loop for i from (+ data-start skip) below (+ data-start len)
                        with acc = 0
                        do (setf acc (+ (ash acc 8) (aref bytes i)))
                        finally (return acc))))
      (values value (+ data-start len)))))

(defun %parse-pkcs1-private-key (der-bytes)
  "Parse a PKCS#1 RSAPrivateKey DER structure (PEM header: BEGIN RSA PRIVATE KEY).
   Returns a plist (:n :e :d :p :q :dp :dq :qinv)."
  (unless (= (aref der-bytes 0) #x30)
    (error 'sf-auth-error :message "RSA key DER: expected SEQUENCE tag at offset 0"))
  (multiple-value-bind (_ data-start)
      (%der-read-length der-bytes 1)
    (declare (ignore _))
    (let ((pos data-start)
          n e d p q dp dq qinv)
      ;; version INTEGER — must be 0 for a two-prime RSA key
      (multiple-value-bind (version next-pos) (%der-read-integer der-bytes pos)
        (unless (zerop version)
          (error 'sf-auth-error :message "PKCS#1 RSA key: version must be 0"))
        (setf pos next-pos))
      (multiple-value-setq (n    pos) (%der-read-integer der-bytes pos))
      (multiple-value-setq (e    pos) (%der-read-integer der-bytes pos))
      (multiple-value-setq (d    pos) (%der-read-integer der-bytes pos))
      (multiple-value-setq (p    pos) (%der-read-integer der-bytes pos))
      (multiple-value-setq (q    pos) (%der-read-integer der-bytes pos))
      (multiple-value-setq (dp   pos) (%der-read-integer der-bytes pos))
      (multiple-value-setq (dq   pos) (%der-read-integer der-bytes pos))
      (multiple-value-setq (qinv pos) (%der-read-integer der-bytes pos))
      (list :n n :e e :d d :p p :q q :dp dp :dq dq :qinv qinv))))

(defun %unwrap-pkcs8 (der-bytes)
  "Unwrap a PKCS#8 PrivateKeyInfo (PEM header: BEGIN PRIVATE KEY) to the
   inner PKCS#1 RSAPrivateKey DER bytes.

   PKCS#8 structure:
     SEQUENCE {
       INTEGER 0                          ; version
       SEQUENCE { OID rsaEncryption NULL} ; AlgorithmIdentifier
       OCTET STRING { <PKCS#1 DER> }      ; privateKey
     }"
  (unless (= (aref der-bytes 0) #x30)
    (error 'sf-auth-error :message "PKCS#8 DER: expected outer SEQUENCE tag"))
  (multiple-value-bind (_ pos)
      (%der-read-length der-bytes 1)
    (declare (ignore _))
    ;; Skip version INTEGER
    (multiple-value-bind (_ next-pos) (%der-read-integer der-bytes pos)
      (declare (ignore _))
      (setf pos next-pos))
    ;; Skip AlgorithmIdentifier SEQUENCE
    (unless (= (aref der-bytes pos) #x30)
      (error 'sf-auth-error :message "PKCS#8: expected AlgorithmIdentifier SEQUENCE"))
    (multiple-value-bind (algo-len algo-data-start)
        (%der-read-length der-bytes (1+ pos))
      (setf pos (+ algo-data-start algo-len)))
    ;; Read OCTET STRING containing the inner PKCS#1 key
    (unless (= (aref der-bytes pos) #x04)
      (error 'sf-auth-error :message "PKCS#8: expected OCTET STRING for privateKey"))
    (multiple-value-bind (inner-len inner-start)
        (%der-read-length der-bytes (1+ pos))
      (subseq der-bytes inner-start (+ inner-start inner-len)))))

(defun %to-pathname (path)
  "Coerce PATH (pathname or string) to a pathname.  Strings may use native
   Windows (drive-letter, backslash) or Unix (/home/u/a.pem) syntax."
  (if (stringp path)
      (uiop:parse-unix-namestring
       (if (uiop:os-windows-p) (substitute #\/ #\\ path) path))
      path))

(defun %load-rsa-private-key (key-file)
  "Load an RSA private key from a PEM file.
   Supports both PKCS#1 (BEGIN RSA PRIVATE KEY) and
   PKCS#8 unencrypted (BEGIN PRIVATE KEY) formats.
   Returns an IRONCLAD:RSA-PRIVATE-KEY."
  (let* ((pem    (uiop:read-file-string (%to-pathname key-file)))
         (pkcs8p (search "BEGIN PRIVATE KEY" pem))  ; not "RSA PRIVATE KEY"
         ;; Strip PEM armour lines and all whitespace, leaving raw base64
         (b64    (cl-ppcre:regex-replace-all "(?m)-----[^-]+-----|\\s" pem ""))
         (der    (cl-base64:base64-string-to-usb8-array b64))
         ;; Unwrap PKCS#8 wrapper if needed to get PKCS#1 DER
         (pkcs1-der (if pkcs8p (%unwrap-pkcs8 der) der))
         (params (%parse-pkcs1-private-key pkcs1-der)))
    (ironclad:make-private-key
     :rsa
     :n (getf params :n)
     :e (getf params :e)
     :d (getf params :d)
     :p (getf params :p)
     :q (getf params :q))))

;;; — JWT construction ——————————————————————————————————————————————

;; PKCS#1 v1.5 DigestInfo prefix for SHA-256 (fixed 19-byte ASN.1 structure).
;; This wraps the hash before RSA encryption per RFC 8017 §9.2.
;; SHA-256 OID: 2.16.840.1.101.3.4.2.1
;; alexandria:define-constant accepts :test #'equalp so re-loading the file
;; doesn't signal "constant being redefined" — defconstant would fail because
;; two vector literals are never EQL even when their contents are identical.
(alexandria:define-constant +sha256-digest-info-prefix+
  (make-array 19 :element-type '(unsigned-byte 8)
                 :initial-contents '(#x30 #x31 #x30 #x0d #x06 #x09 #x60 #x86
                                     #x48 #x01 #x65 #x03 #x04 #x02 #x01
                                     #x05 #x00 #x04 #x20))
  :test #'equalp
  :documentation "ASN.1 DigestInfo prefix for SHA-256 per PKCS#1 v1.5 (RFC 8017 §9.2).")

(defun %make-jwt (client-id username instance private-key)
  "Build and return a signed RS256 JWT string for the Salesforce JWT Bearer flow.

   Steps per https://help.salesforce.com/s/articleView?id=sf.remoteaccess_oauth_jwt_flow.htm
     1. Header:  {\"alg\":\"RS256\",\"typ\":\"JWT\"}
     2. Claims:  {\"iss\":<client-id>,\"sub\":<username>,\"aud\":<instance>,\"exp\":<+3min>}
     3. Signing input: base64url(header) \".\" base64url(claims)
     4. Signature: RSA-PKCS1v1.5-SHA256(signing-input, private-key)
     5. JWT: signing-input \".\" base64url(signature)"
  (let* ((now (local-time:now))
         (exp (local-time:timestamp-to-unix
               (local-time:timestamp+ now 3 :minute)))
         ;; 1 & 2 — header and claims JSON
         (header-json  "{\"alg\":\"RS256\",\"typ\":\"JWT\"}")
         (claims-json  (com.inuoe.jzon:stringify
                        (alexandria:plist-hash-table
                         (list "iss" client-id
                               "sub" username
                               "aud" instance
                               "exp" exp)
                         :test #'equal)))
         ;; 3 — signing input
         (b64-header     (%base64url-encode (%string->bytes header-json)))
         (b64-claims     (%base64url-encode (%string->bytes claims-json)))
         (signing-input  (format nil "~A.~A" b64-header b64-claims))
         (signing-bytes  (%string->bytes signing-input))
         ;; 4 — RSA-PKCS1v1.5-SHA256 signature
         ;;
         ;; ironclad:sign-message for RSA applies raw PKCS#1 v1.5 block
         ;; formatting (0x00 0x01 <FF padding> 0x00 <message>) around the
         ;; supplied bytes.  For RS256 the "message" must be the PKCS#1
         ;; DigestInfo structure (prefix + SHA-256 hash), not the raw data.
         (digest         (ironclad:make-digest :sha256))
         (_              (ironclad:update-digest digest signing-bytes))
         (hash           (ironclad:produce-digest digest))
         (digest-info    (concatenate '(simple-array (unsigned-byte 8) (*))
                                      +sha256-digest-info-prefix+ hash))
         (signature      (ironclad:sign-message private-key digest-info))
         (b64-sig        (%base64url-encode signature)))
    (declare (ignore _))
    (format nil "~A.~A" signing-input b64-sig)))

;;; — connect/jwt ———————————————————————————————————————————————————

(defun connect/jwt (&key client-id key-file username
                         (instance    "https://login.salesforce.com")
                         (api-version "v62.0"))
  "Authenticate using the OAuth 2.0 JWT Bearer Token flow.
   No user interaction required; ideal for headless/CI/server-to-server use.

   CLIENT-ID   — Connected App consumer key (from Salesforce Setup)
   KEY-FILE    — Pathname to the RSA private key PEM file.
                 Generate with:
                   openssl genrsa -out server.key 2048
                   openssl req -new -x509 -key server.key -out server.crt -days 365
                 Upload server.crt to the Connected App in Salesforce Setup.
                 Supports PKCS#1 (BEGIN RSA PRIVATE KEY) and
                 PKCS#8 unencrypted (BEGIN PRIVATE KEY) PEM formats.
   USERNAME    — Salesforce username to impersonate (must be pre-authorised
                 in the Connected App's policies)
   INSTANCE    — Login URL; use \"https://test.salesforce.com\" for sandboxes.
   API-VERSION — Defaults to v62.0.

   Returns a CONNECTION (no refresh token; re-call CONNECT/JWT to rotate)."
  (let* ((pkey (handler-case
                   (%load-rsa-private-key key-file)
                 (error (e)
                   (error 'sf-auth-error
                          :message (format nil "Failed to load RSA key from ~A: ~A"
                                           key-file e)))))
         (jwt  (%make-jwt client-id username instance pkey))
         (params `(("grant_type" . "urn:ietf:params:oauth:grant-type:jwt-bearer")
                   ("assertion"  . ,jwt)))
         (conn (%do-token-request (%token-endpoint instance) params
                                  :api-version   api-version
                                  :login-url     instance
                                  :client-id     client-id)))
    ;; JWT connections have no refresh token; mark the client-id for reference
    (setf (connection-client-id conn) client-id)
    conn))

;;; ──────────────────────────────────────────────────────────────────
;;; connect/web-server — Authorization Code flow
;;; ──────────────────────────────────────────────────────────────────

(defun connect/web-server (&key client-id
                                (redirect-uri "http://localhost:1717/OauthRedirect")
                                (instance     "https://login.salesforce.com")
                                (scope        "full refresh_token offline_access")
                                (api-version  "v62.0")
                                code-challenge)
  "Begin the OAuth 2.0 Authorization Code (Web Server) flow.

   Constructs and returns (values authorization-url state) where STATE is
   a random CSRF token.  Redirect the user's browser to authorization-url.
   After Salesforce redirects back to REDIRECT-URI with ?code=<code>&state=<state>,
   verify the state and call CONNECT/WEB-SERVER-CALLBACK with the code.

   CLIENT-ID    — Connected App consumer key.
   REDIRECT-URI — Must match the Connected App's callback URL exactly.
   INSTANCE     — Login URL; use \"https://test.salesforce.com\" for sandboxes.
   SCOPE        — Space-separated OAuth scopes.
   API-VERSION  — Stored on the returned connection.
   CODE-CHALLENGE — PKCE S256 challenge (see %PKCE-CHALLENGE); keep its
                  verifier and pass it to CONNECT/WEB-SERVER-CALLBACK.
                  Salesforce requires PKCE for many connected apps,
                  including the CLI's PlatformCLI.

   Returns (values authorization-url state-string)."
  (declare (ignore api-version))  ; stored in callback
  ;; Use two independent random states to generate a 64-bit CSRF token
  (let ((state (format nil "~(~8,'0x~8,'0x~)"
                       (random #xFFFFFFFF (make-random-state t))
                       (random #xFFFFFFFF (make-random-state t)))))
    (values
     (format nil "~A/services/oauth2/authorize?~A"
             instance
             (with-output-to-string (out)
               (loop for (k . v) in `(("response_type" . "code")
                                      ("client_id"     . ,client-id)
                                      ("redirect_uri"  . ,redirect-uri)
                                      ("scope"         . ,scope)
                                      ("state"         . ,state)
                                      ,@(when code-challenge
                                          `(("code_challenge"        . ,code-challenge)
                                            ("code_challenge_method" . "S256"))))
                     for first = t then nil
                     unless first do (write-char #\& out)
                     do (format out "~A=~A"
                                k (quri:url-encode v :encoding :utf-8)))))
     state)))

(defun connect/web-server-callback (code &key client-id client-secret redirect-uri
                                              (instance    "https://login.salesforce.com")
                                              (api-version "v62.0")
                                              code-verifier)
  "Complete the Authorization Code flow after receiving the authorization CODE.

   CODE          — The 'code' query parameter from Salesforce's redirect.
   CLIENT-ID     — Same Connected App consumer key used in CONNECT/WEB-SERVER.
   CLIENT-SECRET — Connected App consumer secret.
   REDIRECT-URI  — Must match the original redirect URI exactly.
   INSTANCE      — Same login URL used in CONNECT/WEB-SERVER.
   CODE-VERIFIER — The PKCE verifier whose challenge was sent to
                   CONNECT/WEB-SERVER, if any.

   Returns a CONNECTION (includes a refresh token for future REFRESH-TOKEN calls)."
  (let ((params `(("grant_type"    . "authorization_code")
                  ("code"          . ,code)
                  ("client_id"     . ,client-id)
                  ("redirect_uri"  . ,redirect-uri)
                  ,@(when client-secret
                      `(("client_secret" . ,client-secret)))
                  ,@(when code-verifier
                      `(("code_verifier" . ,code-verifier))))))
    (%do-token-request (%token-endpoint instance) params
                       :api-version   api-version
                       :login-url     instance
                       :client-id     client-id
                       :client-secret client-secret)))

;;; ──────────────────────────────────────────────────────────────────
;;; Saving a new login where the sf/sfdx CLI keeps it
;;;
;;; Same layout the CLI writes (checked against a real install):
;;;   ~/.sfdx/<username>.json  accessToken/refreshToken AES-encrypted with
;;;                            the key in ~/.sfdx/key.json, plus orgId,
;;;                            instanceUrl, loginUrl, clientId, ...
;;;   ~/.sfdx/alias.json       {"orgs": {alias: username}}
;;; After this, CONNECT/CACHED-CREDENTIALS, the sf CLI and other tools see
;;; the login, and token refreshes are written back (%PERSIST-REFRESHED-
;;; CREDENTIALS).  Nothing is saved unless ~/.sfdx/key.json exists: without
;;; the CLI's key we can't encrypt tokens in a form the CLI can read.
;;; ──────────────────────────────────────────────────────────────────

#+(and sbcl unix)
(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-posix))

(defun %fetch-userinfo (conn)
  "GET /services/oauth2/userinfo — who CONN's access token belongs to."
  (com.inuoe.jzon:parse
   (dex:get (format nil "~A/services/oauth2/userinfo"
                    (string-right-trim "/" (connection-instance-url conn)))
            :headers `(("Authorization" . ,(format nil "Bearer ~A" (connection-access-token conn)))
                       ("Accept" . "application/json")))))

(defun %write-private-json-file (path data)
  "Write DATA as JSON to PATH, creating directories; owner-only on Unix
   (it holds tokens), like the CLI does."
  (ensure-directories-exist path)
  (%write-json-file path data)
  #+(and sbcl unix) (sb-posix:chmod (uiop:native-namestring path) #o600))

(defun %read-json-object-for-update (path)
  "PATH's parsed JSON object, or a new empty one if PATH doesn't exist.
   Signals SF-AUTH-ERROR if it exists but can't be read, rather than
   letting a rewrite destroy it."
  (cond ((not (uiop:file-exists-p path)) (make-hash-table :test #'equal))
        ((%try-parse-json-file path))
        (t (error 'sf-auth-error :message (format nil "~A exists but is not valid JSON." path)))))

(defun %set-sfdx-alias (alias username)
  (let* ((path (uiop:subpathname *sfdx-dir* "alias.json"))
         (data (%read-json-object-for-update path))
         (orgs (or (gethash "orgs" data)
                   (setf (gethash "orgs" data) (make-hash-table :test #'equal)))))
    (setf (gethash alias orgs) username)
    (%write-private-json-file path data)))

(defun %cli-timestamp ()
  "Now, formatted the way the CLI stamps instanceApiVersionLastRetrieved
   (e.g. \"10/28/2026, 12:48:59 AM\")."
  (string-upcase
   (local-time:format-timestring
    nil (local-time:now)
    :format '(:month "/" :day "/" :year ", " :hour12 ":" (:min 2) ":" (:sec 2) " " :ampm))))

(defun %persist-new-login (conn &key alias)
  "Save CONN (a fresh login) for the sf/sfdx CLI, as ~/.sfdx/<username>.json,
   and map ALIAS to its username in ~/.sfdx/alias.json.  An existing file
   for the same username is updated, keeping its other fields.  Sets
   CONNECTION-USERNAME so later token refreshes are written back too.
   Best-effort like %PERSIST-REFRESHED-CREDENTIALS: any failure (no key.json,
   no write access, userinfo unreachable) is WARNed and the in-memory
   CONN stays valid.  Returns the username, or NIL if nothing was saved."
  (handler-case
      (let* ((info     (%fetch-userinfo conn))
             (username (or (gethash "preferred_username" info)
                           (error 'sf-auth-error :message "userinfo has no username")))
             (key      (%sfdx-encryption-key))
             (path     (uiop:subpathname *sfdx-dir* (format nil "~A.json" username)))
             (data     (%read-json-object-for-update path))
             (instance (connection-instance-url conn)))
        (flet ((put (k v) (setf (gethash k data) v)))
          (put "accessToken" (%sfdx-encrypt (connection-access-token conn) key))
          (when (connection-refresh-token conn)
            (put "refreshToken" (%sfdx-encrypt (connection-refresh-token conn) key)))
          (put "instanceUrl" instance)
          (put "orgId" (gethash "organization_id" info))
          (put "username" username)
          (put "loginUrl" (connection-login-url conn))
          (put "clientId" (connection-client-id conn))
          (put "isDevHub" (gethash "isDevHub" data nil))
          (put "instanceApiVersion" (string-left-trim "v" (connection-api-version conn)))
          (put "instanceApiVersionLastRetrieved" (%cli-timestamp))
          (put "isSandbox" (and (or (search "test.salesforce.com" (connection-login-url conn))
                                    (search ".sandbox." instance))
                                t))
          (put "isScratch" (gethash "isScratch" data nil)))
        (%write-private-json-file path data)
        (when alias (%set-sfdx-alias alias username))
        (setf (connection-username conn) username)
        username)
    (error (e)
      (warn "cl-sf: logged in, but could not save the login for the sf CLI: ~A" e)
      nil)))

;;; ──────────────────────────────────────────────────────────────────
;;; connect/browser — Authorization Code flow with a local callback
;;; ──────────────────────────────────────────────────────────────────

(defun %open-browser (url)
  (uiop:launch-program
   (cond ((uiop:os-windows-p) (list "rundll32" "url.dll,FileProtocolHandler" url))
         ((uiop:os-macosx-p)  (list "open" url))
         (t                   (list "xdg-open" url)))))

(defun %query-params (target)
  "\"/cb?code=a%3D&state=b\" -> ((\"code\" . \"a=\") (\"state\" . \"b\"))."
  (let ((q (position #\? target)))
    (when q
      (loop for pair in (uiop:split-string (subseq target (1+ q)) :separator "&")
            for eq = (position #\= pair)
            when eq collect (cons (subseq pair 0 eq)
                                  (quri:url-decode (subseq pair (1+ eq))))))))

(defun %wait-for-oauth-redirect (port expected-state timeout)
  "Listen on 127.0.0.1:PORT for Salesforce's redirect and return the
   authorization code.  Requests without a code or error (a browser's
   speculative preconnect, a favicon fetch) are ignored.  Polls a
   non-blocking socket so TIMEOUT (seconds) is honoured on every platform
   (a blocking accept can't be interrupted on Windows)."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
    (sb-bsd-sockets:socket-bind listener #(127 0 0 1) port)
    (sb-bsd-sockets:socket-listen listener 4)
    (setf (sb-bsd-sockets:non-blocking-mode listener) t)
    (unwind-protect
         (loop
           (when (> (get-internal-real-time) deadline)
             (error 'sf-auth-error
                    :message (format nil "No login completed in the browser within ~A seconds."
                                     timeout)))
           (let ((client (sb-bsd-sockets:socket-accept listener)))
             (if (null client)
                 (sleep 0.1)
                 (let* ((stream (sb-bsd-sockets:socket-make-stream
                                 client :input t :output t :buffering :full
                                        :external-format :utf-8))
                        (line   (and (sb-sys:wait-until-fd-usable
                                      (sb-bsd-sockets:socket-file-descriptor client) :input 2)
                                     (ignore-errors (read-line stream nil))))
                        (params (and line (%query-params
                                           (second (uiop:split-string line :separator " "))))))
                   (when (or (assoc "code" params :test #'string=)
                             (assoc "error" params :test #'string=))
                     (format stream "HTTP/1.1 200 OK~C~%Content-Type: text/plain~C~%~
                                     Connection: close~C~%~C~%Authenticated. You can close this tab.~%"
                             #\Return #\Return #\Return #\Return)
                     (finish-output stream)
                     (close stream)
                     (let ((error-text (or (cdr (assoc "error_description" params :test #'string=))
                                           (cdr (assoc "error" params :test #'string=)))))
                       (when error-text
                         (error 'sf-auth-error
                                :message (format nil "Salesforce refused the login: ~A" error-text)))
                       (unless (equal expected-state (cdr (assoc "state" params :test #'string=)))
                         (error 'sf-auth-error
                                :message "OAuth state mismatch (possible CSRF); aborting."))
                       (return (cdr (assoc "code" params :test #'string=)))))
                   (ignore-errors (close stream))))))
      (sb-bsd-sockets:socket-close listener))))

(defun connect/browser (&key (instance "https://login.salesforce.com")
                             (client-id "PlatformCLI")
                             client-secret
                             (redirect-uri "http://localhost:1717/OauthRedirect")
                             (api-version "v62.0")
                             (timeout 300)
                             alias
                             (save t))
  "Authenticate interactively: open the system browser on Salesforce's
   login page, wait on a local port for the redirect, and exchange the
   code for tokens.  Returns a CONNECTION with a refresh token.

   INSTANCE     — login URL: https://login.salesforce.com (default),
                  https://test.salesforce.com for sandboxes, or your My
                  Domain URL.
   CLIENT-ID    — default is the Salesforce CLI's own public connected
                  app, PlatformCLI, which needs no secret and already
                  allows the default REDIRECT-URI (the one `sf org login
                  web` uses).  Pass your own CLIENT-ID / CLIENT-SECRET /
                  REDIRECT-URI to use a different connected app.
   TIMEOUT      — seconds to wait for the browser before giving up.
   SAVE         — when true (default), save the login where the sf/sfdx CLI
                  keeps its own — ~/.sfdx/<username>.json, with ALIAS (if
                  given) mapped in ~/.sfdx/alias.json — so
                  CONNECT/CACHED-CREDENTIALS, the CLI and later Lisp
                  sessions can reuse it.  Needs ~/.sfdx/key.json (the CLI's
                  encryption key); without it a warning is issued and the
                  login is kept in memory only.

   The redirect's port must be free on localhost (the port in
   REDIRECT-URI, 1717 by default)."
  (let* ((verifier (%pkce-verifier)))
   (multiple-value-bind (url state)
      (connect/web-server :client-id client-id :redirect-uri redirect-uri
                          :instance instance :api-version api-version
                          :code-challenge (%pkce-challenge verifier))
    (format t "~&Opening your browser to log in to ~A~%If it doesn't open, visit:~%  ~A~%"
            instance url)
    (%open-browser url)
    (let ((code (%wait-for-oauth-redirect (or (quri:uri-port (quri:uri redirect-uri)) 80)
                                          state timeout)))
      (let ((conn (connect/web-server-callback code
                                               :client-id client-id :client-secret client-secret
                                               :redirect-uri redirect-uri
                                               :instance instance :api-version api-version
                                               :code-verifier verifier)))
        (when save
          (let ((username (%persist-new-login conn :alias alias)))
            (when username
              (format t "~&Saved login for ~A~@[ (alias ~A)~] for the sf CLI.~%" username alias))))
        conn)))))

;;; ──────────────────────────────────────────────────────────────────
;;; with-conn — the ergonomic entry point
;;; ──────────────────────────────────────────────────────────────────

(defvar *session-connections* (make-hash-table :test #'equal :synchronized t)
  "(alias instance-url) -> CONNECTION, for the keyword form of WITH-CONN, so
   a browser login happens once per Lisp session, not once per WITH-CONN.")

(defun %same-instance-p (a b)
  (flet ((norm (u) (string-right-trim "/" (string-downcase u))))
    (string= (norm a) (norm b))))

(defun %cached-connection (alias instance-url)
  "ALIAS's connection from the sf/sfdx CLI cache, or NIL if the CLI has no
   usable credentials for it.  Signals SF-AUTH-ERROR if the cached org is
   a different INSTANCE-URL than requested — never silently pick the
   wrong org."
  (let ((conn (and alias
                   (handler-case (connect/cached-credentials :username alias)
                     (sf-auth-error () nil)))))
    (when (and conn instance-url
               (not (%same-instance-p instance-url (connection-instance-url conn))))
      (error 'sf-auth-error
             :message (format nil "~S is authorized for ~A in the sf CLI, but ~
                                   :INSTANCE-URL is ~A."
                              alias (connection-instance-url conn) instance-url)))
    conn))

(defun %with-conn-keys (&key alias instance-url)
  "WITH-CONN's keyword form: the CLI-cached login for ALIAS if there is
   one, else an interactive CONNECT/BROWSER login to INSTANCE-URL."
  (unless (or alias instance-url)
    (error 'sf-auth-error :message "WITH-CONN needs :ALIAS and/or :INSTANCE-URL."))
  (let ((key (list alias (and instance-url (string-right-trim "/" (string-downcase instance-url))))))
    (or (gethash key *session-connections*)
        (setf (gethash key *session-connections*)
              (or (%cached-connection alias instance-url)
                  (connect/browser :instance (or instance-url "https://login.salesforce.com")
                                      :alias alias))))))

(defun %with-conn-resolve (org)
  "Used by WITH-CONN: a CONNECTION is used as-is; a string (an sf/sfdx CLI
   alias or literal username) is resolved via CONNECT/CACHED-CREDENTIALS."
  (etypecase org
    (connection org)
    (string     (connect/cached-credentials :username org))))

(defmacro with-conn (&body forms)
  "Dynamically bind *CONNECTION* for the extent of the body, so every cl-sf
   call inside can omit :CONN.  Two forms:

   Keywords — (with-conn :alias \"myorg\" :instance-url \"https://...\" body...)
     Uses the sf/sfdx CLI's cached login for :ALIAS if it has one.
     Otherwise opens your browser to log in to :INSTANCE-URL (default
     https://login.salesforce.com) via CONNECT/BROWSER, and saves the login
     where the sf CLI keeps its own (~/.sfdx, with :ALIAS mapped to the
     username) — so the next WITH-CONN, the CLI, and later Lisp sessions
     find it in the cache and skip the browser.  The connection is also
     kept for the rest of the Lisp session.  A cached alias that belongs
     to a different instance than :INSTANCE-URL is an error.

   Positional — (with-conn ORG body...)
     ORG is a string (an sf/sfdx CLI alias or literal username, read from
     the CLI cache) or an existing CONNECTION (e.g. from CONNECT/JWT).

   Examples:
     (with-conn :alias \"org\" :instance-url \"https://myorg.my.salesforce.com\"
       (select Id Name FROM Account LIMIT 5))

     (with-conn \"myorg\"
       (select Id FROM Account))

     (with-conn (connect/jwt :client-id \"...\" :key-file #p\"server.key\"
                             :username \"me@example.com\")
       (select Id FROM Account))"
  (if (keywordp (first forms))
      (let ((options '()))
        (loop while (keywordp (first forms))
              do (push (pop forms) options)
                 (push (pop forms) options))
        `(let ((*connection* (%with-conn-keys ,@(nreverse options))))
           ,@forms))
      `(let ((*connection* (%with-conn-resolve ,(first forms))))
         ,@(rest forms))))
