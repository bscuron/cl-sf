;;;; cl-sf/tests/auth-tests.lisp
;;;; Unit tests for cl-sf/auth — pure functions, no HTTP calls.

(in-package #:cl-sf/tests)

(define-test auth
  :parent cl-sf/tests
  :description "Authentication — connection struct, token validity, response parsing.")

;;; ──────────────────────────────────────────────────────────────────
;;; Connection struct
;;; ──────────────────────────────────────────────────────────────────

(define-test connection/defaults
  :parent auth
  (let ((conn (cl-sf/auth:make-connection)))
    (false  (cl-sf/auth:connection-access-token  conn) "access-token nil by default")
    (false  (cl-sf/auth:connection-refresh-token conn) "refresh-token nil by default")
    (false  (cl-sf/auth:connection-instance-url  conn) "instance-url nil by default")
    (is string= "https://login.salesforce.com"
        (cl-sf/auth:connection-login-url conn)    "login-url default")
    (is string= "v62.0"
        (cl-sf/auth:connection-api-version conn)  "api-version default")
    (is string= "PlatformCLI"
        (cl-sf/auth:connection-client-id conn)    "client-id default")))

(define-test connection/round-trip
  :parent auth
  (let ((conn (cl-sf/auth:make-connection
               :access-token  "TOK"
               :instance-url  "https://myorg.salesforce.com"
               :api-version   "v61.0")))
    (is string= "TOK"                            (cl-sf/auth:connection-access-token conn))
    (is string= "https://myorg.salesforce.com"   (cl-sf/auth:connection-instance-url conn))
    (is string= "v61.0"                          (cl-sf/auth:connection-api-version  conn))))

;;; ──────────────────────────────────────────────────────────────────
;;; token-valid-p
;;; ──────────────────────────────────────────────────────────────────

(define-test token-valid-p/nil-token
  :parent auth
  (false (cl-sf/auth:token-valid-p
          (cl-sf/auth:make-connection :access-token nil))
         "nil access-token → NIL"))

(define-test token-valid-p/no-expiry
  :parent auth
  (true (cl-sf/auth:token-valid-p
         (cl-sf/auth:make-connection :access-token "TOK" :expires-at nil))
        "token set, nil expires-at → T (unknown expiry assumed valid)"))

(define-test token-valid-p/expired
  :parent auth
  ;; expires-at 5 minutes in the past
  (let ((past (local-time:timestamp- (local-time:now) 300 :sec)))
    (false (cl-sf/auth:token-valid-p
            (cl-sf/auth:make-connection :access-token "TOK" :expires-at past))
           "expired token → NIL")))

(define-test token-valid-p/valid
  :parent auth
  ;; expires-at 2 hours in the future
  (let ((future (local-time:timestamp+ (local-time:now) 7200 :sec)))
    (true (cl-sf/auth:token-valid-p
           (cl-sf/auth:make-connection :access-token "TOK" :expires-at future))
          "future expiry → T")))

(define-test token-valid-p/within-buffer
  :parent auth
  ;; expires-at is 30s from now, but buffer is 60s → should be NIL
  (let ((near-future (local-time:timestamp+ (local-time:now) 30 :sec)))
    (false (cl-sf/auth:token-valid-p
            (cl-sf/auth:make-connection :access-token "TOK" :expires-at near-future))
           "expiry within *token-expiry-buffer* → NIL")))

;;; ──────────────────────────────────────────────────────────────────
;;; %parse-token-response
;;; ──────────────────────────────────────────────────────────────────

(define-test parse-token-response/success
  :parent auth
  (let* ((json "{\"access_token\":\"ACC\",\"instance_url\":\"https://org.sf.com\",\
\"refresh_token\":\"REF\",\"issued_at\":\"1700000000000\"}")
         (conn (cl-sf/auth::%parse-token-response json)))
    (is string= "ACC"                (cl-sf/auth:connection-access-token  conn))
    (is string= "https://org.sf.com" (cl-sf/auth:connection-instance-url  conn))
    (is string= "REF"                (cl-sf/auth:connection-refresh-token conn))
    (true (cl-sf/auth:connection-expires-at conn) "expires-at computed from issued_at")))

(define-test parse-token-response/no-refresh-token
  :parent auth
  (let* ((json "{\"access_token\":\"JWT_ACC\",\"instance_url\":\"https://org.sf.com\"}")
         (conn (cl-sf/auth::%parse-token-response json)))
    (is string= "JWT_ACC" (cl-sf/auth:connection-access-token conn))
    (false (cl-sf/auth:connection-refresh-token conn) "JWT flow → no refresh token")))

(define-test parse-token-response/sf-error
  :parent auth
  (fail (cl-sf/auth::%parse-token-response
         "{\"error\":\"invalid_grant\",\"error_description\":\"Bad credentials\"}")
        'cl-sf/auth:sf-token-error
        "Salesforce error response → sf-token-error"))

(define-test parse-token-response/missing-access-token
  :parent auth
  (fail (cl-sf/auth::%parse-token-response "{\"instance_url\":\"https://org.sf.com\"}")
        'cl-sf/auth:sf-auth-error
        "Missing access_token → sf-auth-error"))

(define-test parse-token-response/no-issued-at
  :parent auth
  (let* ((json "{\"access_token\":\"TOK\",\"instance_url\":\"https://org.sf.com\"}")
         (conn (cl-sf/auth::%parse-token-response json)))
    (false (cl-sf/auth:connection-expires-at conn)
           "No issued_at → expires-at is nil")))

;;; ──────────────────────────────────────────────────────────────────
;;; connect/sfdx-url — URL parsing (no HTTP)
;;; ──────────────────────────────────────────────────────────────────

(define-test sfdx-url/invalid-format
  :parent auth
  (fail (cl-sf/auth:connect/sfdx-url "not-a-valid-sfdx-url")
        'cl-sf/auth:sf-auth-error
        "Invalid sfdxAuthUrl → sf-auth-error"))

(define-test sfdx-url/valid-no-refresh
  :parent auth
  ;; :refresh nil skips the HTTP call — tests pure parsing
  (let ((conn (cl-sf/auth:connect/sfdx-url
               "force://PlatformCLI::5AepSomeRefreshToken@https://myorg.salesforce.com"
               :refresh nil)))
    (is string= "https://myorg.salesforce.com"
        (cl-sf/auth:connection-instance-url  conn))
    (is string= "5AepSomeRefreshToken"
        (cl-sf/auth:connection-refresh-token conn))
    (is string= "PlatformCLI"
        (cl-sf/auth:connection-client-id     conn))
    (is string= "https://login.salesforce.com"
        (cl-sf/auth:connection-login-url     conn))))

(define-test sfdx-url/sandbox-login-url
  :parent auth
  (let ((conn (cl-sf/auth:connect/sfdx-url
               "force://PlatformCLI::TOKEN@https://myorg.sandbox.my.salesforce.com"
               :refresh nil)))
    (is string= "https://test.salesforce.com"
        (cl-sf/auth:connection-login-url conn)
        "Sandbox instance → test.salesforce.com login URL")))

(define-test sfdx-url/with-client-secret
  :parent auth
  (let ((conn (cl-sf/auth:connect/sfdx-url
               "force://MyClientId:MySecret:5AepToken@https://myorg.salesforce.com"
               :refresh nil)))
    (is string= "MyClientId" (cl-sf/auth:connection-client-id     conn))
    (is string= "MySecret"   (cl-sf/auth:connection-client-secret  conn))))

;;; ──────────────────────────────────────────────────────────────────
;;; sfdx AES-256-GCM round-trip (pure — no filesystem)
;;; ──────────────────────────────────────────────────────────────────

(define-test sfdx-crypto/round-trip
  :parent auth
  (let* ((key    "0123456789abcdef0123456789abcdef")
         (secret "5Aep861signedExampleRefreshToken==")
         (enc    (cl-sf/auth::%sfdx-encrypt secret key))
         (dec    (cl-sf/auth::%sfdx-decrypt enc key)))
    (is string= secret dec "decrypt(encrypt(x)) == x")
    (is = 1 (count #\: enc) "format has exactly one ':' separator")))

(define-test sfdx-crypto/tampered-tag-fails
  :parent auth
  (let* ((key       "0123456789abcdef0123456789abcdef")
         (enc       (cl-sf/auth::%sfdx-encrypt "secret-value" key))
         (last-char (char enc (1- (length enc))))
         (tampered  (concatenate 'string (subseq enc 0 (1- (length enc)))
                                 (if (char= last-char #\0) "1" "0"))))
    (fail (cl-sf/auth::%sfdx-decrypt tampered key) 'error
          "A tampered auth tag must fail GCM verification")))

;;; ──────────────────────────────────────────────────────────────────
;;; Alias resolution — ~/.sf and ~/.sfdx on-disk state
;;; ──────────────────────────────────────────────────────────────────

(define-test resolve-org-username/alias-via-sf-config
  :parent auth
  (let ((sf-dir (make-temp-dir)) (sfdx-dir (make-temp-dir)))
    (unwind-protect
        (progn
          (write-fixture-json sf-dir "config.json"
                              (make-hash "orgs" (make-hash "myalias" "real@user.com")))
          (let ((cl-sf/auth:*sf-dir* sf-dir) (cl-sf/auth:*sfdx-dir* sfdx-dir))
            (is string= "real@user.com"
                (cl-sf/auth::%resolve-org-username "myalias"))))
      (delete-temp-dir sf-dir) (delete-temp-dir sfdx-dir))))

(define-test resolve-org-username/alias-via-sfdx-alias-file
  :parent auth
  (let ((sf-dir (make-temp-dir)) (sfdx-dir (make-temp-dir)))
    (unwind-protect
        (progn
          (write-fixture-json sfdx-dir "alias.json"
                              (make-hash "orgs" (make-hash "legacy" "legacy@user.com")))
          (let ((cl-sf/auth:*sf-dir* sf-dir) (cl-sf/auth:*sfdx-dir* sfdx-dir))
            (is string= "legacy@user.com"
                (cl-sf/auth::%resolve-org-username "legacy"))))
      (delete-temp-dir sf-dir) (delete-temp-dir sfdx-dir))))

(define-test resolve-org-username/literal-username-fallback
  :parent auth
  (let ((sf-dir (make-temp-dir)) (sfdx-dir (make-temp-dir)))
    (unwind-protect
        (let ((cl-sf/auth:*sf-dir* sf-dir) (cl-sf/auth:*sfdx-dir* sfdx-dir))
          (is string= "someone@example.com"
              (cl-sf/auth::%resolve-org-username "someone@example.com")
              "No alias matches → input treated as a literal username"))
      (delete-temp-dir sf-dir) (delete-temp-dir sfdx-dir))))

(define-test resolve-org-username/default-org-from-target-org
  :parent auth
  (let ((sf-dir (make-temp-dir)) (sfdx-dir (make-temp-dir)))
    (unwind-protect
        (progn
          (write-fixture-json sf-dir "config.json"
                              (make-hash "orgs" (make-hash "myalias" "default@user.com")
                                        "target-org" "myalias"))
          (let ((cl-sf/auth:*sf-dir* sf-dir) (cl-sf/auth:*sfdx-dir* sfdx-dir))
            (is string= "default@user.com"
                (cl-sf/auth::%resolve-org-username nil)
                "NIL → resolves via target-org then its alias")))
      (delete-temp-dir sf-dir) (delete-temp-dir sfdx-dir))))

(define-test resolve-org-username/no-default-configured
  :parent auth
  (let ((sf-dir (make-temp-dir)) (sfdx-dir (make-temp-dir)))
    (unwind-protect
        (let ((cl-sf/auth:*sf-dir* sf-dir) (cl-sf/auth:*sfdx-dir* sfdx-dir))
          (fail (cl-sf/auth::%resolve-org-username nil) 'cl-sf/auth:sf-auth-error
                "No alias and no configured default → sf-auth-error"))
      (delete-temp-dir sf-dir) (delete-temp-dir sfdx-dir))))

;;; ──────────────────────────────────────────────────────────────────
;;; Merged credential loading — ~/.sf (accessToken) + ~/.sfdx (refreshToken)
;;; ──────────────────────────────────────────────────────────────────

(define-test load-cached-org/merges-sf-and-sfdx
  :parent auth
  (let ((sf-dir (make-temp-dir)) (sfdx-dir (make-temp-dir))
        (key "0123456789abcdef0123456789abcdef"))
    (unwind-protect
        (progn
          (write-fixture-json sfdx-dir "key.json" (make-hash "key" key))
          (write-fixture-json sf-dir "orgs/user@example.com.json"
                              (make-hash "username"    "user@example.com"
                                        "orgId"        "00Dxx0000000001"
                                        "instanceUrl"  "https://fresh.my.salesforce.com"
                                        "loginUrl"     "https://login.salesforce.com"
                                        "accessToken"  "FRESH_ACCESS_TOKEN"))
          (write-fixture-json sfdx-dir "user@example.com.json"
                              (make-hash "username"     "user@example.com"
                                        "clientId"      "PlatformCLI"
                                        "accessToken"   (cl-sf/auth::%sfdx-encrypt "STALE_ACCESS" key)
                                        "refreshToken"  (cl-sf/auth::%sfdx-encrypt "THE_REFRESH_TOKEN" key)))
          (let* ((cl-sf/auth:*sf-dir* sf-dir) (cl-sf/auth:*sfdx-dir* sfdx-dir)
                 (cred (cl-sf/auth::%load-cached-org "user@example.com")))
            (is string= "FRESH_ACCESS_TOKEN" (getf cred :access-token)
                "access-token prefers the sf (new CLI) copy")
            (is string= "THE_REFRESH_TOKEN"  (getf cred :refresh-token)
                "refresh-token only exists in the sfdx (legacy) copy")
            (is string= "PlatformCLI"        (getf cred :client-id))
            (is string= "https://fresh.my.salesforce.com" (getf cred :instance-url))))
      (delete-temp-dir sf-dir) (delete-temp-dir sfdx-dir))))

(define-test load-cached-org/neither-file-exists
  :parent auth
  (let ((sf-dir (make-temp-dir)) (sfdx-dir (make-temp-dir)))
    (unwind-protect
        (let ((cl-sf/auth:*sf-dir* sf-dir) (cl-sf/auth:*sfdx-dir* sfdx-dir))
          (fail (cl-sf/auth::%load-cached-org "nobody@example.com") 'cl-sf/auth:sf-auth-error
                "No cached org anywhere → sf-auth-error"))
      (delete-temp-dir sf-dir) (delete-temp-dir sfdx-dir))))

;;; ── WITH-CONN keyword form: CLI cache first, browser login otherwise ──

(defmacro with-stubbed-logins ((cached browser log) &body body)
  "CONNECT/CACHED-CREDENTIALS returns/signals per CACHED (a connection, or
   NIL meaning no cached login); CONNECT/BROWSER returns BROWSER.  Both log
   calls onto LOG.  Clears the per-session connection cache around BODY."
  `(let ((saved-cached (fdefinition 'cl-sf/auth:connect/cached-credentials))
         (saved-browser (fdefinition 'cl-sf/auth:connect/browser)))
     (clrhash cl-sf/auth::*session-connections*)
     (unwind-protect
          (progn
            (setf (fdefinition 'cl-sf/auth:connect/cached-credentials)
                  (lambda (&key username &allow-other-keys)
                    (push (list :cached username) ,log)
                    (or ,cached
                        (error 'cl-sf/auth:sf-auth-error :message "no cached credentials")))
                  (fdefinition 'cl-sf/auth:connect/browser)
                  (lambda (&key instance &allow-other-keys)
                    (push (list :browser instance) ,log)
                    ,browser))
            ,@body)
       (setf (fdefinition 'cl-sf/auth:connect/cached-credentials) saved-cached
             (fdefinition 'cl-sf/auth:connect/browser) saved-browser)
       (clrhash cl-sf/auth::*session-connections*))))

(define-test with-conn/keywords-use-cli-cache-first
  :parent auth
  (let* ((log '())
         (cached (cl-sf/auth:make-connection :instance-url "https://org.my.salesforce.com/")))
    (with-stubbed-logins (cached nil log)
      (cl-sf/auth:with-conn :alias "org" :instance-url "https://ORG.my.salesforce.com"
        (is eq cached cl-sf/auth:*connection*))
      (is equal '((:cached "org")) log))))

(define-test with-conn/keywords-fall-back-to-browser-once-per-session
  :parent auth
  (let* ((log '())
         (fresh (cl-sf/auth:make-connection :instance-url "https://org.my.salesforce.com")))
    (with-stubbed-logins (nil fresh log)
      (dotimes (i 2)
        (cl-sf/auth:with-conn :alias "org" :instance-url "https://org.my.salesforce.com"
          (is eq fresh cl-sf/auth:*connection*)))
      (is equal '((:browser "https://org.my.salesforce.com") (:cached "org"))
          log))))

(define-test with-conn/keywords-instance-url-only-skips-the-cache
  :parent auth
  (let* ((log '()) (fresh (cl-sf/auth:make-connection)))
    (with-stubbed-logins (nil fresh log)
      (cl-sf/auth:with-conn :instance-url "https://test.salesforce.com"
        (is eq fresh cl-sf/auth:*connection*))
      (is equal '((:browser "https://test.salesforce.com")) log))))

(define-test with-conn/cached-alias-for-a-different-instance-is-an-error
  :parent auth
  (let* ((log '())
         (cached (cl-sf/auth:make-connection :instance-url "https://prod.my.salesforce.com")))
    (with-stubbed-logins (cached nil log)
      (fail (cl-sf/auth:with-conn :alias "org" :instance-url "https://dev.my.salesforce.com" 1)
            'cl-sf/auth:sf-auth-error)
      (is equal '((:cached "org")) log))))

(define-test with-conn/keywords-need-an-alias-or-instance
  :parent auth
  (fail (cl-sf/auth::%with-conn-keys) 'cl-sf/auth:sf-auth-error))

(define-test with-conn/positional-form-still-works
  :parent auth
  (let ((conn (cl-sf/auth:make-connection)))
    (cl-sf/auth:with-conn conn
      (is eq conn cl-sf/auth:*connection*))))

(define-test oauth-redirect/query-params-are-decoded
  :parent auth
  (is equal '(("code" . "aPr==") ("state" . "abc"))
      (cl-sf/auth::%query-params "/OauthRedirect?code=aPr%3D%3D&state=abc")))

(defun http-get-localhost (port path)
  "Open a connection to localhost:PORT; send GET PATH when PATH is non-NIL.
   Returns the stream (so the caller can read the response)."
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect s #(127 0 0 1) port)
    (let ((st (sb-bsd-sockets:socket-make-stream s :input t :output t :buffering :full)))
      (when path
        (format st "GET ~A HTTP/1.1~C~%Host: localhost~C~%~C~%" path #\Return #\Return #\Return)
        (finish-output st))
      st)))

(defun run-redirect-listener (port state timeout)
  (sb-thread:make-thread
   (lambda () (handler-case (cl-sf/auth::%wait-for-oauth-redirect port state timeout)
                (cl-sf/auth:sf-auth-error (e) e)))))

(define-test oauth-redirect/returns-the-code-ignoring-stray-connections
  :parent auth
  (let ((listener (run-redirect-listener 17174 "st8" 20)))
    (sleep 0.5)
    (http-get-localhost 17174 nil)                   ; idle preconnect
    (http-get-localhost 17174 "/favicon.ico")        ; no code -> ignored
    (let ((response (http-get-localhost 17174 "/OauthRedirect?code=aPr%3D%3D&state=st8")))
      (true (search "200 OK" (read-line response nil))))
    (is equal "aPr==" (sb-thread:join-thread listener :timeout 15))))

(define-test oauth-redirect/rejects-a-wrong-state
  :parent auth
  (let ((listener (run-redirect-listener 17175 "st8" 20)))
    (sleep 0.5)
    (http-get-localhost 17175 "/OauthRedirect?code=x&state=evil")
    (true (typep (sb-thread:join-thread listener :timeout 15) 'cl-sf/auth:sf-auth-error))))

(define-test oauth-redirect/times-out
  :parent auth
  (true (typep (sb-thread:join-thread (run-redirect-listener 17176 "st8" 1) :timeout 15)
               'cl-sf/auth:sf-auth-error)))
