(in-package #:autolith)

;;;; -- Subsystem Tests --

(-> authentication-tests--test-secret-use-quiescence () null)
(defun authentication-tests--test-secret-use-quiescence ()
  "Test checkpoint quiescence rejects new work without blocking its owner."
  (let ((child-result nil))
    (call-with-secret-use-quiescence
     (lambda ()
       (call-with-secret-use
        (lambda ()
          (test-assert
           (secret-use-active-p)
           "the quiescence owner may perform nested secret-bearing cleanup")))
       (let ((thread
               (make-thread
                (lambda ()
                  (setf
                   child-result
                   (handler-case
                       (progn
                         (call-with-secret-use (lambda () nil))
                         ':unexpected-success)
                     (authentication-error ()
                       ':rejected))))
                :name "Autolith secret-use quiescence test")))
         (join-thread thread))))
    (test-assert
     (eq child-result ':rejected)
     "checkpoint quiescence rejects a new secret user on another thread")
    (test-assert
     (not (secret-use-active-p))
     "checkpoint quiescence leaves no active secret-use count")
    (test-assert
     (null *secret-use-quiescence-owner*)
     "checkpoint quiescence releases its owner after success"))
  (let* ((lock (make-lock "Autolith existing secret-use test"))
         (condition
           (make-condition-variable
            :name "Autolith existing secret-use test"))
         (ready-p nil)
         (continue-p nil)
         (nested-use-observed-p nil)
         (thread
           (make-thread
            (lambda ()
              (call-with-secret-use
               (lambda ()
                 (with-lock-held (lock)
                   (setf ready-p t)
                   (condition-notify condition)
                   (loop until continue-p
                         do (condition-wait condition lock)))
                 (call-with-secret-use
                  (lambda ()
                    (setf nested-use-observed-p
                          (secret-use-active-p)))))))
            :name "Autolith existing secret-use test")))
    (with-lock-held (lock)
      (loop until ready-p
            do (condition-wait condition lock)))
    (call-with-secret-use-quiescence
     (lambda ()
       (with-lock-held (lock)
         (setf continue-p t)
         (condition-notify condition))
       (join-thread thread)))
    (test-assert
     nested-use-observed-p
     "a pre-existing secret operation may enter nested cleanup during quiescence"))
  (handler-case
      (call-with-secret-use-quiescence
       (lambda ()
         (error "synthetic quiescence failure")))
    (simple-error ()
      nil))
  (test-assert
   (null *secret-use-quiescence-owner*)
   "checkpoint quiescence releases its owner during unwinding")
  (test-assert
   (string=
    (redact-exact-string-values
     "unchanged"
    (list "" nil)
    "[REDACTED]")
   "unchanged")
   "exact redaction ignores empty and absent secret values")
  (let* ((secrets '("]x" "Z"))
         (marker (safe-redaction-marker "[REDACTED]" secrets))
         (redacted
           (redact-exact-string-values "Zx" secrets marker)))
    (test-assert
     (notany (lambda (secret)
               (search secret redacted))
             secrets)
     "redaction markers cannot form an earlier secret across a boundary"))
  nil)

(-> authentication-tests--test-oauth-error-code () null)
(defun authentication-tests--test-oauth-error-code ()
  "Test OAuth error extraction rejects malformed values and bounds strings."
  (dolist (body
            (list
             (json-encode (json-object "error" 42))
             (json-encode
              (json-object "error" (json-object "code" 42)))
             (json-encode
              (json-object "error"
                           (json-object "type" (json-object "nested" t))))
             (json-encode (json-object "error" "   "))
             "not-json"))
    (test-assert
     (null (oauth-error-code body))
     "OAuth error extraction rejects a malformed or empty code"))
  (let* ((unbounded (make-string 300 :initial-element #\x))
         (code
           (oauth-error-code
            (json-encode (json-object "error" unbounded)))))
    (test-assert
     (and code (= (length code) 256))
     "OAuth error extraction bounds an untrusted string code"))
  nil)

(-> test-authentication-store () null)
(defun test-authentication-store ()
  "Test private credential storage without exposing real authentication data."
  (authentication-tests--test-secret-use-quiescence)
  (authentication-tests--test-oauth-error-code)
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (source (make-instance 'autolith-credential-source
                                :pathname (configuration-auth-path configuration)))
         (credentials (make-instance 'oauth-credentials
                                     :access-token "test-access-token"
                                     :refresh-token "test-refresh-token"
                                     :id-token nil
                                     :account-id "test-account"
                                     :expires-at nil
                                     :source-path (configuration-auth-path configuration))))
    (unwind-protect
         (progn
           (test-assert
            (equal (configuration-auth-path configuration)
                   (merge-pathnames "auth.sexp"
                                    (configuration-state-root configuration)))
            "private credentials live under the state root")
           (credential-source-save source credentials)
           (let* ((loaded (credential-source-load source))
                  (mode (sb-posix:stat-mode
                         (sb-posix:stat
                          (namestring (configuration-auth-path configuration))))))
             (test-assert
              (string= (oauth-credentials-account-id loaded) "test-account")
              "the private credential store round-trips its account")
             (test-assert (= (logand mode #o777) #o600)
                          "the private credential store has mode 0600")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-write-codex-auth
    (pathname &key (:auth-mode string) (:account-id string) (:access-token string))
    null)
(defun test-write-codex-auth (pathname &key auth-mode account-id access-token)
  "Write a synthetic Codex credential document to PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string
     (json-encode
      (json-object
       "auth_mode" auth-mode
       "tokens" (json-object
                  "access_token" access-token
                  "refresh_token" "must-not-be-imported"
                  "account_id" account-id)))
     stream))
  nil)

(-> test-account-jwt (string) string)
(defun test-account-jwt (account-id)
  "Return a synthetic unsigned JWT carrying ACCOUNT-ID."
  (format nil
          "e30.~A.signature"
          (cl-base64:string-to-base64-string
           (json-encode (json-object "chatgpt_account_id" account-id))
           :uri t)))

(-> test-authentication-bootstrap-and-refresh () null)
(defun test-authentication-bootstrap-and-refresh ()
  "Test one-way Codex bootstrap import, account continuity, and refresh parsing."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (bootstrap-pathname (configuration-codex-auth-path configuration))
         (manager (credential-manager-create configuration)))
    (unwind-protect
         (progn
           (test-write-codex-auth bootstrap-pathname
                                  :auth-mode "apikey"
                                  :account-id "account-a"
                                  :access-token "bootstrap-a")
           (test-assert
            (null (credential-source-load
                   (credential-manager-bootstrap-source manager)))
            "Codex bootstrap rejects non-ChatGPT authentication modes")
           (test-write-codex-auth bootstrap-pathname
                                  :auth-mode "chatgpt"
                                  :account-id "account-a"
                                  :access-token "bootstrap-a")
           (let ((imported (credential-manager-load manager)))
             (test-assert
              (string= (oauth-credentials-account-id imported) "account-a")
              "the initial ChatGPT bootstrap account is imported")
             (test-assert (null (oauth-credentials-refresh-token imported))
                          "the Codex refresh token is never imported")
             (test-assert
              (equal (oauth-credentials-source-path imported)
                     (configuration-auth-path configuration))
              "bootstrap access is copied into Autolith's private store"))
           (test-write-codex-auth bootstrap-pathname
                                  :auth-mode "chatgpt"
                                  :account-id "account-b"
                                  :access-token "bootstrap-b")
           (let ((loaded (credential-manager-load manager)))
             (test-assert
              (string= (oauth-credentials-account-id loaded) "account-a")
              "subsequent loads ignore changes to the Codex bootstrap store")
             (test-assert
              (string= (oauth-credentials-access-token loaded) "bootstrap-a")
              "Autolith requests depend only on the imported private credential"))
           (test-assert
            (handler-case
                (progn
                  (credential-manager-refresh manager
                                              (credential-manager-load manager))
                  nil)
              (token-refresh-failed ()
                t))
            "non-renewable bootstrap credentials require Autolith's device flow")
           (let* ((primary-source (credential-manager-primary-source manager))
                  (renewable
                    (make-instance 'oauth-credentials
                                   :access-token "old-access"
                                   :refresh-token "old-refresh"
                                   :id-token nil
                                   :account-id "account-a"
                                   :expires-at nil
                                   :source-path
                                   (credential-source-pathname primary-source)))
                  (valid
                    (oauth-refresh-response-credentials
                     manager
                     renewable
                     (json-encode
                      (json-object "access_token" "new-access"
                                   "refresh_token" "new-refresh")))))
             (test-assert
              (string= (oauth-credentials-access-token valid) "new-access")
              "a validated refresh response yields new access credentials")
             (test-assert
              (string= (oauth-credentials-account-id valid) "account-a")
              "refresh without an account claim preserves the pinned account")
             (dolist (body '("not-json" "{}"))
               (test-assert
                (handler-case
                    (progn
                      (oauth-refresh-response-credentials manager renewable body)
                      nil)
                  (token-refresh-failed ()
                    t))
                "malformed refresh success bodies become typed failures"))
             (test-assert
              (handler-case
                  (progn
                    (oauth-refresh-response-credentials
                     manager
                     renewable
                     (json-encode
                      (json-object
                       "access_token" (test-account-jwt "account-b")
                       "refresh_token" "new-refresh")))
                    nil)
                (token-refresh-failed ()
                  t))
              "refresh rejects a token that switches ChatGPT accounts")
             (credential-source-save primary-source renewable)
             (let ((condition
                     (handler-case
                         (test-call-with-function-replacements
                          (list
                           (list
                            'dexador:post
                            (lambda (url &rest arguments)
                              (declare (ignore url arguments))
                              (error
                               (make-condition
                                'http-request-failed
                                :body
                                (json-encode
                                 (json-object "error" "old-refresh"))
                                :status 400
                                :headers nil
                                :uri nil
                                :method :post)))))
                          (lambda ()
                            (credential-manager-refresh manager renewable)))
                       (token-refresh-failed (failure)
                         failure))))
               (test-assert
                (and
                 condition
                 (not
                  (test-object-contains-string-p
                   condition
                   "old-refresh"))
                 (test-object-contains-string-p
                  condition
                  "[OAUTH CREDENTIAL REDACTED]"))
                "OAuth failure diagnostics redact an echoed refresh token"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
