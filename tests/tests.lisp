(in-package #:frob)

;;;; -- Minimal Test Harness --

(defvar *test-count* 0
  "The number of assertions attempted by the current test run.")

(-> test-assert (t string) null)
(defun test-assert (value description)
  "Record one assertion and signal an error when VALUE is false."
  (incf *test-count*)
  (unless value
    (error "Test failed: ~A" description))
  nil)

(-> test-configuration () configuration)
(defun test-configuration ()
  "Return an isolated configuration rooted in a fresh temporary directory."
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames
                 (format nil "frob-tests-~A/" (make-identifier))
                 (uiop:temporary-directory))))
         (source-root (asdf:system-source-directory :frob)))
    (make-instance 'configuration
                   :source-root source-root
                   :working-directory source-root
                   :data-root (merge-pathnames "data/" root)
                   :state-root (merge-pathnames "state/" root)
                   :cache-root (merge-pathnames "cache/" root)
                   :codex-auth-path (merge-pathnames "missing-auth.json" root)
                   :model +default-model+
                   :reasoning-effort +default-reasoning-effort+
                   :provider-endpoint +codex-responses-endpoint+)))

(-> test-configuration-root (configuration) pathname)
(defun test-configuration-root (configuration)
  "Return the common temporary root containing CONFIGURATION's data directory."
  (uiop:pathname-parent-directory-pathname
   (configuration-data-root configuration)))

(-> test-conversation-persistence () null)
(defun test-conversation-persistence ()
  "Test append-only conversation projection and incomplete-tail recovery."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration :identifier "test-turn"))
                (assistant-item
                  (json-object
                   "type" "message"
                   "role" "assistant"
                   "content" (json-array
                              (json-object "type" "output_text" "text" "hello")))))
           (conversation-append-user-message conversation "hi")
           (conversation-append-provider-item conversation assistant-item)
           (conversation-append-tool-result
            conversation "call-1" "lisp.eval" "42" t)
           (with-open-file (stream (conversation-pathname conversation)
                                   :direction :output
                                   :if-exists :append
                                   :external-format :utf-8)
             (write-string "(:incomplete" stream))
           (let ((loaded (conversation-load-by-id configuration "test-turn")))
             (test-assert (= (length (conversation-input-items loaded)) 3)
                          "conversation reload projects complete wire items")
             (test-assert (= (conversation-next-sequence loaded) 4)
                          "conversation reload restores its next sequence")
             (test-assert
              (string= (json-get (first (conversation-input-items loaded)) "role")
                       "user")
              "conversation reload preserves the first user message")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> run-tests () boolean)
(defun run-tests ()
  "Run Frob's dependency-free unit tests and return true on success."
  (setf *test-count* 0)
  (let ((configuration (configuration-create
                        :source-root (asdf:system-source-directory :frob)
                        :working-directory (asdf:system-source-directory :frob))))
    (test-assert (string= (configuration-model configuration) "gpt-5.6-sol")
                 "the default model is gpt-5.6-sol")
    (test-assert (string= (configuration-reasoning-effort configuration) "ultra")
                 "the default reasoning effort is ultra")
    (test-assert (string= (configuration-wire-effort configuration) "max")
                 "ultra maps to the provider max effort")
    (test-assert (= (json-get (json-object "answer" 42) "answer") 42)
                 "JSON object access preserves values")
    (test-conversation-persistence))
  (format t "~&~:D Frob tests passed.~%" *test-count*)
  t)
