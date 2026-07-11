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
                 "JSON object access preserves values"))
  (format t "~&~:D Frob tests passed.~%" *test-count*)
  t)
