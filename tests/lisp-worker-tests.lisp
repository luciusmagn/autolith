(in-package #:frob)

;;;; -- Subsystem Tests --

(-> test-lisp-worker-protocol () null)
(defun test-lisp-worker-protocol ()
  "Test portable worker request execution and condition reporting."
  (let ((success
          (worker-handle-request
           '(:request :id 1 :operation :eval :arguments (:form "(+ 20 22)"))))
        (failure
          (worker-handle-request
           '(:request :id 2 :operation :eval :arguments (:form "(/ 1 0)")))))
    (test-assert (eq (getf (rest success) :status) :ok)
                 "the worker evaluates a valid request")
    (test-assert (equal (getf (rest success) :values) '("42"))
                 "the worker returns rendered values")
    (test-assert (eq (getf (rest failure) :status) :error)
                 "the worker turns evaluation conditions into protocol errors")
    (test-assert (non-empty-string-p (getf (rest failure) :message))
                 "worker protocol errors carry a readable condition report"))
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (worker (lisp-worker-create configuration)))
    (unwind-protect
         (let ((response (lisp-worker-request worker :eval '(:form "(+ 40 2)"))))
           (test-assert (eq (getf (rest response) :status) :ok)
                        "the disposable worker starts through its direct active loader")
           (test-assert (equal (getf (rest response) :values) '("42"))
                        "the launched worker completes its isolated protocol request"))
      (lisp-worker-stop worker)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
