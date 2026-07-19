(in-package #:autolith)

;;;; -- User Initialization Tests --

(defvar *user-init-test-value* nil
  "The value installed by the isolated user initialization fixture.")

(-> test-user-init () null)
(defun test-user-init ()
  "Test user initialization discovery, package binding, and typed failure."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (configuration-user-init-path configuration))
         (registrations (context--registry-snapshot)))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (test-assert (null (user-init-load configuration))
                        "a missing user init is an ordinary empty configuration")
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (write-string
              "(progn (setf *user-init-test-value* (list *user-init-loading-p* *package*)) (register-context-contributor \"user-init-test\" 'context-tests--next-request))"
              stream))
           (setf *user-init-test-value* nil)
           (test-assert (equal (user-init-load configuration) pathname)
                        "the configured user init pathname is loaded")
           (test-assert
            (and (first *user-init-test-value*)
                 (eq (second *user-init-test-value*)
                     (find-package '#:autolith)))
            "user init executes in the Autolith package and marked dynamic extent")
           (test-assert
            (eq (getf (find "user-init-test"
                            (context-contributor-registrations)
                            :test #'string=
                            :key (lambda (registration)
                                   (getf registration :identifier)))
                      :source)
                ':user)
            "contributors registered by user init retain their source")
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :external-format :utf-8)
             (write-string "(error \"broken user init\")" stream))
           (test-assert
            (handler-case
                (progn
                  (user-init-load configuration)
                  nil)
              (user-init-error (condition)
                (and (equal (user-init-error-pathname condition) pathname)
                     (typep (user-init-error-cause condition)
                            'serious-condition))))
            "a broken user init signals a structured startup condition")
           (test-assert
            (find "user-init-test" (context-contributor-registrations)
                  :test #'string=
                  :key (lambda (registration)
                         (getf registration :identifier)))
            "a failed reload restores the previous contributor registry")
           (delete-file pathname)
           (user-init-load configuration)
           (test-assert
            (null (find ':user (context-contributor-registrations)
                        :key (lambda (registration)
                               (getf registration :source))))
            "removing init.lisp removes its stale contributor registrations"))
      (setf *user-init-test-value* nil)
      (context--registry-restore registrations)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
