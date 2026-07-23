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
         (context-registrations (context--registry-snapshot))
         (command-registrations (application-command--registry-snapshot))
         (mcp-registrations (mcp--registry-snapshot)))
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
              "(progn
                 (setf *user-init-test-value*
                       (list *user-init-loading-p* *package*))
                 (register-context-contributor
                  \"user-init-test\"
                  'context-tests--next-request)
                 (define-context-contributor user-init-tests--contributor
                     (request)
                   (declare (ignore request))
                   (make-context-contribution
                    :identifier \"user-init-transaction\"
                    :instruction \"old contributor\"))
                 (define-application-command user-init-tests--command
                     (:name \"/user-init-test\"
                      :argument nil
                      :description \"exercise user command registration\"
                      :tip \"exists only in the user-init test.\"
                      :busy-behavior :inspect
                      :terminal-behavior :shared)
                     (application invocation)
                   (declare (ignore application invocation))
                   :continue))"
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
           (let ((command (application-command-find "/user-init-test")))
             (test-assert
              (and command
                   (eq
                    (getf
                     (find
                      'user-init-tests--command
                      (application-command--registrations)
                      :key (lambda (registration)
                             (getf registration :definition-name)))
                     :source)
                    ':user))
              "commands registered by user init retain their source"))
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :external-format :utf-8)
             (write-string
              "(progn
                 (define-context-contributor user-init-tests--contributor
                     (request)
                   (declare (ignore request))
                   (make-context-contribution
                    :identifier \"user-init-transaction\"
                    :instruction \"new contributor\"))
                 (error \"broken user init\"))"
              stream))
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
           (let* ((registration
                    (find
                     "user-init-tests--contributor"
                     (context-contributor-registrations)
                     :test #'string=
                     :key (lambda (candidate)
                            (getf candidate :identifier))))
                  (contribution
                    (and registration
                         (funcall (getf registration :function) nil))))
             (test-assert
              (and contribution
                   (string=
                    (context-contribution-instruction contribution)
                    "old contributor"))
              "a failed reload restores the exact previous contributor function"))
           (test-assert
            (application-command-find "/user-init-test")
            "a failed reload restores the previous command registry")
           (delete-file pathname)
           (user-init-load configuration)
           (test-assert
            (null (find ':user (context-contributor-registrations)
                        :key (lambda (registration)
                               (getf registration :source))))
            "removing init.lisp removes its stale contributor registrations")
           (test-assert
            (null
             (find ':user
                   (application-command--registrations)
                   :key (lambda (registration)
                          (getf registration :source))))
            "removing init.lisp removes its stale command registrations"))
      (setf *user-init-test-value* nil)
      (context--registry-restore context-registrations)
      (application-command--registry-restore command-registrations)
      (mcp--registry-restore mcp-registrations)
      (when (fboundp 'user-init-tests--command)
        (fmakunbound 'user-init-tests--command))
      (when (fboundp 'user-init-tests--contributor)
        (fmakunbound 'user-init-tests--contributor))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
