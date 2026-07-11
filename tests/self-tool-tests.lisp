(in-package #:frob)

;;;; -- Subsystem Tests --

(-> test-self-target () integer)
(defun test-self-target ()
  "Return the baseline value used by active-image mutation tests."
  0)

(-> test-self-tools () null)
(defun test-self-tools ()
  "Test active definition installation, inspection, and form-aware persistence."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (merge-pathnames "definitions.lisp" root))
         (previous-function (symbol-function 'test-self-target)))
    (unwind-protect
         (progn
           (self-install-definition
            configuration
            "(defun test-self-target () \"Return the installed test value.\" 42)")
           (test-assert (= (test-self-target) 42)
                        "self definition installation mutates the active image")
           (test-assert
            (search "Return the installed test value."
                    (self-inspect-symbol 'test-self-target))
            "active-image inspection exposes function documentation")
           (test-assert
            (equal (definition-signature
                    '(defmethod sample-operation ((left string) right) left))
                   (definition-signature
                    '(defmethod sample-operation ((value string) ignored) value)))
            "method identity ignores parameter names while retaining specializers")
           (test-assert
            (definition-form-p '(defun (setf sample-value) (value object)
                                  (declare (ignore object))
                                  value))
            "definition identity accepts SETF function names")
           (test-assert (definition-form-p '(defparameter *sample-value* 42))
                        "durable definitions include mutable global parameters")
           (test-assert
            (handler-case
                (progn
                  (self-validate-commit-paths configuration
                                              (json-array "build-recovery"))
                  nil)
              (tool-error ()
                t))
            "normal self commits cannot replace the pristine recovery builder")
           (let ((original
                   (make-condition 'simple-error
                                   :format-control "original failure"
                                   :format-arguments nil)))
             (test-assert
              (handler-case
                  (progn
                    (self-restore-definition
                     "(defun test-self-target () 0)"
                     original
                     :installer
                     (lambda (definition source)
                       (declare (ignore definition source))
                       (error "restoration failure")))
                    nil)
                (active-image-corruption (condition)
                  (and (eq (active-image-corruption-original-condition condition)
                           original)
                       (typep
                        (active-image-corruption-restoration-condition condition)
                        'serious-condition))))
              "a restoration failure preserves both conditions and escapes tool handling"))
           (ensure-directories-exist pathname)
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (format stream
                     "; preserve this comment~%~%(defun first-definition () 1)~%~%(defun test-self-target () 0)~%"))
           (source-replace-definition
            pathname
            "(defun test-self-target () \"Persisted documentation.\" 84)")
           (let ((updated (uiop:read-file-string pathname)))
             (test-assert (search "; preserve this comment" updated)
                          "form-aware replacement preserves preceding comments")
             (test-assert (search "Persisted documentation." updated)
                          "form-aware replacement writes the complete definition")
             (test-assert (search "(defun first-definition () 1)" updated)
                          "form-aware replacement preserves neighboring forms")))
      (setf (symbol-function 'test-self-target) previous-function)
      (remhash (definition-key '(defun test-self-target () 0))
               *exploratory-definitions*)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-durable-self-mutation () null)
(defun test-durable-self-mutation ()
  "Test checked live installation, source persistence, commit, and durable journaling."
  (let* ((source-root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "frob-durable-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (configuration (test-configuration-for-source-root source-root))
         (source-pathname (merge-pathnames "src/definitions.lisp" source-root))
         (previous-function (symbol-function 'test-self-target))
         (active-check-count 0)
         (source-check-count 0)
         (expected-source-fragment "Return the durable baseline.")
         (checker
           (make-instance
            'callback-mutation-checker
            :active-callback
            (lambda (checked-configuration definition-source)
              (declare (ignore checked-configuration definition-source))
              (incf active-check-count)
              (test-assert
               (search expected-source-fragment
                       (uiop:read-file-string source-pathname))
               "active checks run before durable source replacement")
              "active checks passed")
            :source-callback
            (lambda (checked-configuration paths)
              (declare (ignore checked-configuration))
              (incf source-check-count)
              (test-assert (equal paths '("src/definitions.lisp"))
                           "source checks receive normalized explicit paths")
              (test-assert
               (search "Return the durable value."
                       (uiop:read-file-string source-pathname))
               "source checks run after durable source replacement")
              "source checks passed"))))
    (unwind-protect
         (progn
           (ensure-directories-exist source-pathname)
           (with-open-file (stream source-pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (format stream
                     "(in-package #:frob)~%~%(defun test-self-target () \"Return the durable baseline.\" 0)~%"))
           (self-git-command configuration '("init" "--quiet"))
           (self-git-command configuration '("config" "user.name" "Frob Test"))
           (self-git-command configuration
                             '("config" "user.email" "frob-test@example.invalid"))
           (self-git-command configuration '("add" "src/definitions.lisp"))
           (self-git-command configuration
                             '("commit" "--quiet" "-m" "Create baseline"))
           (let* ((conversation
                    (conversation-create configuration :identifier "durable-mutation"))
                  (context
                    (make-instance 'tool-context
                                   :configuration configuration
                                   :worker nil
                                   :conversation conversation
                                   :mutation-checker checker))
                  (failing-active-context
                    (make-instance
                     'tool-context
                     :configuration configuration
                     :worker nil
                     :conversation conversation
                     :mutation-checker
                     (make-instance
                      'callback-mutation-checker
                      :active-callback
                      (lambda (checked-configuration definition-source)
                        (declare (ignore checked-configuration definition-source))
                        (error "Injected active check failure."))
                      :source-callback
                      (lambda (checked-configuration paths)
                        (declare (ignore checked-configuration paths))
                        "unused"))))
                  (registry (make-default-tool-registry))
                  (persist-tool (tool-registry-find registry "self" "persist-definition"))
                  (commit-tool (tool-registry-find registry "self" "commit"))
                  (failed-persist-p
                    (handler-case
                        (progn
                          (tool-execute
                           persist-tool
                           failing-active-context
                           (json-object
                            "definition"
                            "(defun test-self-target () \"Return a rejected value.\" 13)"
                            "pathname" "src/definitions.lisp"))
                          nil)
                      (error ()
                        t)))
                  (failed-persist-restored-p (= (test-self-target) 0))
                  (failed-persist-source-unchanged-p
                    (search "Return the durable baseline."
                            (uiop:read-file-string source-pathname)))
                  (persist-result
                    (tool-execute
                     persist-tool
                     context
                     (json-object
                      "definition"
                      "(defun test-self-target () \"Return the durable value.\" 84)"
                      "pathname" "src/definitions.lisp")))
                  (mutation
                    (loop for value being the hash-values of *durable-mutations*
                          when (and
                                (string= (durable-mutation-target value)
                                         (definition-key '(defun test-self-target () 84)))
                                (eq (durable-mutation-phase value) :source-written))
                            return value)))
             (test-assert failed-persist-p
                          "a failing active check rejects durable persistence")
             (test-assert failed-persist-restored-p
                          "a rejected durable definition restores the active definition")
             (test-assert failed-persist-source-unchanged-p
              "a rejected durable definition leaves source unchanged")
             (test-assert (tool-result-success-p persist-result)
                          "durable persistence succeeds after active checks")
             (test-assert (= (test-self-target) 84)
                          "durable persistence installs the live definition")
             (test-assert (= active-check-count 1)
                          "durable persistence runs active checks exactly once")
             (test-assert mutation
                          "durable persistence retains an explicit source-written transaction")
             (let ((identifier (durable-mutation-identifier mutation)))
               (clrhash *durable-mutations*)
               (durable-mutations-load configuration)
               (setf mutation (gethash identifier *durable-mutations*))
               (test-assert
                (and mutation
                     (eq (durable-mutation-phase mutation) :source-written))
                "pending durable state reconstructs from the append-only journal"))
             (let* ((failing-source-context
                      (make-instance
                       'tool-context
                       :configuration configuration
                       :worker nil
                       :conversation conversation
                       :mutation-checker
                       (make-instance
                        'callback-mutation-checker
                        :active-callback
                        (lambda (checked-configuration definition-source)
                          (declare (ignore checked-configuration definition-source))
                          "unused")
                        :source-callback
                        (lambda (checked-configuration paths)
                          (declare (ignore checked-configuration paths))
                          (error "Injected source check failure.")))))
                    (baseline-commit
                      (string-trim
                       '(#\Space #\Tab #\Newline #\Return)
                       (self-git-command configuration '("rev-parse" "HEAD"))))
                    (failed-commit-p
                      (handler-case
                          (progn
                            (tool-execute
                             commit-tool
                             failing-source-context
                             (json-object
                              "title" "Reject durable test definition"
                              "paths" (json-array "src/definitions.lisp")))
                            nil)
                        (error ()
                          t)))
                    (failed-commit-left-git-p
                      (string= baseline-commit
                               (string-trim
                                '(#\Space #\Tab #\Newline #\Return)
                                (self-git-command configuration
                                                  '("rev-parse" "HEAD")))))
                    (failed-commit-left-pending-p
                      (eq (durable-mutation-phase mutation) :source-written))
                    (commit-result
                     (tool-execute
                      commit-tool
                      context
                      (json-object
                       "title" "Persist durable test definition"
                       "paths" (json-array "src/definitions.lisp")))))
               (test-assert failed-commit-p
                            "a failing clean-source check rejects self.commit")
               (test-assert failed-commit-left-git-p
                "a rejected self.commit leaves Git unchanged")
               (test-assert failed-commit-left-pending-p
                            "a rejected self.commit leaves its transaction pending")
               (test-assert (tool-result-success-p commit-result)
                            "self.commit creates the explicit checked commit")
               (test-assert (= source-check-count 1)
                            "self.commit runs clean-source checks exactly once")
               (test-assert (eq (durable-mutation-phase mutation) :durable)
                            "self.commit marks the matching transaction durable")
               (test-assert
                (string= (durable-mutation-git-commit mutation)
                         (string-trim
                          '(#\Space #\Tab #\Newline #\Return)
                          (self-git-command configuration '("rev-parse" "HEAD"))))
                "the durable journal records the exact Git commit")
               (setf expected-source-fragment "Return the durable value.")
               (tool-execute
                persist-tool
                context
                (json-object
                 "definition"
                 "(defun test-self-target () \"Return the reconciled value.\" 85)"
                 "pathname" "src/definitions.lisp"))
               (let* ((pending
                        (loop for value being the hash-values of *durable-mutations*
                              when (and
                                    (eq (durable-mutation-phase value)
                                        :source-written)
                                    (search "reconciled"
                                            (durable-mutation-proposed-source value)))
                                return value))
                      (identifier (durable-mutation-identifier pending)))
                 (self-git-command
                  configuration
                  '("commit" "--quiet" "-m" "Commit before journal"
                    "--only" "--" "src/definitions.lisp"))
                 (clrhash *durable-mutations*)
                 (durable-mutations-load configuration)
                 (let ((reconciled (gethash identifier *durable-mutations*)))
                   (test-assert
                    (and reconciled
                         (eq (durable-mutation-phase reconciled) :durable)
                         (non-empty-string-p
                          (durable-mutation-git-commit reconciled)))
                    "journal replay reconciles a crash after Git commit")))
               (setf expected-source-fragment "Return the reconciled value.")
               (tool-execute
                persist-tool
                context
                (json-object
                 "definition"
                 "(defun test-self-target () \"Return a drifting value.\" 86)"
                 "pathname" "src/definitions.lisp"))
               (let ((drifting
                       (loop for value being the hash-values of *durable-mutations*
                             when (and
                                   (eq (durable-mutation-phase value)
                                       :source-written)
                                   (search "drifting"
                                           (durable-mutation-proposed-source value)))
                               return value)))
                 (source-replace-definition
                  source-pathname
                  "(defun test-self-target () \"Return the reconciled value.\" 85)")
                 (durable-mutation-mark-paths
                  configuration
                  '("src/definitions.lisp")
                  (string-trim
                   '(#\Space #\Tab #\Newline #\Return)
                   (self-git-command configuration '("rev-parse" "HEAD"))))
                 (test-assert (eq (durable-mutation-phase drifting) :superseded)
                              "a committed path cannot bless a drifted definition")
                 (mutation-journal-append
                  configuration
                  (list :mutation
                        :kind :durable-definition
                        :id (durable-mutation-identifier drifting)
                        :target (durable-mutation-target drifting)
                        :pathname (durable-mutation-pathname drifting)
                        :previous (durable-mutation-previous-source drifting)
                        :proposed (durable-mutation-proposed-source drifting)
                        :base-commit (durable-mutation-base-commit drifting)
                        :result :installed))
                 (test-assert
                  (handler-case
                      (progn
                        (durable-mutations-load configuration)
                        nil)
                    (source-mutation-error ()
                      t))
                  "journal replay rejects an illegal durable transition")))))
      (setf (symbol-function 'test-self-target) previous-function)
      (let ((test-identifiers nil))
        (maphash
         (lambda (identifier mutation)
           (when (string= (durable-mutation-target mutation)
                          (definition-key '(defun test-self-target () 0)))
             (push identifier test-identifiers)))
         *durable-mutations*)
        (dolist (identifier test-identifiers)
          (remhash identifier *durable-mutations*)))
      (uiop:delete-directory-tree source-root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)
