(in-package #:autolith)

;;;; -- Application Command Protocol Tests --

(-> application-command-tests--command
    (&key (:definition-name symbol) (:name string) (:aliases list)
          (:busy-behavior keyword) (:terminal-behavior keyword)
          (:handler function))
    application-command)
(defun application-command-tests--command
    (&key definition-name name aliases (busy-behavior ':inspect)
          (terminal-behavior ':shared)
          (handler (lambda (application invocation)
                     (declare (ignore application invocation))
                     ':continue)))
  "Return one complete command fixture with the supplied policy."
  (application-command-create
   :definition-name definition-name
   :name name
   :aliases aliases
   :argument nil
   :description "test command"
   :tip "exists for command protocol tests."
   :busy-behavior busy-behavior
   :terminal-behavior terminal-behavior
   :handler handler))

(-> application-command-tests--macroexpand-error-p (list) boolean)
(defun application-command-tests--macroexpand-error-p (form)
  "Return true when macroexpanding FORM signals an error."
  (handler-case
      (progn
        (macroexpand-1 form)
        nil)
    (error ()
      t)))

(-> test-application-command-defining-form () null)
(defun test-application-command-defining-form ()
  "Test literal command metadata and handler declarations fail closed."
  (let ((snapshot (application-command--registry-snapshot)))
    (dolist
        (form
         '((define-application-command application-command-tests--missing-tip
               (:name "/missing-tip"
                :argument nil
                :description "missing tip"
                :busy-behavior :inspect
                :terminal-behavior :shared)
               (application invocation)
             (declare (ignore application invocation))
             :continue)
           (define-application-command application-command-tests--blank-tip
               (:name "/blank-tip"
                :argument nil
                :description "blank tip"
                :tip "   "
                :busy-behavior :inspect
                :terminal-behavior :shared)
               (application invocation)
             (declare (ignore application invocation))
             :continue)
           (define-application-command application-command-tests--bad-busy
               (:name "/bad-busy"
                :argument nil
                :description "bad busy policy"
                :tip "has invalid policy."
                :busy-behavior :immediate
                :terminal-behavior :shared)
               (application invocation)
             (declare (ignore application invocation))
             :continue)
           (define-application-command application-command-tests--bad-terminal
               (:name "/bad-terminal"
                :argument nil
                :description "bad terminal policy"
                :tip "has invalid policy."
                :busy-behavior :inspect
                :terminal-behavior :sometimes)
               (application invocation)
             (declare (ignore application invocation))
             :continue)
           (define-application-command :keyword-definition
               (:name "/keyword"
                :argument nil
                :description "keyword identity"
                :tip "has invalid identity."
                :busy-behavior :inspect
                :terminal-behavior :shared)
               (application invocation)
             (declare (ignore application invocation))
             :continue)))
      (test-assert
       (application-command-tests--macroexpand-error-p form)
       "invalid command metadata fails during macro expansion"))
    (test-assert
     (equal snapshot (application-command--registry-snapshot))
     "macro expansion never mutates the live command registry"))
  nil)

(-> test-application-command-registry () null)
(defun test-application-command-registry ()
  "Test ordered replacement, layering, aliases, and collision atomicity."
  (let ((snapshot (application-command--registry-snapshot)))
    (unwind-protect
         (progn
           (application-command--registry-restore nil)
           (let* ((alpha
                    (application-command-tests--command
                     :definition-name
                     'application-command-tests--alpha
                     :name "/alpha"
                     :aliases '("/a")))
                  (beta
                    (application-command-tests--command
                     :definition-name
                     'application-command-tests--beta
                     :name "/beta"
                     :aliases nil))
                  (renamed
                    (application-command-tests--command
                     :definition-name
                     'application-command-tests--alpha
                     :name "/renamed"
                     :aliases '("/r"))))
             (register-application-command alpha :source ':runtime)
             (register-application-command beta :source ':runtime)
             (register-application-command renamed :source ':runtime)
             (test-assert
              (equal (mapcar #'application-command-name
                             (application-command-list))
                     '("/renamed" "/beta"))
              "redefining one command preserves its registry position")
             (test-assert
              (and (null (application-command-find "/alpha"))
                   (null (application-command-find "/a"))
                   (eq (application-command-find "/R") renamed))
              "renaming a command removes stale names and resolves aliases")
             (let ((shadow
                     (application-command-tests--command
                      :definition-name
                      'application-command-tests--shadow
                      :name "/renamed"
                      :aliases '("/shadow"))))
               (register-application-command shadow :source ':user)
               (test-assert
                (eq (application-command-find "/renamed") shadow)
                "a later layer shadows the same canonical command")
               (unregister-application-command
                'application-command-tests--shadow
                :source ':user)
               (test-assert
                (eq (application-command-find "/renamed") renamed)
                "removing a layer reveals the preceding command"))
             (let* ((before (application-command--registry-snapshot))
                    (collision
                      (application-command-tests--command
                       :definition-name
                       'application-command-tests--collision
                       :name "/collision"
                       :aliases '("/beta"))))
               (test-assert
                (handler-case
                    (progn
                      (register-application-command
                       collision
                       :source ':runtime)
                      nil)
                  (configuration-error ()
                    t))
                "an effective alias collision is rejected")
               (test-assert
                (equal before (application-command--registry-snapshot))
                "a rejected collision leaves every registry projection unchanged"))))
      (application-command--registry-restore snapshot)))
  nil)

(-> test-application-command-policies () null)
(defun test-application-command-policies ()
  "Test invocation parsing and invocation-sensitive command policies."
  (let ((snapshot (application-command--registry-snapshot)))
    (unwind-protect
         (progn
           (application-command--registry-restore nil)
           (dolist
               (command
                (list
                 (application-command-tests--command
                  :definition-name 'application-command-tests--inspect
                  :name "/inspect"
                  :aliases '("/i")
                  :busy-behavior ':inspect)
                 (application-command-tests--command
                  :definition-name 'application-command-tests--hold
                  :name "/hold"
                  :aliases nil
                  :busy-behavior ':hold
                  :terminal-behavior ':exclusive)
                 (application-command-tests--command
                  :definition-name 'application-command-tests--conditional
                  :name "/conditional"
                  :aliases nil
                  :busy-behavior ':cancel
                  :terminal-behavior ':exclusive-without-arguments)))
             (register-application-command command :source ':runtime))
           (let* ((inspection
                    (application-command-invocation-parse "  /I  "))
                  (inspection-with-argument
                    (application-command-invocation-parse
                     "/inspect alpha beta"))
                  (held
                    (application-command-invocation-parse "/hold"))
                  (conditional
                    (application-command-invocation-parse "/conditional value")))
             (test-assert
              (and
               (string= (application-command-invocation-name inspection) "/i")
               (string=
                (application-command-name
                 (application-command-invocation-command inspection))
                "/inspect")
               (string=
                (application-command-invocation-remainder
                 inspection-with-argument)
                "alpha beta")
               (string=
                (application-command-invocation-argument
                 inspection-with-argument)
                "alpha"))
              "command parsing preserves the full remainder and resolves aliases")
             (test-assert
              (and
               (eq (application-command-busy-action
                    (application-command-invocation-command inspection)
                    inspection)
                   ':execute)
               (eq (application-command-busy-action
                    (application-command-invocation-command
                     inspection-with-argument)
                    inspection-with-argument)
                   ':hold)
               (eq (application-command-busy-action
                    (application-command-invocation-command held)
                    held)
                   ':hold)
               (eq (application-command-busy-action
                    (application-command-invocation-command conditional)
                    conditional)
                   ':cancel))
              "busy behavior is declared by the command and refined by invocation")
             (test-assert
              (and
               (application-command-terminal-owner-p
                (application-command-invocation-command held)
                held)
               (not
                (application-command-terminal-owner-p
                 (application-command-invocation-command conditional)
                 conditional)))
              "terminal ownership follows each command's declared policy")))
      (application-command--registry-restore snapshot)))
  nil)

(-> test-built-in-application-command-policies () null)
(defun test-built-in-application-command-policies ()
  "Test every responsive built-in follows its declared active-turn policy."
  (dolist
      (case
       '(("/help" :execute)
         ("/conversations" :execute)
         ("/cwd" :execute)
         ("/cwd /tmp" :hold)
         ("/trace" :execute)
         ("/trace on" :hold)
         ("/goal" :execute)
         ("/goal pause" :hold)
         ("/agenda" :execute)
         ("/generations" :execute)
         ("/status" :execute)
         ("/usage" :execute)
         ("/context" :execute)
         ("/new" :hold)
         ("/compact" :hold)
         ("/quit" :cancel)
         ("/exit" :cancel)))
    (destructuring-bind (input expected) case
      (let* ((invocation (application-command-invocation-parse input))
             (command (application-command-invocation-command invocation)))
        (test-assert
         (and command
              (eq (application-command-busy-action command invocation)
                  expected))
         (format nil "~A has active-turn behavior ~S" input expected)))))
  (dolist
      (case
       '(("/model" t)
         ("/model gpt-5.6-sol" t)
         ("/resume" t)
         ("/resume K-8vQ2mp" nil)
         ("/effort" t)
         ("/effort high" nil)
         ("/permissions" t)
         ("/permissions list" nil)
         ("/rollback" t)
         ("/rollback generation" nil)
         ("/auth" t)
         ("/compact" nil)))
    (destructuring-bind (input expected) case
      (let* ((invocation (application-command-invocation-parse input))
             (command (application-command-invocation-command invocation)))
        (test-assert
         (eq (not
              (null
               (application-command-terminal-owner-p command invocation)))
             expected)
         (format nil "~A has terminal ownership ~S" input expected)))))
  nil)

(-> run-application-command-tests () boolean)
(defun run-application-command-tests ()
  "Run application command protocol tests and return true."
  (test-application-command-defining-form)
  (test-application-command-registry)
  (test-application-command-policies)
  (test-built-in-application-command-policies)
  t)
