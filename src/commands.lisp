(in-package #:frob)

;;;; -- Interactive Commands --

(-> application-help () string)
(defun application-help ()
  "Return the concise interactive command reference."
  (let ((label-width
          (loop for entry in +application-commands+
                maximize (length (terminal-completion-label entry)))))
    (format nil "~{~A~^~%~}"
            (loop for entry in +application-commands+
                  collect (format nil "~vA  ~A"
                                  label-width
                                  (terminal-completion-label entry)
                                  (getf entry :description))))))

(-> application-list-conversations (application) string)
(defun application-list-conversations (application)
  "Return known conversation identifiers newest first."
  (let ((pathnames (conversation-list (application-configuration application))))
    (if pathnames
        (format nil "conversations~%~{~A~%~}"
                (mapcar #'pathname-name pathnames))
        "No saved conversations exist.")))

(-> application-authenticate (application) null)
(defun application-authenticate (application)
  "Run Frob-owned device authentication outside raw terminal mode."
  (let* ((ui (application-ui application))
         (provider (application-provider application)))
    (unless (typep provider 'codex-subscription-provider)
      (error 'authentication-error
             :message "The active provider does not support ChatGPT device login."))
    (terminal-ui-stop ui)
    (unwind-protect
         (device-authentication-login
          (device-authentication-client-create)
          (provider-credential-manager provider)
          :stream *standard-output*
          :open-browser-p t)
      (terminal-ui-start ui))
    (application-present application "ChatGPT authentication was saved by Frob."))
  nil)

(-> application-checkpoint (application) null)
(defun application-checkpoint (application)
  "Begin a non-stopping retained generation for APPLICATION."
  (terminal-ui-set-status (application-ui application)
                          "checking source before checkpoint")
  (unwind-protect
       (let ((generation
               (checkpoint-create
                (checkpoint-backend-create
                 (application-configuration application)
                 (application-worker application)))))
         (application-present
          application
          (format nil "Checkpoint ~A is publishing in process ~D."
                  (generation-identifier generation)
                  (generation-coordinator-pid generation))))
    (terminal-ui-set-status (application-ui application) nil))
  nil)

(-> application-command (application string) keyword)
(defun application-command (application input)
  "Execute slash command INPUT for APPLICATION and return its loop action."
  (let* ((parts (remove-if-not
                 #'non-empty-string-p
                 (uiop:split-string input :separator '(#\Space #\Tab))))
         (command (string-downcase (or (first parts) "")))
         (argument (second parts))
         (configuration (application-configuration application)))
    (cond
      ((member command '("/quit" "/exit") :test #'string=)
       :quit)
      ((string= command "/help")
       (application-present application (application-help))
       :continue)
      ((string= command "/new")
       (application-install-conversation application
                                         (conversation-create configuration))
       (application-present
        application
        (format nil "Started conversation ~A."
                (conversation-identifier
                 (application-conversation application))))
       :continue)
      ((string= command "/resume")
       (unless (non-empty-string-p argument)
         (error 'conversation-error
                :message "Usage: /resume ID"
                :pathname (configuration-conversation-root configuration)
                :sequence nil))
       (application-install-conversation
        application
        (conversation-load-by-id configuration argument))
       (application-render-records application)
       :continue)
      ((string= command "/conversations")
       (application-present application
                            (application-list-conversations application))
       :continue)
      ((string= command "/auth")
       (application-authenticate application)
       :continue)
      ((string= command "/checkpoint")
       (application-checkpoint application)
       :continue)
      ((string= command "/generations")
       (application-present application
                            (generation-render-list configuration))
       :continue)
      ((string= command "/rollback")
       (unless (non-empty-string-p argument)
         (error 'checkpoint-error
                :message "Usage: /rollback ID"
                :stage ':selection
                :pathname nil))
       (let ((generation (generation-find configuration argument)))
         (unless generation
           (error 'checkpoint-error
                  :message (format nil "Unknown retained generation ~A." argument)
                  :stage ':selection
                  :pathname nil))
         (generation-select configuration generation)
         (application-present
          application
          (format nil "Selected ~A. Run frob --recovery to boot it." argument)))
       :continue)
      (t
       (application-present application
                            (format nil "Unknown command ~A. Use /help." command))
       :continue))))

(-> application-handle-input (application string) keyword)
(defun application-handle-input (application input)
  "Handle submitted INPUT and return :CONTINUE or :QUIT."
  (cond
    ((not (non-empty-string-p input))
     :continue)
    ((uiop:string-prefix-p "//" input)
     (application-run-message application (subseq input 1))
     :continue)
    ((uiop:string-prefix-p "/" input)
     (application-command application input))
    (t
     (application-run-message application input)
     :continue)))
