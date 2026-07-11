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
  "Return saved conversations newest first with their times and origins."
  (let ((items (application--conversation-items application)))
    (if items
        (format nil "conversations~%~{~A~%~}"
                (loop for item in items
                      collect (format nil "~A  ~A"
                                      (getf item :name)
                                      (getf item :description))))
        "No saved conversations exist.")))


;;;; -- Interactive Pickers --

(-> application--calendar-description (integer) string)
(defun application--calendar-description (universal-time)
  "Return UNIVERSAL-TIME as a compact local calendar description."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (declare (ignore second))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
            year month date hour minute)))

(-> application--abbreviated-directory ((option string)) (option string))
(defun application--abbreviated-directory (namestring)
  "Return NAMESTRING with the user home directory abbreviated to a tilde."
  (when (non-empty-string-p namestring)
    (let ((home (namestring (user-homedir-pathname))))
      (if (and (uiop:string-prefix-p home namestring)
               (> (length namestring) (length home)))
          (concatenate 'string "~/" (subseq namestring (length home)))
          namestring))))

(-> application--conversation-items (application) list)
(defun application--conversation-items (application)
  "Return picker items for saved conversations, newest first."
  (let ((current (conversation-identifier
                  (application-conversation application))))
    (loop for pathname in (conversation-list
                           (application-configuration application))
          for identifier = (pathname-name pathname)
          for header = (conversation-peek-header pathname)
          collect (list :name identifier
                        :argument nil
                        :description
                        (format nil "~A~@[, ~A~]~:[~;, current~]"
                                (application--calendar-description
                                 (or (file-write-date pathname) 0))
                                (application--abbreviated-directory
                                 (getf (rest header) :directory))
                                (string= identifier current))))))

(-> application--effort-items (application) list)
(defun application--effort-items (application)
  "Return picker items for the supported reasoning efforts."
  (let ((current (configuration-reasoning-effort
                  (application-configuration application))))
    (loop for effort in +supported-reasoning-efforts+
          collect (list :name effort
                        :argument nil
                        :description (if (string= effort current)
                                         "current"
                                         "")))))

(-> application-set-reasoning-effort (application string) null)
(defun application-set-reasoning-effort (application effort)
  "Switch APPLICATION to reasoning EFFORT for this session's next turns."
  (let* ((previous (application-configuration application))
         (configuration (configuration-create
                         :working-directory (configuration-working-directory
                                             previous)
                         :model (configuration-model previous)
                         :reasoning-effort effort))
         (provider (provider-create configuration))
         (agent (agent-create :configuration configuration
                              :provider provider
                              :conversation (application-conversation
                                             application)
                              :tool-registry (application-tool-registry
                                              application)
                              :worker (application-worker application))))
    (setf (application-configuration application) configuration
          (application-provider application) provider
          (application-agent application) agent))
  nil)

(-> application--generation-items (application) list)
(defun application--generation-items (application)
  "Return picker items for retained generations, newest first."
  (loop for generation in (generation-list
                           (application-configuration application))
        collect (list :name (generation-identifier generation)
                      :argument nil
                      :description
                      (format nil "~A~:[, incompatible~;~]"
                              (application--calendar-description
                               (generation-created-at generation))
                              (generation-compatible-p generation)))))

(-> application--pick-identifier
    (application &key (:title string) (:items list) (:usage string)
                 (:empty-notice string))
    (option string))
(defun application--pick-identifier
    (application &key (title "select") items (usage "") (empty-notice ""))
  "Pick one identifier from ITEMS interactively, or explain why none was picked.

Signals a usage error on non-interactive terminals, presents EMPTY-NOTICE
when ITEMS is empty, and returns NIL when the picker is cancelled."
  (block nil
    (let ((ui (application-ui application)))
      (unless (terminal-interactive-p (terminal-ui-terminal ui))
        (error 'configuration-error :message usage))
      (unless items
        (application-present application empty-notice)
        (return nil))
      (terminal-ui-select ui :title title :items items))))

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
       (let ((identifier
               (or argument
                   (application--pick-identifier
                    application
                    :title "resume conversation"
                    :items (application--conversation-items application)
                    :usage "Usage: /resume ID"
                    :empty-notice "No saved conversations exist."))))
         (when identifier
           (application-install-conversation
            application
            (conversation-load-by-id configuration identifier))
           (application-render-records application)))
       :continue)
      ((string= command "/conversations")
       (application-present application
                            (application-list-conversations application))
       :continue)
      ((string= command "/auth")
       (application-authenticate application)
       :continue)
      ((string= command "/effort")
       (let ((effort
               (or argument
                   (application--pick-identifier
                    application
                    :title "pick the reasoning effort"
                    :items (application--effort-items application)
                    :usage "Usage: /effort LEVEL"
                    :empty-notice "No supported reasoning efforts exist."))))
         (when effort
           (application-set-reasoning-effort application effort)
           (application-present
            application
            (format nil "Reasoning effort is now ~A."
                    (configuration-reasoning-effort
                     (application-configuration application))))))
       :continue)
      ((string= command "/checkpoint")
       (application-checkpoint application)
       :continue)
      ((string= command "/generations")
       (application-present application
                            (generation-render-list configuration))
       :continue)
      ((string= command "/rollback")
       (let ((identifier
               (or argument
                   (application--pick-identifier
                    application
                    :title "select a generation for recovery"
                    :items (application--generation-items application)
                    :usage "Usage: /rollback ID"
                    :empty-notice "No retained generations exist."))))
         (when identifier
           (let ((generation (generation-find configuration identifier)))
             (unless generation
               (error 'checkpoint-error
                      :message (format nil "Unknown retained generation ~A."
                                       identifier)
                      :stage ':selection
                      :pathname nil))
             (generation-select configuration generation)
             (application-present
              application
              (format nil "Selected ~A. Run frob --recovery to boot it."
                      identifier)))))
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
