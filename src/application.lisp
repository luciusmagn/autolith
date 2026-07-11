(in-package #:frob)

;;;; -- Active Application --

(defclass application ()
  ((configuration
    :initarg :configuration
    :accessor application-configuration
    :type configuration
    :documentation "The current paths, model, and provider choices.")
   (conversation
    :initarg :conversation
    :accessor application-conversation
    :type conversation
    :documentation "The durable conversation currently shown to the user.")
   (provider
    :initarg :provider
    :accessor application-provider
    :type (option model-provider)
    :documentation "The reconnectable model provider.")
   (tool-registry
    :initarg :tool-registry
    :accessor application-tool-registry
    :type tool-registry
    :documentation "The live, checkpointed tool registry.")
   (worker
    :initarg :worker
    :accessor application-worker
    :type (option lisp-worker)
    :documentation "The reconnectable disposable Lisp worker.")
   (agent
    :initarg :agent
    :accessor application-agent
    :type (option agent)
    :documentation "The reconnectable provider and tool coordinator.")
   (ui
    :initarg :ui
    :accessor application-ui
    :type (option terminal-ui)
    :documentation "The reconnectable primary-screen terminal UI.")
   (rendered-sequence
    :initform 0
    :accessor application-rendered-sequence
    :type integer
    :documentation "The last durable conversation sequence printed to scrollback.")
   (presentation-counter
    :initform 0
    :accessor application-presentation-counter
    :type integer
    :documentation "The identifier source for non-conversation terminal notices."))
  (:documentation "The globally rooted logical state and reconnectable resources of Frob."))

(defvar *active-application* nil
  "The live application root retained in saved generations.")

(defvar *terminal-resize-pending-p* nil
  "True after SIGWINCH until the active UI recomputes its width.")


;;;; -- Construction and Reconnection --

(-> terminal-current-columns () integer)
(defun terminal-current-columns ()
  "Return the current terminal width, falling back to the restrained default."
  (labels ((positive-integer-or-nil (value)
             "Parse VALUE as a positive integer, returning NIL on failure."
             (handler-case
                 (let ((parsed (and (non-empty-string-p value)
                                    (parse-integer value :junk-allowed t))))
                   (and parsed (plusp parsed) parsed))
               (error ()
                 nil))))
    (or (positive-integer-or-nil (uiop:getenv "COLUMNS"))
        (and (interactive-stream-p *terminal-io*)
             (handler-case
                 (positive-integer-or-nil
                  (uiop:run-program '("tput" "cols")
                                    :output :string
                                    :error-output :output))
               (error ()
                 nil)))
        +terminal-default-columns+)))

(define-constant +application-prompt+ "❯ "
  :test #'string=
  :documentation "The styled input prompt shown on the live editor row.")

(define-constant +application-placeholder+
  "Ask Frob anything. Type /help for commands."
  :test #'string=
  :documentation "The dim hint shown on the prompt row while input is empty.")

(-> application-terminal-ui-create () terminal-ui)
(defun application-terminal-ui-create ()
  "Create the standard interactive terminal UI at the current terminal width."
  (terminal-ui-create
   :terminal (stream-terminal-create :columns (terminal-current-columns))
   :prompt +application-prompt+
   :placeholder +application-placeholder+))

(-> application-create
    (configuration &key (:conversation-id (option string)))
    application)
(defun application-create (configuration &key conversation-id)
  "Create a connected application, loading CONVERSATION-ID when supplied."
  (configuration-ensure-directories configuration)
  (durable-mutations-load configuration)
  (let* ((conversation (if conversation-id
                           (conversation-load-by-id configuration conversation-id)
                           (conversation-create configuration)))
         (provider (provider-create configuration))
         (registry (make-default-tool-registry))
         (worker (lisp-worker-create configuration))
         (agent (agent-create :configuration configuration
                              :provider provider
                              :conversation conversation
                              :tool-registry registry
                              :worker worker))
         (ui (application-terminal-ui-create)))
    (make-instance 'application
                   :configuration configuration
                   :conversation conversation
                   :provider provider
                   :tool-registry registry
                   :worker worker
                   :agent agent
                   :ui ui)))

(-> application-reconnect
    (application &key (:conversation-id (option string)))
    application)
(defun application-reconnect (application &key conversation-id)
  "Reconnect retained APPLICATION resources, optionally selecting CONVERSATION-ID."
  (let* ((previous (application-configuration application))
         (configuration
           (configuration-create
            :working-directory (uiop:getcwd)
            :model (configuration-model previous)
            :reasoning-effort (configuration-reasoning-effort previous)))
         (recovery-conversation-id
           (uiop:getenv "FROB_RECOVERY_CONVERSATION_ID"))
         (selected-conversation-id
           (or conversation-id
               (and (non-empty-string-p recovery-conversation-id)
                    recovery-conversation-id)
               (conversation-identifier
                (application-conversation application))))
         (conversation
           (conversation-load-by-id
            configuration
            selected-conversation-id))
         (provider (provider-create configuration))
         (worker (lisp-worker-create configuration))
         (registry (application-tool-registry application))
         (agent (agent-create :configuration configuration
                              :provider provider
                              :conversation conversation
                              :tool-registry registry
                              :worker worker))
         (ui (application-terminal-ui-create))
         (recovery-rendered-sequence
           (handler-case
               (let ((value (uiop:getenv "FROB_RECOVERY_RENDERED_SEQUENCE")))
                 (and (non-empty-string-p value)
                      (parse-integer value :junk-allowed nil)))
             (error ()
               nil))))
    (setf (application-configuration application) configuration
          (application-conversation application) conversation
          (application-provider application) provider
          (application-worker application) worker
          (application-agent application) agent
          (application-ui application) ui
          (application-rendered-sequence application)
          (if (and recovery-rendered-sequence
                   (string= selected-conversation-id
                            (or recovery-conversation-id "")))
              recovery-rendered-sequence
              0))
    application))

(defmethod checkpoint-detach-state ((application application))
  "Detach APPLICATION's ephemeral object graph in a checkpoint saver child."
  (setf (application-provider application) nil
        (application-worker application) nil
        (application-agent application) nil
        (application-ui application) nil)
  application)

(-> application-install-conversation (application conversation) application)
(defun application-install-conversation (application conversation)
  "Make CONVERSATION active in APPLICATION and reconnect its agent coordinator."
  (let ((agent
          (agent-create
           :configuration (application-configuration application)
           :provider (application-provider application)
           :conversation conversation
           :tool-registry (application-tool-registry application)
           :worker (application-worker application))))
    (setf (application-conversation application) conversation
          (application-agent application) agent
          (application-rendered-sequence application) 0)
    application))


;;;; -- Transcript Projection --

(-> response-item-text (json-object) (option string))
(defun response-item-text (item)
  "Return a human-readable transcript entry for completed provider ITEM."
  (let ((type (json-get item "type")))
    (cond
      ((and (string= (or type "") "message")
            (string= (or (json-get item "role") "") "assistant"))
       (let ((content (json-get item "content")))
         (when (vectorp content)
           (let ((parts
                   (loop for part across content
                         when (and (json-object-p part)
                                   (member (json-get part "type")
                                           '("output_text" "text")
                                           :test #'string=)
                                   (stringp (json-get part "text")))
                           collect (json-get part "text"))))
             (when parts
               (format nil "assistant~%~{~A~^~%~}" parts))))))
      ((string= (or type "") "function_call")
       (format nil "tool request~%~A~@[~%~A~]"
               (function-call-canonical-name item)
               (let ((arguments (json-get item "arguments")))
                 (and (non-empty-string-p arguments)
                      (bounded-string arguments :limit 2000)))))
      (t
       nil))))

(-> conversation-record-text (list) (option string))
(defun conversation-record-text (record)
  "Return the terminal transcript text represented by durable RECORD."
  (case (first record)
    (:message
     (when (eq (getf (rest record) :role) :user)
       (format nil "you~%~A" (getf (rest record) :content))))
    (:provider-item
     (let ((wire-json (getf (rest record) :wire-json)))
       (and (stringp wire-json)
            (response-item-text (json-decode wire-json)))))
    (:tool-result
     (format nil "tool result: ~A (~(~A~))~%~A"
             (getf (rest record) :tool)
             (getf (rest record) :status)
             (getf (rest record) :output)))
    (otherwise
     nil)))

(-> application-present (application string) boolean)
(defun application-present (application text)
  "Append non-conversation TEXT once to APPLICATION's normal scrollback."
  (let ((identifier (incf (application-presentation-counter application))))
    (terminal-ui-append-finalized
     (application-ui application)
     (list :presentation identifier)
     text)))

(-> application-render-records (application) null)
(defun application-render-records (application)
  "Append APPLICATION's not-yet-rendered durable transcript records once."
  (let* ((conversation (application-conversation application))
         (conversation-id (conversation-identifier conversation)))
    (dolist (record (rest (conversation--read-records
                           (conversation-pathname conversation))))
      (let ((sequence (getf (rest record) :seq)))
        (when (and (integerp sequence)
                   (> sequence (application-rendered-sequence application)))
          (let ((text (conversation-record-text record)))
            (when text
              (terminal-ui-append-finalized
               (application-ui application)
               (list :conversation conversation-id sequence)
               text)))
          (setf (application-rendered-sequence application) sequence)))))
  nil)


;;;; -- Agent Presentation --

(-> application-stream-status (application string string) null)
(defun application-stream-status (application label text)
  "Show LABEL and the bounded single-line tail of streaming TEXT."
  (let* ((safe (terminal-sanitize-text text :single-line-p t))
         (start (max 0 (- (length safe) 240))))
    (terminal-ui-set-status
     (application-ui application)
     (format nil "~A: ~A" label (subseq safe start))))
  nil)

(-> application-agent-observer (application) agent-observer)
(defun application-agent-observer (application)
  "Return a terminal observer for one APPLICATION user turn."
  (let ((assistant-tail "")
        (reasoning-tail ""))
    (callback-agent-observer-create
     :text-callback
     (lambda (delta)
       (setf assistant-tail
             (bounded-string (concatenate 'string assistant-tail delta)
                             :limit 500))
       (application-stream-status application "assistant" assistant-tail))
     :reasoning-callback
     (lambda (delta)
       (setf reasoning-tail
             (bounded-string (concatenate 'string reasoning-tail delta)
                             :limit 500))
       (application-stream-status application "thinking" reasoning-tail))
     :status-callback
     (lambda (status details)
       (case status
         (:provider-request-started
          (terminal-ui-set-status (application-ui application) "thinking"))
         (:tool-call-started
          (terminal-ui-set-status
           (application-ui application)
           (format nil "running ~A" (getf details :tool))))
         (:tool-call-completed
          (terminal-ui-set-status
           (application-ui application)
           (format nil "completed ~A" (getf details :tool))))
         (:turn-completed
          (terminal-ui-set-status (application-ui application) nil)))))))

(-> application-run-message (application string) null)
(defun application-run-message (application content)
  "Persist and run one model turn for CONTENT, presenting durable results once."
  (let* ((conversation (application-conversation application))
         (sequence (conversation-next-sequence conversation))
         (identifier (list :conversation
                           (conversation-identifier conversation)
                           sequence)))
    (terminal-ui-append-finalized
     (application-ui application)
     identifier
     (format nil "you~%~A" content))
    (setf (application-rendered-sequence application) sequence)
    (unwind-protect
         (progn
           (terminal-ui-set-status (application-ui application) "thinking")
           (agent-run-user-turn
            (application-agent application)
            content
            :observer (application-agent-observer application)))
      (terminal-ui-set-status (application-ui application) nil)
      (application-render-records application)))
  nil)
