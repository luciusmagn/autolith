(in-package #:autolith)

;;;; -- Responsive Terminal Input --

(defclass application-input-controller ()
  ((application
    :initarg :application
    :reader application-input-controller-application
    :type application
    :documentation "The application receiving terminal events and submitted work.")
   (lock
    :initform (make-lock "Autolith input controller")
    :reader application-input-controller-lock
    :type t
    :documentation "The lock protecting work, reader, and exit state.")
   (condition-variable
    :initform (make-condition-variable :name "Autolith input controller")
    :reader application-input-controller-condition-variable
    :type t
    :documentation "The main and reader thread wakeup condition.")
   (work-items
    :initform nil
    :accessor application-input-controller-work-items
    :type list
    :documentation "FIFO message and command work submitted by the reader.")
   (steering-items
    :initform nil
    :accessor application-input-controller-steering-items
    :type list
    :documentation "FIFO user messages waiting for the active turn's next tool boundary.")
   (active-p
    :initform nil
    :accessor application-input-controller-active-p
    :type boolean
    :documentation "Whether the main thread is processing one work item.")
   (stopping-p
    :initform nil
    :accessor application-input-controller-stopping-p
    :type boolean
    :documentation "Whether no more terminal input or work may be accepted.")
   (exit-reason
    :initform nil
    :accessor application-input-controller-exit-reason
    :type (option keyword)
    :documentation "The user-facing reason input processing stopped.")
   (reader-thread
    :initform nil
    :accessor application-input-controller-reader-thread
    :type t
    :documentation "The restartable terminal reader thread.")
   (reader-paused-p
    :initform nil
    :accessor application-input-controller-reader-paused-p
    :type boolean
    :documentation "Whether the reader must remain stopped for main-thread input.")
   (pause-depth
    :initform 0
    :accessor application-input-controller-pause-depth
    :type (integer 0)
    :documentation "Nested main-thread requests keeping the reader stopped.")
   (main-thread
    :initarg :main-thread
    :reader application-input-controller-main-thread
    :type t
    :documentation "The model and command thread interrupted for immediate exit.")
   (failure
    :initform nil
    :accessor application-input-controller-failure
    :type (option serious-condition)
    :documentation "A fatal terminal-reader condition awaiting main-thread handling.")
   (failure-backtrace
    :initform nil
    :accessor application-input-controller-failure-backtrace
    :type (option string)
    :documentation "The reader backtrace captured with FAILURE."))
  (:documentation
   "Ephemeral terminal input and FIFO submission state for one application run."))

(-> application--message-input (string) (option string))
(defun application--message-input (input)
  "Return INPUT's model message, or NIL when it is empty or a slash command."
  (cond
    ((not (non-empty-string-p input))
     nil)
    ((uiop:string-prefix-p "//" input)
     (subseq input 1))
    ((uiop:string-prefix-p "/" input)
     nil)
    (t
     input)))

(-> application--quit-command-p (string) boolean)
(defun application--quit-command-p (input)
  "Return true when INPUT is the explicit quit or exit slash command."
  (let ((command (string-downcase
                  (or (first (uiop:split-string
                              input
                              :separator '(#\Space #\Tab)))
                      ""))))
    (not (null (member command '("/quit" "/exit") :test #'string=)))))

(-> application--command-needs-terminal-owner-p (string) boolean)
(defun application--command-needs-terminal-owner-p (input)
  "Return true when command INPUT must read from or reconfigure the terminal."
  (let* ((parts (remove-if-not
                 #'non-empty-string-p
                 (uiop:split-string input :separator '(#\Space #\Tab))))
         (command (string-downcase (or (first parts) "")))
         (argument (second parts)))
    (or (string= command "/auth")
        (and (null argument)
             (not
              (null
               (member command
                       '("/resume" "/model" "/effort" "/rollback"
                         "/permissions")
                       :test #'string=)))))))

(-> application-input-controller--pending-input-count
    (application-input-controller)
    (integer 0))
(defun application-input-controller--pending-input-count (controller)
  "Return CONTROLLER's queued follow-up count while its lock is held."
  (length (application-input-controller-work-items controller)))

(-> application-input-controller--publish-counts
    (application-input-controller)
    null)
(defun application-input-controller--publish-counts (controller)
  "Publish CONTROLLER's steering and follow-up counts through its serialized UI."
  (with-lock-held ((application-input-controller-lock controller))
    (terminal-ui-set-input-counts
     (application-ui (application-input-controller-application controller))
     (length (application-input-controller-steering-items controller))
     (application-input-controller--pending-input-count controller)))
  nil)

(-> application-input-controller-turn-active-p
    (application-input-controller)
    boolean)
(defun application-input-controller-turn-active-p (controller)
  "Return true when CONTROLLER's main thread is processing one work item."
  (not
   (null
    (with-lock-held ((application-input-controller-lock controller))
      (application-input-controller-active-p controller)))))

(-> application-input-controller-busy-p
    (application-input-controller)
    boolean)
(defun application-input-controller-busy-p (controller)
  "Return true when CONTROLLER has active or pending application work."
  (not
   (null
    (with-lock-held ((application-input-controller-lock controller))
      (or (application-input-controller-active-p controller)
          (application-input-controller-work-items controller))))))

(-> application-input-controller--interrupt-main
    (application-input-controller condition)
    null)
(defun application-input-controller--interrupt-main (controller condition)
  "Signal CONDITION on CONTROLLER's main thread unless already running there."
  (let ((thread (application-input-controller-main-thread controller)))
    (unless (eq thread (current-thread))
      (when (thread-alive-p thread)
        (interrupt-thread thread (lambda () (error condition))))))
  nil)

(-> application-input-controller--record-failure
    (application-input-controller serious-condition (option string))
    null)
(defun application-input-controller--record-failure
    (controller condition backtrace)
  "Record reader CONDITION, discard pending work, and wake the main thread."
  (let ((active-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (application-input-controller-failure controller)
        (setf (application-input-controller-failure controller) condition
              (application-input-controller-failure-backtrace controller) backtrace
              (application-input-controller-work-items controller) nil
              (application-input-controller-steering-items controller) nil
              (application-input-controller-stopping-p controller) t))
      (setf active-p (application-input-controller-active-p controller))
      (condition-notify
       (application-input-controller-condition-variable controller)))
    (application-input-controller--publish-counts controller)
    (when active-p
      (handler-case
          (application-input-controller--interrupt-main
           controller
           (make-condition
            'application-input-failed
            :original-condition condition
            :backtrace backtrace))
        (error ()
          nil))))
  nil)

(-> application-input-controller--enqueue
    (application-input-controller keyword string)
    null)
(defun application-input-controller--enqueue (controller kind input)
  "Append one work item of KIND carrying INPUT to CONTROLLER."
  (with-lock-held ((application-input-controller-lock controller))
    (unless (application-input-controller-stopping-p controller)
      (setf (application-input-controller-work-items controller)
            (nconc (application-input-controller-work-items controller)
                   (list (list kind (copy-seq input)))))
      (condition-notify
       (application-input-controller-condition-variable controller))))
  (application-input-controller--publish-counts controller)
  nil)

(-> application-input-controller--enqueue-steering
    (application-input-controller string)
    null)
(defun application-input-controller--enqueue-steering (controller input)
  "Queue INPUT for the active turn, or promote it before follow-ups if that turn ended."
  (with-lock-held ((application-input-controller-lock controller))
    (unless (application-input-controller-stopping-p controller)
      (if (application-input-controller-active-p controller)
          (setf (application-input-controller-steering-items controller)
                (nconc (application-input-controller-steering-items controller)
                       (list (copy-seq input))))
          (push (list ':message (copy-seq input))
                (application-input-controller-work-items controller)))
      (condition-notify
       (application-input-controller-condition-variable controller))))
  (application-input-controller--publish-counts controller)
  nil)

(-> application-input-controller--take-steering
    (application-input-controller)
    list)
(defun application-input-controller--take-steering (controller)
  "Return and consume CONTROLLER's messages for the completed tool boundary."
  (let ((messages nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (application-input-controller-stopping-p controller)
        (setf messages (application-input-controller-steering-items controller)
              (application-input-controller-steering-items controller) nil)))
    (application-input-controller--publish-counts controller)
    messages))

(-> application-input-controller--request-exit
    (application-input-controller keyword)
    null)
(defun application-input-controller--request-exit (controller reason)
  "Stop CONTROLLER for REASON, discarding work and cancelling an active turn."
  (let ((active-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (unless (application-input-controller-exit-reason controller)
        (setf (application-input-controller-exit-reason controller) reason))
      (setf (application-input-controller-stopping-p controller) t
            (application-input-controller-work-items controller) nil
            (application-input-controller-steering-items controller) nil
            active-p (application-input-controller-active-p controller))
      (condition-notify
       (application-input-controller-condition-variable controller)))
    (application-input-controller--publish-counts controller)
    (when active-p
      (handler-case
          (application-input-controller--interrupt-main
           controller
           (make-condition 'application-turn-cancelled))
        (error ()
          nil))))
  nil)

(-> application-input-controller--hold-command
    (application-input-controller string)
    null)
(defun application-input-controller--hold-command (controller input)
  "Restore busy command INPUT and explain when it can be submitted."
  (let* ((application (application-input-controller-application controller))
         (ui (application-ui application)))
    (terminal-ui-set-input ui input)
    (application-present
     application
     (list
      (terminal-span
       ':hint
       "∙ command held until the current response finishes")
      (terminal-span ':plain (string #\Newline))
      (terminal-span
       ':dim
       "  Edit it now or press Enter again when idle."))))
  nil)

(-> application-input-controller--handle-submission
    (application-input-controller string &key (:steer-p boolean))
    null)
(defun application-input-controller--handle-submission
    (controller input &key steer-p)
  "Route submitted INPUT to model work, command work, or busy-command policy."
  (let ((message (application--message-input input)))
    (cond
      (message
       (if steer-p
           (application-input-controller--enqueue-steering controller message)
           (application-input-controller--enqueue controller ':message message)))
      ((not (non-empty-string-p input))
       nil)
      ((application-input-controller-busy-p controller)
       (if (application--quit-command-p input)
           (application-input-controller--request-exit controller ':quit)
           (application-input-controller--hold-command controller input)))
      (t
       (application-input-controller--enqueue controller ':command input))))
  nil)

(-> application-input-controller--handle-queue-submission
    (application-input-controller string)
    null)
(defun application-input-controller--handle-queue-submission (controller input)
  "Queue INPUT as post-turn message or command work."
  (let ((message (application--message-input input)))
    (cond
      (message
       (application-input-controller--enqueue controller ':message message))
      ((non-empty-string-p input)
       (application-input-controller--enqueue controller ':command input))))
  nil)

(-> application-input-controller--process-event
    (application-input-controller t)
    null)
(defun application-input-controller--process-event (controller event)
  "Apply terminal EVENT and publish any resulting work or exit request."
  (let ((ui (application-ui
             (application-input-controller-application controller)))
        (turn-active-p
          (application-input-controller-turn-active-p controller)))
    (multiple-value-bind (action payload)
        (terminal-ui-process-event
         ui event :queue-completion-p turn-active-p)
      (case action
        (:submit
         (application-input-controller--handle-submission
          controller payload :steer-p turn-active-p))
        (:queue
         (application-input-controller--handle-queue-submission
          controller payload))
        (:end-of-input
         (application-input-controller--request-exit controller ':end-of-input))
        (:interrupt
         (application-input-controller--request-exit controller ':interrupt)))))
  nil)

(-> application-input-controller--input-ready-p
    (application-input-controller)
    boolean)
(defun application-input-controller--input-ready-p (controller)
  "Apply pending resizes and report whether CONTROLLER's terminal has input."
  (let* ((ui (application-ui
              (application-input-controller-application controller)))
         (terminal (terminal-ui-terminal ui)))
    (terminal-ui-refresh-size ui #'application-pending-terminal-size)
    (terminal-ui-refresh-status ui)
    (if (terminal-input-ready-p terminal)
        t
        (progn
          (with-lock-held ((application-input-controller-lock controller))
            (unless (or (application-input-controller-stopping-p controller)
                        (application-input-controller-reader-paused-p controller))
              (condition-wait
               (application-input-controller-condition-variable controller)
               (application-input-controller-lock controller)
               :timeout 0.02)))
          nil))))

(-> application-input-controller--reader-loop
    (application-input-controller)
    null)
(defun application-input-controller--reader-loop (controller)
  "Read and process terminal events until pause, exit, or reader failure."
  (let ((signal-backtrace nil))
    (handler-bind
        ((serious-condition
           (lambda (condition)
             (declare (ignore condition))
             (setf signal-backtrace (application-safe-backtrace)))))
      (handler-case
          (loop
            (when
                (with-lock-held ((application-input-controller-lock controller))
                  (or (application-input-controller-stopping-p controller)
                      (application-input-controller-reader-paused-p controller)))
              (return))
            (when (application-input-controller--input-ready-p controller)
              (application-input-controller--process-event
               controller
               (application-read-terminal-event
                (application-ui
                 (application-input-controller-application controller))))))
        (serious-condition (condition)
          (application-input-controller--record-failure
           controller condition signal-backtrace)))))
  nil)

(-> application-input-controller--start-reader
    (application-input-controller)
    null)
(defun application-input-controller--start-reader (controller)
  "Start CONTROLLER's reader unless it is paused, stopping, or already live."
  (with-lock-held ((application-input-controller-lock controller))
    (unless (or (application-input-controller-stopping-p controller)
                (application-input-controller-reader-paused-p controller)
                (let ((thread
                        (application-input-controller-reader-thread controller)))
                  (and thread (thread-alive-p thread))))
      (setf (application-input-controller-reader-thread controller)
            (make-thread
             (lambda ()
               (application-input-controller--reader-loop controller))
             :name "Autolith terminal input"))))
  nil)

(-> application-input-controller--pause-reader
    (application-input-controller)
    null)
(defun application-input-controller--pause-reader (controller)
  "Stop and join CONTROLLER's reader without ending the application."
  (let ((thread nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf (application-input-controller-reader-paused-p controller) t
            thread (application-input-controller-reader-thread controller))
      (condition-notify
       (application-input-controller-condition-variable controller)))
    (when thread
      (join-thread thread)
      (with-lock-held ((application-input-controller-lock controller))
        (when (eq thread
                  (application-input-controller-reader-thread controller))
          (setf (application-input-controller-reader-thread controller) nil)))))
  nil)

(-> application-input-controller-call-with-reader-paused
    (application-input-controller function)
    t)
(defun application-input-controller-call-with-reader-paused
    (controller function)
  "Call FUNCTION while CONTROLLER has no live terminal reader."
  (let ((outermost-p nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf outermost-p
            (zerop (application-input-controller-pause-depth controller)))
      (incf (application-input-controller-pause-depth controller)))
    (when outermost-p
      (application-input-controller--pause-reader controller))
    (unwind-protect
         (funcall function)
      (let ((restart-p nil))
        (with-lock-held ((application-input-controller-lock controller))
          (decf (application-input-controller-pause-depth controller))
          (when (zerop (application-input-controller-pause-depth controller))
            (setf (application-input-controller-reader-paused-p controller) nil
                  restart-p
                  (not (application-input-controller-stopping-p controller)))))
        (when restart-p
          (application-input-controller--start-reader controller))))))

(-> application--command-authorization-items (string pathname) list)
(defun application--command-authorization-items (command directory)
  "Return the modal choices for COMMAND in DIRECTORY."
  (declare (ignore command))
  (list
   (list :name "once"
         :argument nil
         :description "allow once inside the workspace sandbox")
   (list :name "always"
         :argument nil
         :description
         (format nil "always allow this exact command in ~A"
                 (application--abbreviated-directory (namestring directory))))
   (list :name "sandbox"
         :argument nil
         :description "allow sandboxed commands for this session")
   (list :name "full"
         :argument nil
         :description "let it ride with full user privileges for this session")
   (list :name "deny"
         :argument nil
         :description "do not run the command")))

(-> application--ask-command-permission
    (application string pathname)
    keyword)
(defun application--ask-command-permission (application command directory)
  "Ask interactively how COMMAND may run in DIRECTORY, failing closed otherwise."
  (block nil
    (let* ((controller (application-input-controller application))
           (ui         (application-ui application)))
      (unless (and controller
                   ui
                   (terminal-interactive-p (terminal-ui-terminal ui)))
        (return ':deny))
      (let ((choice
              (application-input-controller-call-with-reader-paused
               controller
               (lambda ()
                 (terminal-ui-select
                  ui
                  :title
                  (format nil "run ~A"
                          (text-cell-prefix
                           (sanitize-text command :single-line-p t)
                           56))
                  :items (application--command-authorization-items
                          command directory)
                  :resize-callback #'application-pending-terminal-size)))))
        (cond
          ((string= (or choice "") "once")
           ':sandboxed)
          ((string= (or choice "") "always")
           (permissions-allow
            :configuration (application-configuration application)
            :state         (application-permission-state application)
            :command       command
            :directory     directory)
           ':sandboxed)
          ((string= (or choice "") "sandbox")
           (setf (application-permission-mode application) ':sandboxed)
           ':sandboxed)
          ((string= (or choice "") "full")
           (setf (application-permission-mode application) ':full-access)
           ':full-access)
          (t
           ':deny))))))

(-> application-authorize-command (application string pathname) keyword)
(defun application-authorize-command (application command directory)
  "Return the session, saved, or interactively selected permission for COMMAND."
  (case (application-permission-mode application)
    (:full-access
     ':full-access)
    (:sandboxed
     ':sandboxed)
    (:ask
     (if (permissions-allowed-p
          (application-permission-state application)
          command
          directory)
         ':sandboxed
         (application--ask-command-permission application command directory)))))

(-> application-input-controller-create
    (application)
    application-input-controller)
(defun application-input-controller-create (application)
  "Create CONTROLLER for APPLICATION and start its terminal reader."
  (let ((controller
          (make-instance 'application-input-controller
                         :application application
                         :main-thread (current-thread))))
    (setf (application-input-controller application) controller)
    (application-input-controller--start-reader controller)
    controller))

(-> application-input-controller--next-work
    (application-input-controller)
    (option list))
(defun application-input-controller--next-work (controller)
  "Wait for and return CONTROLLER's next work item, or NIL after exit."
  (let ((work nil))
    (with-lock-held ((application-input-controller-lock controller))
      (loop while (and (null (application-input-controller-work-items controller))
                       (null (application-input-controller-failure controller))
                       (not (application-input-controller-stopping-p controller)))
            do (condition-wait
                (application-input-controller-condition-variable controller)
                (application-input-controller-lock controller)))
      (when (application-input-controller-failure controller)
        (error
         'application-input-failed
         :original-condition (application-input-controller-failure controller)
         :backtrace (application-input-controller-failure-backtrace controller)))
      (unless (application-input-controller-stopping-p controller)
        (setf work (pop (application-input-controller-work-items controller))
              (application-input-controller-active-p controller) t)))
    (application-input-controller--publish-counts controller)
    work))

(-> application-input-controller--finish-work
    (application-input-controller)
    null)
(defun application-input-controller--finish-work (controller)
  "Finish current work and promote unconsumed steering before queued follow-ups."
  (with-lock-held ((application-input-controller-lock controller))
    (unless (application-input-controller-stopping-p controller)
      (let ((steering-items
              (application-input-controller-steering-items controller)))
        (when steering-items
          (setf (application-input-controller-work-items controller)
                (append (mapcar (lambda (input)
                                  (list ':message input))
                                steering-items)
                        (application-input-controller-work-items controller)))))
      (setf (application-input-controller-steering-items controller) nil))
    (setf (application-input-controller-active-p controller) nil)
    (condition-notify
     (application-input-controller-condition-variable controller)))
  (application-input-controller--publish-counts controller)
  nil)

(-> application-input-controller-stop (application-input-controller) null)
(defun application-input-controller-stop (controller)
  "Stop CONTROLLER, discard pending work, and join its terminal reader."
  (let ((thread nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf (application-input-controller-stopping-p controller) t
            (application-input-controller-reader-paused-p controller) t
            (application-input-controller-work-items controller) nil
            (application-input-controller-steering-items controller) nil
            (application-input-controller-active-p controller) nil
            thread (application-input-controller-reader-thread controller))
      (condition-notify
       (application-input-controller-condition-variable controller)))
    (terminal-ui-set-input-counts
     (application-ui (application-input-controller-application controller))
     0
     0)
    (when thread
      (join-thread thread)
      (with-lock-held ((application-input-controller-lock controller))
        (when (eq thread
                  (application-input-controller-reader-thread controller))
          (setf (application-input-controller-reader-thread controller) nil))))
    (let ((application (application-input-controller-application controller)))
      (when (eq controller (application-input-controller application))
        (setf (application-input-controller application) nil))))
  nil)

(-> application--run-message-input
    (application string &key (:steering-function (option function)))
    keyword)
(defun application--run-message-input
    (application input &key steering-function)
  "Run model INPUT with established expected, cancellation, and fatal handling."
  (let ((signal-backtrace nil))
    (handler-bind
        ((serious-condition
           (lambda (condition)
             (declare (ignore condition))
             (setf signal-backtrace (application-safe-backtrace)))))
      (handler-case
          (progn
            (application-run-message
             application input :steering-function steering-function)
            ':continue)
        (application-turn-cancelled (condition)
          (error condition))
        (application-input-failed (condition)
          (error condition))
        (rollback-requested (condition)
          (error condition))
        ((or agent-loop-error
             conversation-invariant-error
             response-stream-error
             active-image-corruption)
         (condition)
          (application-raise-fatal application condition signal-backtrace))
        (autolith-error (condition)
          (application-handle-expected-error application condition)
          ':continue)
        (serious-condition (condition)
          (application-raise-fatal application condition signal-backtrace))))))

(-> application--run-command-input (application string) keyword)
(defun application--run-command-input (application input)
  "Run command INPUT with established expected and fatal handling."
  (let ((signal-backtrace nil))
    (handler-bind
        ((serious-condition
           (lambda (condition)
             (declare (ignore condition))
             (setf signal-backtrace (application-safe-backtrace)))))
      (handler-case
          (application-handle-input application input)
        (rollback-requested (condition)
          (error condition))
        ((or agent-loop-error
             conversation-invariant-error
             response-stream-error
             active-image-corruption)
         (condition)
          (application-raise-fatal application condition signal-backtrace))
        (autolith-error (condition)
          (application-handle-expected-error application condition)
          ':continue)
        (serious-condition (condition)
          (application-raise-fatal application condition signal-backtrace))))))

(-> application-input-controller--run-work
    (application-input-controller list)
    null)
(defun application-input-controller--run-work (controller work)
  "Run one submitted WORK item on the application main thread."
  (let ((application (application-input-controller-application controller)))
    (case (first work)
      (:message
       (application--run-message-input
        application
        (second work)
        :steering-function
        (lambda ()
          (application-input-controller--take-steering controller))))
      (:command
       (let* ((input (second work))
              (result
                (if (application--command-needs-terminal-owner-p input)
                    (application-input-controller-call-with-reader-paused
                     controller
                     (lambda ()
                       (application--run-command-input application input)))
                    (application--run-command-input application input))))
         (when (eq result ':quit)
           (application-input-controller--request-exit controller ':quit))))))
  nil)
