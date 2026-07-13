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
                       '("/resume" "/model" "/effort" "/rollback")
                       :test #'string=)))))))

(-> application-input-controller--pending-message-count
    (application-input-controller)
    (integer 0))
(defun application-input-controller--pending-message-count (controller)
  "Return CONTROLLER's queued message count while its lock is held."
  (count ':message
         (application-input-controller-work-items controller)
         :key #'first))

(-> application-input-controller--publish-count
    (application-input-controller)
    null)
(defun application-input-controller--publish-count (controller)
  "Publish CONTROLLER's queued message count through its serialized UI."
  (let ((count
          (with-lock-held ((application-input-controller-lock controller))
            (application-input-controller--pending-message-count controller))))
    (terminal-ui-set-queued-input-count
     (application-ui (application-input-controller-application controller))
     count))
  nil)

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
              (application-input-controller-stopping-p controller) t))
      (setf active-p (application-input-controller-active-p controller))
      (condition-notify
       (application-input-controller-condition-variable controller)))
    (application-input-controller--publish-count controller)
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
  (application-input-controller--publish-count controller)
  nil)

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
            active-p (application-input-controller-active-p controller))
      (condition-notify
       (application-input-controller-condition-variable controller)))
    (application-input-controller--publish-count controller)
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
    (application-input-controller string)
    null)
(defun application-input-controller--handle-submission (controller input)
  "Route submitted INPUT to model work, command work, or busy-command policy."
  (let ((message (application--message-input input)))
    (cond
      (message
       (application-input-controller--enqueue controller ':message message))
      ((not (non-empty-string-p input))
       nil)
      ((application-input-controller-busy-p controller)
       (if (application--quit-command-p input)
           (application-input-controller--request-exit controller ':quit)
           (application-input-controller--hold-command controller input)))
      (t
       (application-input-controller--enqueue controller ':command input))))
  nil)

(-> application-input-controller--process-event
    (application-input-controller t)
    null)
(defun application-input-controller--process-event (controller event)
  "Apply terminal EVENT and publish any resulting work or exit request."
  (let ((ui (application-ui
             (application-input-controller-application controller))))
    (multiple-value-bind (action payload)
        (terminal-ui-process-event ui event)
      (case action
        (:submit
         (application-input-controller--handle-submission controller payload))
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

(-> application-input-controller-create
    (application)
    application-input-controller)
(defun application-input-controller-create (application)
  "Create CONTROLLER for APPLICATION and start its terminal reader."
  (let ((controller
          (make-instance 'application-input-controller
                         :application application
                         :main-thread (current-thread))))
    (application-input-controller--start-reader controller)
    controller))

(-> application-input-controller--next-work
    (application-input-controller)
    (option list))
(defun application-input-controller--next-work (controller)
  "Wait for and return CONTROLLER's next work item, or NIL after exit."
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
    (when (application-input-controller-stopping-p controller)
      (return-from application-input-controller--next-work nil))
    (let ((work (pop (application-input-controller-work-items controller))))
      (setf (application-input-controller-active-p controller) t)
      work)))

(-> application-input-controller--finish-work
    (application-input-controller)
    null)
(defun application-input-controller--finish-work (controller)
  "Mark CONTROLLER's current main-thread work finished."
  (with-lock-held ((application-input-controller-lock controller))
    (setf (application-input-controller-active-p controller) nil)
    (condition-notify
     (application-input-controller-condition-variable controller)))
  (application-input-controller--publish-count controller)
  nil)

(-> application-input-controller-stop (application-input-controller) null)
(defun application-input-controller-stop (controller)
  "Stop CONTROLLER, discard pending work, and join its terminal reader."
  (let ((thread nil))
    (with-lock-held ((application-input-controller-lock controller))
      (setf (application-input-controller-stopping-p controller) t
            (application-input-controller-reader-paused-p controller) t
            (application-input-controller-work-items controller) nil
            (application-input-controller-active-p controller) nil
            thread (application-input-controller-reader-thread controller))
      (condition-notify
       (application-input-controller-condition-variable controller)))
    (terminal-ui-set-queued-input-count
     (application-ui (application-input-controller-application controller))
     0)
    (when thread
      (join-thread thread)
      (with-lock-held ((application-input-controller-lock controller))
        (when (eq thread
                  (application-input-controller-reader-thread controller))
          (setf (application-input-controller-reader-thread controller) nil)))))
  nil)

(-> application--run-message-input (application string) keyword)
(defun application--run-message-input (application input)
  "Run model INPUT with established expected, cancellation, and fatal handling."
  (let ((signal-backtrace nil))
    (handler-bind
        ((serious-condition
           (lambda (condition)
             (declare (ignore condition))
             (setf signal-backtrace (application-safe-backtrace)))))
      (handler-case
          (progn
            (application-run-message application input)
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
       (application--run-message-input application (second work)))
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
