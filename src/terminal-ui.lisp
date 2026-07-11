(in-package #:frob)

;;;; -- UI Construction --

(-> terminal-ui-create
    (&key (:terminal terminal) (:editor (option line-editor)) (:prompt string))
    terminal-ui)
(defun terminal-ui-create (&key terminal editor (prompt "> "))
  "Create a scrollback-preserving UI for TERMINAL."
  (unless (typep terminal 'terminal)
    (error 'terminal-error
           :message "TERMINAL-UI-CREATE requires a terminal instance."
           :operation ':create-ui
           :cause nil))
  (make-instance 'terminal-ui
                 :terminal terminal
                 :editor (or editor (line-editor-create))
                 :prompt prompt))


;;;; -- Live Region Mechanics --

(-> terminal--cursor-up (terminal integer) null)
(defun terminal--cursor-up (terminal rows)
  "Move TERMINAL upward by ROWS within the bounded live region."
  (when (plusp rows)
    (terminal--write terminal
                     (format nil "~C[~:DA"
                             +terminal-escape-character+
                             rows)))
  nil)

(-> terminal--cursor-right (terminal integer) null)
(defun terminal--cursor-right (terminal columns)
  "Move TERMINAL right by COLUMNS within the current prompt row."
  (when (plusp columns)
    (terminal--write terminal
                     (format nil "~C[~:DC"
                             +terminal-escape-character+
                             columns)))
  nil)

(-> terminal-ui--clear-live (terminal-ui) null)
(defun terminal-ui--clear-live (ui)
  "Erase only UI's currently rendered status and prompt rows."
  (when (and (terminal-interactive-p (terminal-ui-terminal ui))
             (terminal-ui-live-rendered-p ui))
    (let ((terminal (terminal-ui-terminal ui)))
      (terminal--write terminal (string #\Return))
      (terminal--write terminal +terminal-erase-line+)
      (when (terminal-ui-rendered-status-p ui)
        (terminal--cursor-up terminal 1)
        (terminal--write terminal (string #\Return))
        (terminal--write terminal +terminal-erase-line+)))
    (setf (terminal-ui-live-rendered-p ui) nil
          (terminal-ui-rendered-status-p ui) nil))
  nil)

(-> terminal--write-newline (terminal) null)
(defun terminal--write-newline (terminal)
  "Write a line break that returns to column zero on interactive terminals."
  (terminal--write terminal
                   (if (terminal-interactive-p terminal)
                       (format nil "~C~C" #\Return #\Newline)
                       (string #\Newline)))
  nil)

(-> terminal--write-safe-text (terminal string) null)
(defun terminal--write-safe-text (terminal text)
  "Write sanitized TEXT while making its line endings terminal-safe."
  (let ((line-start 0))
    (loop for newline = (position #\Newline text :start line-start)
          while newline
          do (terminal--write terminal (subseq text line-start newline))
             (terminal--write-newline terminal)
             (setf line-start (1+ newline))
          finally (terminal--write terminal (subseq text line-start))))
  nil)

(-> terminal-ui--paint-live (terminal-ui) null)
(defun terminal-ui--paint-live (ui)
  "Render UI's optional status and one-row editor without touching scrollback."
  (let ((terminal (terminal-ui-terminal ui)))
    (when (terminal-interactive-p terminal)
      (let ((status (terminal-ui-status ui)))
        (when status
          (terminal--write terminal
                           (terminal--prefix-within-width
                            (terminal-sanitize-text status :single-line-p t)
                            (terminal-columns terminal)))
          (terminal--write-newline terminal)))
      (multiple-value-bind (row cursor-column)
          (line-editor-render (terminal-ui-editor ui)
                              (terminal-ui-prompt ui)
                              (terminal-columns terminal))
        (terminal--write terminal (string #\Return))
        (terminal--write terminal +terminal-erase-line+)
        (terminal--write terminal row)
        (terminal--write terminal (string #\Return))
        (terminal--cursor-right terminal cursor-column))
      (setf (terminal-ui-live-rendered-p ui) t
            (terminal-ui-rendered-status-p ui)
            (not (null (terminal-ui-status ui))))
      (terminal-flush terminal)))
  nil)

(-> terminal-ui--repaint-live (terminal-ui) null)
(defun terminal-ui--repaint-live (ui)
  "Clear and repaint only UI's bounded live region."
  (terminal-ui--clear-live ui)
  (terminal-ui--paint-live ui)
  nil)

(-> terminal-ui--write-finalized (terminal-ui string) null)
(defun terminal-ui--write-finalized (ui text)
  "Write sanitized finalized TEXT once at the live region's former position."
  (let* ((terminal (terminal-ui-terminal ui))
         (safe-text (terminal-sanitize-text text)))
    (terminal--write-safe-text terminal safe-text)
    (unless (and (plusp (length safe-text))
                 (char= (char safe-text (1- (length safe-text))) #\Newline))
      (terminal--write-newline terminal))
    (terminal-flush terminal))
  nil)


;;;; -- Public UI Lifecycle and Events --

(-> terminal-ui-start (terminal-ui) terminal-ui)
(defun terminal-ui-start (ui)
  "Start UI on the primary screen and render its bounded live region."
  (unless (terminal-ui-started-p ui)
    (terminal-start (terminal-ui-terminal ui))
    (setf (terminal-ui-started-p ui) t)
    (terminal-ui--paint-live ui))
  ui)

(-> terminal-ui-stop (terminal-ui) terminal-ui)
(defun terminal-ui-stop (ui)
  "Erase UI's unfinished rows and restore its terminal even after partial startup."
  (unwind-protect
       (when (terminal-ui-started-p ui)
         (terminal-ui--clear-live ui)
         (terminal-flush (terminal-ui-terminal ui)))
    (setf (terminal-ui-started-p ui) nil)
    (terminal-stop (terminal-ui-terminal ui)))
  ui)

(defmacro with-terminal-ui ((variable ui-form) &body body)
  "Bind VARIABLE to UI-FORM, run BODY, and always restore its terminal state."
  `(let ((,variable ,ui-form))
     (unwind-protect
          (progn
            (terminal-ui-start ,variable)
            (locally
              ,@body))
       (terminal-ui-stop ,variable))))

(-> terminal-ui-append-finalized (terminal-ui t string) boolean)
(defun terminal-ui-append-finalized (ui identifier text)
  "Append finalized transcript TEXT once for IDENTIFIER and return true when emitted."
  (block nil
    (when (gethash identifier (terminal-ui-finalized-identifiers ui))
      (return nil))
    (setf (gethash identifier (terminal-ui-finalized-identifiers ui)) t)
    (terminal-ui--clear-live ui)
    (terminal-ui--write-finalized ui text)
    (terminal-ui--paint-live ui)
    t))

(-> terminal-ui-set-status (terminal-ui (option string)) terminal-ui)
(defun terminal-ui-set-status (ui status)
  "Replace UI's unfinished one-row STATUS and repaint only the live region."
  (let ((safe-status (and status
                          (terminal-sanitize-text status :single-line-p t))))
    (unless (equal safe-status (terminal-ui-status ui))
      (terminal-ui--clear-live ui)
      (setf (terminal-ui-status ui) safe-status)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-resize (terminal-ui integer) terminal-ui)
(defun terminal-ui-resize (ui columns)
  "Set UI terminal width to positive COLUMNS and repaint only unfinished rows."
  (let ((new-columns (max 1 columns)))
    (unless (= new-columns (terminal-columns (terminal-ui-terminal ui)))
      (terminal-ui--clear-live ui)
      (setf (terminal-columns (terminal-ui-terminal ui)) new-columns)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-read-event (terminal-ui) t)
(defun terminal-ui-read-event (ui)
  "Read one semantic input event for UI without emitting fallback prompt controls."
  (terminal-read-event (terminal-ui-terminal ui)))

(-> terminal-ui-process-event
    (terminal-ui t)
    (values keyword (option string)))
(defun terminal-ui-process-event (ui event)
  "Apply EVENT to UI's editor, repaint live rows, and return its action and payload."
  (multiple-value-bind (action payload)
      (line-editor-handle-event (terminal-ui-editor ui) event)
    (when (member action '(:changed :cleared :submit))
      (terminal-ui--repaint-live ui))
    (values action payload)))
