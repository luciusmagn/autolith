(in-package #:frob)

;;;; -- UI Construction --

(-> terminal-ui-create
    (&key (:terminal terminal) (:editor (option line-editor)) (:prompt string)
          (:placeholder string))
    terminal-ui)
(defun terminal-ui-create (&key terminal editor (prompt "> ") (placeholder ""))
  "Create a scrollback-preserving UI for TERMINAL."
  (unless (typep terminal 'terminal)
    (error 'terminal-error
           :message "TERMINAL-UI-CREATE requires a terminal instance."
           :operation ':create-ui
           :cause nil))
  (make-instance 'terminal-ui
                 :terminal terminal
                 :editor (or editor (line-editor-create))
                 :prompt prompt
                 :placeholder placeholder))


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

(-> terminal--cursor-down (terminal integer) null)
(defun terminal--cursor-down (terminal rows)
  "Move TERMINAL downward by ROWS within the bounded live region."
  (when (plusp rows)
    (terminal--write terminal
                     (format nil "~C[~:DB"
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
  "Erase only UI's currently painted live rows, ending at the region's top row."
  (let ((terminal (terminal-ui-terminal ui))
        (total (terminal-ui-live-row-count ui)))
    (when (and (terminal-interactive-p terminal)
               (plusp total))
      (terminal--cursor-down terminal
                             (- total 1 (terminal-ui-live-cursor-row ui)))
      (loop for row from (1- total) downto 0
            do (terminal--write terminal (string #\Return))
               (terminal--write terminal +terminal-erase-line+)
               (when (plusp row)
                 (terminal--cursor-up terminal 1)))
      (setf (terminal-ui-live-row-count ui) 0
            (terminal-ui-live-cursor-row ui) 0)))
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

(-> terminal--write-row (terminal list) null)
(defun terminal--write-row (terminal spans)
  "Write one live row of single-line SPANS already sanitized by their builder."
  (dolist (span spans)
    (let ((sequence (and (terminal-styled-p terminal)
                         (terminal-style-sequence (terminal-span-style span)))))
      (when sequence
        (terminal--write terminal sequence))
      (terminal--write terminal (terminal-span-text span))
      (when sequence
        (terminal--write terminal +terminal-style-reset+))))
  nil)

(-> terminal-ui--prompt-row (terminal-ui) (values list integer))
(defun terminal-ui--prompt-row (ui)
  "Return UI's prompt row spans and cursor column within the terminal width."
  (let* ((terminal (terminal-ui-terminal ui))
         (columns (terminal-columns terminal))
         (row-width (max 0 (1- columns)))
         (editor (terminal-ui-editor ui))
         (safe-prompt (terminal-sanitize-text (terminal-ui-prompt ui)
                                              :single-line-p t))
         (visible-prompt (terminal--prefix-within-width safe-prompt row-width)))
    (if (and (zerop (length (line-editor-text editor)))
             (non-empty-string-p (terminal-ui-placeholder ui)))
        (values (terminal--clip-spans
                 (list (terminal-span :brand visible-prompt)
                       (terminal-span :hint (terminal-ui-placeholder ui)))
                 row-width)
                (terminal--text-width visible-prompt))
        (multiple-value-bind (row cursor-column)
            (line-editor-render editor (terminal-ui-prompt ui) columns)
          (let ((content-style (if (uiop:string-prefix-p
                                    "/" (line-editor-text editor))
                                   :user
                                   :plain)))
            (values (list (terminal-span :brand visible-prompt)
                          (terminal-span content-style
                                         (subseq row (length visible-prompt))))
                    cursor-column))))))

(-> terminal-ui--live-rows (terminal-ui) (values list integer integer))
(defun terminal-ui--live-rows (ui)
  "Return UI's live rows as span lists plus the cursor row index and column."
  (let* ((terminal (terminal-ui-terminal ui))
         (row-width (max 0 (1- (terminal-columns terminal))))
         (rows nil))
    (when (terminal-ui-status ui)
      (push (terminal--clip-spans
             (list (terminal-span :brand "∙ ")
                   (terminal-span :dim (terminal-ui-status ui)))
             row-width)
            rows)
      (push nil rows))
    (let ((prompt-row (length rows)))
      (multiple-value-bind (prompt-spans cursor-column)
          (terminal-ui--prompt-row ui)
        (push prompt-spans rows)
        (push nil rows)
        (values (nreverse rows)
                prompt-row
                (min cursor-column row-width))))))

(-> terminal-ui--paint-live (terminal-ui) null)
(defun terminal-ui--paint-live (ui)
  "Paint UI's bounded live rows below the transcript without touching scrollback."
  (let ((terminal (terminal-ui-terminal ui)))
    (when (terminal-interactive-p terminal)
      (multiple-value-bind (rows cursor-row cursor-column)
          (terminal-ui--live-rows ui)
        (loop for row in rows
              for index from 0
              do (terminal--write terminal (string #\Return))
                 (terminal--write terminal +terminal-erase-line+)
                 (terminal--write-row terminal row)
                 (when (< (1+ index) (length rows))
                   (terminal--write-newline terminal)))
        (terminal--cursor-up terminal (- (length rows) 1 cursor-row))
        (terminal--write terminal (string #\Return))
        (terminal--cursor-right terminal cursor-column)
        (setf (terminal-ui-live-row-count ui) (length rows)
              (terminal-ui-live-cursor-row ui) cursor-row))
      (terminal-flush terminal)))
  nil)

(-> terminal-ui--repaint-live (terminal-ui) null)
(defun terminal-ui--repaint-live (ui)
  "Clear and repaint only UI's bounded live region."
  (terminal-ui--clear-live ui)
  (terminal-ui--paint-live ui)
  nil)

(-> terminal-ui--write-finalized (terminal-ui (or string list)) null)
(defun terminal-ui--write-finalized (ui entry)
  "Write sanitized finalized ENTRY once, followed by one separating blank row."
  (let* ((terminal (terminal-ui-terminal ui))
         (spans (loop for span in (if (stringp entry)
                                      (list (terminal-span :plain entry))
                                      entry)
                      collect (terminal-span
                               (terminal-span-style span)
                               (terminal-sanitize-text
                                (terminal-span-text span))))))
    (dolist (span spans)
      (let ((sequence (and (terminal-styled-p terminal)
                           (terminal-style-sequence
                            (terminal-span-style span)))))
        (when sequence
          (terminal--write terminal sequence))
        (terminal--write-safe-text terminal (terminal-span-text span))
        (when sequence
          (terminal--write terminal +terminal-style-reset+))))
    (let ((last-text (if spans
                         (terminal-span-text (first (last spans)))
                         "")))
      (unless (and (plusp (length last-text))
                   (char= (char last-text (1- (length last-text))) #\Newline))
        (terminal--write-newline terminal)))
    (terminal--write-newline terminal)
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

(-> terminal-ui-append-finalized (terminal-ui t (or string list)) boolean)
(defun terminal-ui-append-finalized (ui identifier entry)
  "Append finalized transcript ENTRY once for IDENTIFIER and return true when emitted."
  (block nil
    (when (gethash identifier (terminal-ui-finalized-identifiers ui))
      (return nil))
    (setf (gethash identifier (terminal-ui-finalized-identifiers ui)) t)
    (terminal-ui--clear-live ui)
    (terminal-ui--write-finalized ui entry)
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
