(in-package #:frob)

;;;; -- Line Editor Mechanics --

(-> line-editor--set-state (line-editor string integer) line-editor)
(defun line-editor--set-state (editor text cursor)
  "Replace EDITOR's TEXT and CURSOR while preserving its history."
  (setf (slot-value editor 'text) text
        (slot-value editor 'cursor) (min (max 0 cursor) (length text)))
  editor)

(-> line-editor--leave-history (line-editor) null)
(defun line-editor--leave-history (editor)
  "Leave EDITOR's history navigation after a direct edit."
  (setf (slot-value editor 'history-index) -1
        (slot-value editor 'history-draft) nil)
  nil)

(-> line-editor--insert (line-editor string) null)
(defun line-editor--insert (editor inserted-text)
  "Insert sanitized INSERTED-TEXT at EDITOR's cursor."
  (line-editor--leave-history editor)
  (let* ((safe-text (terminal-sanitize-text inserted-text))
         (text (line-editor-text editor))
         (cursor (line-editor-cursor editor)))
    (line-editor--set-state
     editor
     (concatenate 'string
                  (subseq text 0 cursor)
                  safe-text
                  (subseq text cursor))
     (+ cursor (length safe-text))))
  nil)

(-> line-editor--delete-backward (line-editor) null)
(defun line-editor--delete-backward (editor)
  "Delete the character immediately before EDITOR's cursor."
  (line-editor--leave-history editor)
  (let ((text (line-editor-text editor))
        (cursor (line-editor-cursor editor)))
    (when (plusp cursor)
      (line-editor--set-state
       editor
       (concatenate 'string
                    (subseq text 0 (1- cursor))
                    (subseq text cursor))
       (1- cursor))))
  nil)

(-> line-editor--delete-forward (line-editor) null)
(defun line-editor--delete-forward (editor)
  "Delete the character at EDITOR's cursor."
  (line-editor--leave-history editor)
  (let ((text (line-editor-text editor))
        (cursor (line-editor-cursor editor)))
    (when (< cursor (length text))
      (line-editor--set-state
       editor
       (concatenate 'string
                    (subseq text 0 cursor)
                    (subseq text (1+ cursor)))
       cursor)))
  nil)

(-> line-editor--history-step (line-editor integer) null)
(defun line-editor--history-step (editor direction)
  "Move EDITOR one history entry in DIRECTION, where positive means older."
  (let* ((history (line-editor-history editor))
         (index (slot-value editor 'history-index)))
    (cond
      ((null history)
       nil)
      ((plusp direction)
       (when (< index (1- (length history)))
         (when (minusp index)
           (setf (slot-value editor 'history-draft)
                 (line-editor-text editor)))
         (incf (slot-value editor 'history-index))
         (let ((entry (nth (slot-value editor 'history-index) history)))
           (line-editor--set-state editor entry (length entry)))))
      ((not (minusp index))
       (decf (slot-value editor 'history-index))
       (if (minusp (slot-value editor 'history-index))
           (let ((draft (or (slot-value editor 'history-draft) "")))
             (setf (slot-value editor 'history-draft) nil)
             (line-editor--set-state editor draft (length draft)))
           (let ((entry (nth (slot-value editor 'history-index) history)))
             (line-editor--set-state editor entry (length entry)))))))
  nil)


;;;; -- Line Editor Methods --

(-> line-editor-set-text (line-editor string) line-editor)
(-> line-editor-clear (line-editor) line-editor)
(-> line-editor-add-history (line-editor string) line-editor)

(defmethod line-editor-handle-event ((editor line-editor) event)
  "Apply one semantic input EVENT to EDITOR."
  (labels ((changed ()
             (values :changed nil)))
    (cond
      ((and (consp event) (eq (first event) :insert))
       (line-editor--insert editor (second event))
       (changed))
      ((and (consp event) (eq (first event) :paste))
       (line-editor--insert editor (second event))
       (changed))
      ((and (consp event) (eq (first event) :line))
       (line-editor-set-text editor (second event))
       (line-editor-handle-event editor :submit))
      ((eq event :left)
       (when (plusp (line-editor-cursor editor))
         (decf (slot-value editor 'cursor)))
       (changed))
      ((eq event :right)
       (when (< (line-editor-cursor editor) (length (line-editor-text editor)))
         (incf (slot-value editor 'cursor)))
       (changed))
      ((eq event :home)
       (setf (slot-value editor 'cursor) 0)
       (changed))
      ((eq event :end)
       (setf (slot-value editor 'cursor) (length (line-editor-text editor)))
       (changed))
      ((eq event :backspace)
       (line-editor--delete-backward editor)
       (changed))
      ((eq event :delete)
       (line-editor--delete-forward editor)
       (changed))
      ((eq event :history-previous)
       (line-editor--history-step editor 1)
       (changed))
      ((eq event :history-next)
       (line-editor--history-step editor -1)
       (changed))
      ((eq event :interrupt)
       (if (plusp (length (line-editor-text editor)))
           (progn
             (line-editor-clear editor)
             (values :cleared nil))
           (values :interrupt nil)))
      ((eq event :end-of-input)
       (if (plusp (length (line-editor-text editor)))
           (progn
             (line-editor--delete-forward editor)
             (changed))
           (values :end-of-input nil)))
      ((eq event :submit)
       (let ((submitted (line-editor-text editor)))
         (when (non-empty-string-p submitted)
           (line-editor-add-history editor submitted))
         (line-editor-clear editor)
         (values :submit submitted)))
      ((eq event :escape)
       (values :escape nil))
      (t
       (values :ignored nil)))))

(defmethod line-editor-render
    ((editor line-editor) (prompt string) (columns integer))
  "Render EDITOR as one horizontally clipped row."
  (let* ((safe-prompt (terminal-sanitize-text prompt :single-line-p t))
         (prompt-limit (max 0 (1- columns)))
         (visible-prompt (terminal--prefix-within-width safe-prompt prompt-limit))
         (prompt-width (terminal--text-width visible-prompt))
         (content-width (max 1 (- columns prompt-width 1)))
         (safe-text (terminal-sanitize-text (line-editor-text editor)
                                            :single-line-p t)))
    (multiple-value-bind (visible-text cursor-offset)
        (terminal--editor-window safe-text
                                 (line-editor-cursor editor)
                                 content-width)
      (values (concatenate 'string visible-prompt visible-text)
              (min (1- columns) (+ prompt-width cursor-offset))))))

;; Generic FTYPEs remain broad so later adapters can add method specializations.
(-> line-editor-handle-event (t t) *)
(-> line-editor-render (t t t) *)


;;;; -- Public Editor Operations --

(-> line-editor-create (&key (:history-limit integer)) line-editor)
(defun line-editor-create (&key (history-limit +terminal-history-limit+))
  "Create an empty line editor retaining at most HISTORY-LIMIT submissions."
  (unless (plusp history-limit)
    (error 'terminal-error
           :message "The terminal history limit must be positive."
           :operation ':create-editor
           :cause nil))
  (make-instance 'line-editor :history-limit history-limit))

(-> line-editor-set-text (line-editor string) line-editor)
(defun line-editor-set-text (editor text)
  "Replace EDITOR input with sanitized TEXT and move its cursor to the end."
  (let ((safe-text (terminal-sanitize-text text)))
    (line-editor--leave-history editor)
    (line-editor--set-state editor safe-text (length safe-text))))

(-> line-editor-clear (line-editor) line-editor)
(defun line-editor-clear (editor)
  "Clear EDITOR input and leave history navigation."
  (line-editor--leave-history editor)
  (line-editor--set-state editor "" 0))

(-> line-editor-add-history (line-editor string) line-editor)
(defun line-editor-add-history (editor text)
  "Add non-empty TEXT to EDITOR history unless it duplicates the newest entry."
  (when (and (non-empty-string-p text)
             (not (and (line-editor-history editor)
                       (string= text (first (line-editor-history editor))))))
    (push text (slot-value editor 'history))
    (when (> (length (line-editor-history editor))
             (line-editor-history-limit editor))
      (setf (slot-value editor 'history)
            (subseq (line-editor-history editor)
                    0
                    (line-editor-history-limit editor)))))
  editor)
