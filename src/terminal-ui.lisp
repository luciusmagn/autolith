(in-package #:autolith)

;;;; -- UI Construction --

(-> terminal-ui--maximum-live-rows (terminal) (integer 1))
(defun terminal-ui--maximum-live-rows (terminal)
  "Return the viewport row budget reserved for TERMINAL's unfinished content."
  (max 1 (1- (terminal-rows terminal))))

(defparameter *terminal-ui-stale-status-seconds* 30
  "The idle duration after which live activity is labelled as stale.")

(defparameter *terminal-ui-status-spinner-frames-per-second* 4
  "The number of REPL status spinner frames painted each second.")

(defparameter *terminal-ui-pending-preview-limit* 3
  "The maximum pending inputs previewed for each delivery class.")

(defparameter *terminal-ui-agent-visible-limit* 8
  "The maximum queued and running child agents shown above the modeline.")

(defparameter *terminal-ui-agent-spinner-frames*
  #("⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷")
  "The shared running-child spinner cycle.")

(-> terminal-ui--monotonic-seconds () real)
(defun terminal-ui--monotonic-seconds ()
  "Return monotonic process time in seconds for live activity accounting."
  (/ (get-internal-real-time)
     (coerce internal-time-units-per-second 'double-float)))

(-> terminal-completion-p (t) boolean)
(defun terminal-completion-p (value)
  "Return true when VALUE describes one interactive completion entry."
  (and (listp value)
       (non-empty-string-p (getf value :name))
       (typep (getf value :argument) '(option string))
       (stringp (getf value :description))))

(-> terminal-agent-activity-p (t) boolean)
(defun terminal-agent-activity-p (value)
  "Return true when VALUE is one queued or running child presentation."
  (handler-case
      (not
       (null
        (and (listp value)
             (evenp (length value))
             (non-empty-string-p (getf value :id))
             (typep (getf value :index) '(integer 1))
             (non-empty-string-p (getf value :agent))
             (member (getf value :state) '(:queued :running) :test #'eq)
             (typep (getf value :current-tool) '(option string))
             (stringp (getf value :assignment))
             (typep (getf value :detached) 'boolean))))
    (error ()
      nil)))

(-> terminal-ui-create
    (&key (:terminal terminal) (:editor (option line-editor)) (:prompt string)
          (:placeholder string) (:completions list)
          (:completion-function (option function))
          (:clock-function function))
    terminal-ui)
(defun terminal-ui-create
    (&key terminal editor (prompt "> ") (placeholder "") completions
          completion-function
          (clock-function #'terminal-ui--monotonic-seconds))
  "Create a scrollback-preserving UI for TERMINAL."
  (unless (typep terminal 'terminal)
    (error 'terminal-error
           :message "TERMINAL-UI-CREATE requires a terminal instance."
           :operation ':create-ui
           :cause nil))
  (unless (every #'terminal-completion-p completions)
    (error 'terminal-error
           :message "Every completion entry needs a name and a description."
           :operation ':create-ui
           :cause nil))
  (unless (typep completion-function '(option function))
    (error 'terminal-error
           :message "The completion provider must be a function or NIL."
           :operation ':create-ui
           :cause nil))
  (let ((live-region
          (make-live-region
           :columns (terminal-columns terminal)
           :maximum-rows (terminal-ui--maximum-live-rows terminal)
           :write-function (lambda (text)
                             (terminal--write terminal text))
           :flush-function (lambda ()
                             (terminal-flush terminal)))))
    (make-instance 'terminal-ui
                   :terminal terminal
                   :editor (or editor
                               (line-editor-create
                                :history-limit *terminal-history-limit*))
                   :live-region live-region
                   :clock-function clock-function
                   :prompt prompt
                   :placeholder placeholder
                   :completions completions
                   :completion-function completion-function
                   :completion-selector
                   (make-selector
                    :visible-count *terminal-ui-visible-completions*
                    :arrangement ':vertical))))

(defmacro with-terminal-ui-locked ((ui) &body body)
  "Run BODY while holding UI's recursive presentation lock."
  (let ((locked-ui (gensym "UI")))
    `(let ((,locked-ui ,ui))
       (with-recursive-lock-held ((terminal-ui-lock ,locked-ui))
         ,@body))))


;;;; -- Terminal Presentation --

(-> terminal-ui--image-label (integer) string)
(defun terminal-ui--image-label (number)
  "Return the visible composer label for image NUMBER."
  (format nil "[Image #~D]" number))

(-> terminal-ui--copy-image-attachments (list) list)
(defun terminal-ui--copy-image-attachments (attachments)
  "Return a detached copy of draft ATTACHMENTS."
  (loop for attachment in attachments
        collect (cons (copy-seq (first attachment))
                      (rest attachment))))

(-> terminal-ui--replace-image-labels (string list) string)
(defun terminal-ui--replace-image-labels (text mapping)
  "Replace image labels in TEXT according to simultaneous MAPPING."
  (with-output-to-string (stream)
    (loop with position = 0
          while (< position (length text))
          for replacement =
            (find-if
             (lambda (entry)
               (let ((label (first entry)))
                 (and (<= (+ position (length label)) (length text))
                      (string= label text
                               :start2 position
                               :end2 (+ position (length label))))))
             mapping)
          do (if replacement
                 (progn
                   (write-string (rest replacement) stream)
                   (incf position (length (first replacement))))
                 (progn
                   (write-char (char text position) stream)
                   (incf position))))))

(-> terminal-ui--submission-input (terminal-ui string)
    (or string user-message-input))
(defun terminal-ui--submission-input (ui text)
  "Return TEXT with only its surviving, consecutively labelled attachments."
  (let* ((attachments
           (remove-if-not
            (lambda (attachment)
              (search (first attachment) text))
            (terminal-ui-image-attachments ui)))
         (mapping
           (loop for attachment in attachments
                 for number from 1
                 collect (cons (first attachment)
                               (terminal-ui--image-label number))))
         (normalized-text (terminal-ui--replace-image-labels text mapping)))
    (if attachments
        (user-message-input-create
         :text normalized-text
         :image-pathnames (mapcar #'rest attachments))
        normalized-text)))

(-> terminal-ui--remember-image-submission (terminal-ui string list) null)
(defun terminal-ui--remember-image-submission (ui text attachments)
  "Remember submitted TEXT and ATTACHMENTS for Clinedi history recall."
  (when attachments
    (push (list :text (copy-seq text)
                :attachments
                (terminal-ui--copy-image-attachments attachments))
          (terminal-ui-image-history ui))
    (when (> (length (terminal-ui-image-history ui))
             *terminal-history-limit*)
      (setf (terminal-ui-image-history ui)
            (subseq (terminal-ui-image-history ui)
                    0 *terminal-history-limit*))))
  nil)

(-> terminal-ui--restore-history-images (terminal-ui) null)
(defun terminal-ui--restore-history-images (ui)
  "Restore or prune image attachments for UI's current editor text."
  (let ((text (line-editor-text (terminal-ui-editor ui))))
    (if (terminal-ui-image-attachments ui)
        (setf (terminal-ui-image-attachments ui)
              (remove-if-not
               (lambda (attachment)
                 (search (first attachment) text))
               (terminal-ui-image-attachments ui)))
        (let ((record
                (find text
                      (terminal-ui-image-history ui)
                      :key (lambda (entry) (getf entry :text))
                      :test #'string=)))
          (when record
            (setf (terminal-ui-image-attachments ui)
                  (terminal-ui--copy-image-attachments
                   (getf record :attachments)))))))
  nil)

(-> terminal-ui--attach-pasted-image (terminal-ui string) boolean)
(defun terminal-ui--attach-pasted-image (ui pasted-text)
  "Attach PASTED-TEXT when it names a supported local image."
  (let ((pathname (image-input-recognize-pasted-path pasted-text)))
    (if pathname
        (let ((label
                (terminal-ui--image-label
                 (1+ (length (terminal-ui-image-attachments ui))))))
          (line-editor-handle-event
           (terminal-ui-editor ui)
           (list ':insert label))
          (setf (terminal-ui-image-attachments ui)
                (nconc (terminal-ui-image-attachments ui)
                       (list (cons label pathname))))
          t)
        nil)))

(-> terminal-ui--set-draft-input
    (terminal-ui (or string user-message-input))
    null)
(defun terminal-ui--set-draft-input (ui input)
  "Replace UI's editor and attachment state with INPUT."
  (etypecase input
    (string
     (setf (terminal-ui-image-attachments ui) nil)
     (line-editor-set-text (terminal-ui-editor ui) (sanitize-text input)))
    (user-message-input
     (setf (terminal-ui-image-attachments ui)
           (loop for pathname in (user-message-input-image-pathnames input)
                 for number from 1
                 collect (cons (terminal-ui--image-label number) pathname)))
     (line-editor-set-text
      (terminal-ui-editor ui)
      (sanitize-text (user-message-input-text input)))))
  nil)

(-> terminal-ui-live-row-count (terminal-ui) (integer 0))
(defun terminal-ui-live-row-count (ui)
  "Return the number of live physical rows currently painted for UI."
  (live-region-row-count (terminal-ui-live-region ui)))

(-> terminal-ui-live-cursor-row (terminal-ui) (integer 0))
(defun terminal-ui-live-cursor-row (ui)
  "Return the physical live row currently holding UI's input cursor."
  (live-region-cursor-row (terminal-ui-live-region ui)))

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
  "Write trusted TEXT while making its line endings terminal-safe."
  (let ((line-start 0))
    (loop for newline = (position #\Newline text :start line-start)
          while newline
          do (terminal--write terminal (subseq text line-start newline))
             (terminal--write-newline terminal)
             (setf line-start (1+ newline))
          finally (terminal--write terminal (subseq text line-start))))
  nil)

(-> terminal--spans-text (list) string)
(defun terminal--spans-text (spans)
  "Return the sanitized visible text represented by SPANS."
  (with-output-to-string (stream)
    (dolist (span spans)
      (write-string (sanitize-text (terminal-span-text span)) stream))))

(-> terminal--render-spans (terminal list) string)
(defun terminal--render-spans (terminal spans)
  "Return trusted terminal presentation for sanitized semantic SPANS."
  (with-output-to-string (stream)
    (dolist (span spans)
      (let* ((text (sanitize-text (terminal-span-text span)))
             (sequence
               (and (terminal-styled-p terminal)
                    (terminal-style-sequence (terminal-span-style span)))))
        (when sequence
          (write-string sequence stream))
        (write-string text stream)
        (when sequence
          (write-string *terminal-style-reset* stream))))))

(-> terminal--write-row (terminal list) null)
(defun terminal--write-row (terminal spans)
  "Write sanitized semantic SPANS as one trusted terminal row."
  (terminal--write-safe-text terminal (terminal--render-spans terminal spans))
  nil)

(-> terminal-ui--prompt-content (terminal-ui) (values list integer))
(defun terminal-ui--prompt-content (ui)
  "Return UI's multiline prompt spans and cursor character offset."
  (let* ((terminal (terminal-ui-terminal ui))
         (columns (terminal-columns terminal))
         (editor (terminal-ui-editor ui))
         (safe-prompt (sanitize-text (terminal-ui-prompt ui)
                                     :single-line-p t)))
    (if (and (zerop (length (line-editor-text editor)))
             (non-empty-string-p (terminal-ui-placeholder ui)))
        (let ((spans
                (terminal--clip-spans
                 (list (terminal-span :brand safe-prompt)
                       (terminal-span :hint (terminal-ui-placeholder ui)))
                 columns)))
          (values spans
                  (min (length safe-prompt)
                       (length (terminal--spans-text spans)))))
        (let ((content-style
                (if (uiop:string-prefix-p "/" (line-editor-text editor))
                    ':user
                    ':plain)))
          (values (list (terminal-span ':brand safe-prompt)
                        (terminal-span content-style
                                       (line-editor-text editor)))
                  (+ (length safe-prompt)
                     (line-editor-cursor editor)))))))

;;;; -- Command Completion Suggestions --

(-> terminal-ui--current-completions (terminal-ui) list)
(defun terminal-ui--current-completions (ui)
  "Return and validate UI's current static or dynamically provided completions."
  (let ((completions
          (if (terminal-ui-completion-function ui)
              (funcall (terminal-ui-completion-function ui))
              (terminal-ui-completions ui))))
    (unless (and (listp completions)
                 (every #'terminal-completion-p completions))
      (error 'terminal-error
             :message "The completion provider returned invalid entries."
             :operation ':complete
             :cause nil))
    completions))

(-> terminal-ui--matching-completions (terminal-ui) list)
(defun terminal-ui--matching-completions (ui)
  "Return UI completions whose names extend the command currently being typed."
  (let ((text (line-editor-text (terminal-ui-editor ui)))
        (completions (terminal-ui--current-completions ui)))
    (if (and (terminal-interactive-p (terminal-ui-terminal ui))
             completions
             (uiop:string-prefix-p "/" text)
             (not (find #\Space text))
             (not (find #\Newline text)))
        (remove-if-not (lambda (entry)
                         (uiop:string-prefix-p (string-downcase text)
                                               (getf entry :name)))
                       completions)
        nil)))

(-> terminal-ui--reconcile-completions (terminal-ui) list)
(defun terminal-ui--reconcile-completions (ui)
  "Return UI's current matches, resetting the selection when the set changes."
  (let ((selector (terminal-ui-completion-selector ui)))
    (if (terminal-ui-completion-active-p ui)
        (selector-items selector)
        (let ((matches (terminal-ui--matching-completions ui)))
          (selector-set-items selector matches)
          matches))))

(-> terminal-completion-label (list) string)
(defun terminal-completion-label (entry)
  "Return completion ENTRY's display name including its argument hint."
  (let ((argument (getf entry :argument)))
    (if argument
        (format nil "~A ~A" (getf entry :name) argument)
        (getf entry :name))))

(-> terminal-ui--choice-rows (selector integer) list)
(defun terminal-ui--choice-rows (selector row-width)
  "Return styled candidate rows and nonselectable group headings."
  (multiple-value-bind (index-rows arrangement-widths)
      (selector-arrange selector
                        row-width
                        :width-function
                        (lambda (entry)
                          (text-cell-width
                           (terminal-completion-label entry))))
    (declare (ignore arrangement-widths))
    (let* ((cell-rows
             (loop for entry in (selector-items selector)
                   collect (list (terminal-completion-label entry)
                                 (or (getf entry :description) ""))))
           (column-widths
             (layout-column-widths cell-rows
                                   (max 0 (- row-width 2))
                                   :gap-width 2
                                   :minimum-widths '(1 0)))
           (label-width (or (first column-widths) 0))
           (description-width (or (second column-widths) 0)))
      (let ((previous-group nil))
        (loop for index-row in index-rows
              for index = (first index-row)
              for entry = (nth index (selector-items selector))
              for selected-p = (= index (selector-selection selector))
              for group = (getf entry :group)
              append
              (prog1
                  (append
                   (when (and group (not (equal group previous-group)))
                     (append
                      (when previous-group (list nil))
                      (list
                       (terminal--clip-spans
                        (list (terminal-span ':strong
                                             (format nil "  ~A" group)))
                        row-width))))
                   (list
                    (terminal--clip-spans
                     (list (terminal-span (if selected-p
                                              :brand
                                              :dim)
                                          (if selected-p
                                              "▸ "
                                              "  "))
                           (terminal-span :user
                                          (layout-fit-text
                                           (terminal-completion-label entry)
                                           label-width))
                           (terminal-span ':plain
                                          (if (plusp description-width)
                                              "  "
                                              ""))
                           (terminal-span
                            (if selected-p ':plain ':dim)
                            (text-cell-prefix
                             (or (getf entry :description) "")
                             description-width)))
                     row-width)))
                (setf previous-group group)))))))

(-> terminal-ui--completion-rows (terminal-ui integer) list)
(defun terminal-ui--completion-rows (ui row-width)
  "Return styled rows for UI's matching command completions."
  (terminal-ui--reconcile-completions ui)
  (terminal-ui--choice-rows (terminal-ui-completion-selector ui) row-width))

(-> terminal-ui--accept-completion (terminal-ui list) null)
(defun terminal-ui--accept-completion (ui entry)
  "Replace UI's input with ENTRY's name, adding a space when it takes an argument."
  (line-editor-set-text
   (terminal-ui-editor ui)
   (sanitize-text
    (concatenate 'string
                 (getf entry :name)
                 (if (getf entry :argument)
                     " "
                     ""))))
  nil)

(-> terminal-ui--begin-completion (terminal-ui) null)
(defun terminal-ui--begin-completion (ui)
  "Begin choosing among UI's current command completion candidates."
  (unless (terminal-ui-completion-active-p ui)
    (setf (terminal-ui-completion-prefix ui)
          (line-editor-text (terminal-ui-editor ui))
          (terminal-ui-completion-active-p ui) t))
  nil)

(-> terminal-ui--end-completion (terminal-ui) null)
(defun terminal-ui--end-completion (ui)
  "Leave UI's active command completion selection without changing input."
  (setf (terminal-ui-completion-active-p ui) nil
        (terminal-ui-completion-prefix ui) nil)
  nil)

(-> terminal-ui--cancel-completion (terminal-ui) null)
(defun terminal-ui--cancel-completion (ui)
  "Cancel UI's completion selection and restore its original command prefix."
  (let ((prefix (terminal-ui-completion-prefix ui)))
    (when prefix
      (line-editor-set-text (terminal-ui-editor ui) prefix)))
  (selector-set-items (terminal-ui-completion-selector ui) nil)
  (terminal-ui--end-completion ui)
  nil)

(-> terminal-ui--handle-completion-event
    (terminal-ui t)
    (values (option keyword) (option string)))
(defun terminal-ui--handle-completion-event (ui event)
  "Apply EVENT to UI's completion suggestions and return its action when consumed."
  (terminal-ui--reconcile-completions ui)
  (let ((selector (terminal-ui-completion-selector ui)))
    (block nil
      (unless (selector-items selector)
        (return (values nil nil)))
      (unless (or (terminal-ui-completion-active-p ui)
                  (member event
                          '(:up :down :history-previous :history-next
                            :complete :complete-previous :submit)))
        (return (values nil nil)))
      (when (member event '(:up :down :history-previous :history-next
                            :complete :complete-previous))
        (terminal-ui--begin-completion ui))
      (multiple-value-bind (selector-action entry)
          (selector-handle-event selector event)
        (case selector-action
          (:changed
           (terminal-ui--accept-completion ui entry)
           (terminal-ui--repaint-live ui)
           (values :changed nil))
          (:accept
           (terminal-ui--end-completion ui)
           (terminal-ui--accept-completion ui entry)
           (cond
             ((getf entry :argument)
              (terminal-ui--repaint-live ui)
              (values :changed nil))
             (t
              (multiple-value-bind (action payload)
                  (line-editor-handle-event (terminal-ui-editor ui) :submit)
                (terminal-ui--repaint-live ui)
                (values action payload)))))
          (:cancel
           (terminal-ui--cancel-completion ui)
           (terminal-ui--repaint-live ui)
           (values :changed nil))
          (:dismiss
           (terminal-ui--end-completion ui)
           (values nil nil))
          (t
           (values nil nil)))))))

(-> terminal-ui--pending-input-rows
    (string list &key (:count integer) (:row-width integer))
    list)
(defun terminal-ui--pending-input-rows
    (label inputs &key count row-width)
  "Return bounded live rows previewing pending INPUTS under LABEL."
  (let* ((visible-count (min *terminal-ui-pending-preview-limit*
                             (length inputs)))
         (omitted (- count visible-count)))
    (append
     (loop for input in (subseq inputs 0 visible-count)
           for index from 1
           collect
           (terminal--clip-spans
            (list (terminal-span ':brand "∙ ")
                  (terminal-span ':dim
                                 (format nil "~A ~D/~D  "
                                         label index count))
                  (terminal-span
                   ':plain
                   (sanitize-text input :single-line-p t)))
            row-width))
     (when (plusp omitted)
       (list
        (terminal--clip-spans
         (list (terminal-span ':brand "∙ ")
               (terminal-span ':dim
                              (format nil "~D more ~A input~:P"
                                      omitted label)))
         row-width))))))


;;;; -- Live Region Composition --

(-> terminal-ui--agent-spinner-phase-at (real) (integer 0))
(defun terminal-ui--agent-spinner-phase-at (now)
  "Return the shared child-agent spinner phase at monotonic NOW."
  (mod (floor (* *terminal-ui-status-spinner-frames-per-second*
                 (max 0 now)))
       (length *terminal-ui-agent-spinner-frames*)))

(-> terminal-ui--agent-activity-row-at
    (list real integer &key (:identity-width integer))
    list)
(defun terminal-ui--agent-activity-row-at
    (activity now row-width &key (identity-width 0))
  "Return one clipped child ACTIVITY row at monotonic NOW."
  (let* ((running-p (eq (getf activity :state) ':running))
         (current-tool (getf activity :current-tool))
         (assignment (getf activity :assignment))
         (detail
           (cond
             ((non-empty-string-p current-tool)
              (terminal-span ':agent-tool current-tool))
             ((not running-p)
              (terminal-span ':dim "queued"))
             ((non-empty-string-p assignment)
              (terminal-span ':dim assignment))
             (t
              nil))))
    (terminal--clip-spans
     (remove
      nil
      (append
       (list
        (terminal-span ':dim "  ")
        (if running-p
            (terminal-span
             ':agent-spinner
             (format nil "~A "
                     (aref *terminal-ui-agent-spinner-frames*
                           (terminal-ui--agent-spinner-phase-at now))))
            (terminal-span ':dim "○ "))
        (terminal-span ':agent-name
                       (layout-fit-text (getf activity :id)
                                        identity-width))
        (terminal-span ':dim " · ")
        (terminal-span ':agent-role (getf activity :agent)))
       (when detail
         (list (terminal-span ':dim " · ")
               detail))))
     row-width)))

(-> terminal-ui--agent-activity-rows-at
    (terminal-ui real integer)
    list)
(defun terminal-ui--agent-activity-rows-at (ui now row-width)
  "Return bounded child-agent rows for UI at monotonic NOW."
  (let* ((activities (terminal-ui-agent-activities ui))
         (visible-count
           (min *terminal-ui-agent-visible-limit* (length activities)))
         (omitted-count (- (length activities) visible-count))
         (visible-activities (subseq activities 0 visible-count))
         (identity-width
           (or
            (first
             (layout-column-widths
              (loop for activity in visible-activities
                    collect
                    (list (getf activity :id)
                          (getf activity :agent)))
              (max 0 (- row-width (text-cell-width "  ○ ")))
              :gap-width 3
              :minimum-widths '(1 1)))
            0)))
    (when activities
      (append
       (list
        (terminal--clip-spans
         (list (terminal-span ':agent-name "agents")
               (terminal-span ':dim
                              (format nil " ~D" (length activities))))
         row-width)
        nil)
       (loop for activity in visible-activities
             collect
             (terminal-ui--agent-activity-row-at
              activity now row-width
              :identity-width identity-width))
       (when (plusp omitted-count)
         (list
          (terminal--clip-spans
           (list (terminal-span ':dim "  ")
                 (terminal-span ':dim
                                (format nil "… ~D more agent~:P"
                                        omitted-count)))
           row-width)))))))

(-> terminal-ui--status-times-at
    (terminal-ui real)
    (values integer integer))
(defun terminal-ui--status-times-at (ui now)
  "Return elapsed and idle whole seconds for UI's activity at monotonic NOW."
  (let* ((started-at (or (terminal-ui-status-started-at ui) now))
         (progress-at (or (terminal-ui-status-progress-at ui) started-at)))
    (values (max 0 (floor (- now started-at)))
            (max 0 (floor (- now progress-at))))))

(-> terminal-ui--duration-text (integer) string)
(defun terminal-ui--duration-text (seconds)
  "Format non-negative SECONDS as a compact activity duration."
  (let ((seconds (max 0 seconds)))
    (multiple-value-bind (minutes remaining-seconds)
        (floor seconds 60)
      (multiple-value-bind (hours remaining-minutes)
          (floor minutes 60)
        (if (plusp hours)
            (format nil "~D:~2,'0D:~2,'0D"
                    hours remaining-minutes remaining-seconds)
            (format nil "~2,'0D:~2,'0D"
                    remaining-minutes remaining-seconds))))))

(-> terminal-ui--status-spinner-phase-at
    (terminal-ui real)
    (integer 0 3))
(defun terminal-ui--status-spinner-phase-at (ui now)
  "Return UI's quarter-second REPL status spinner phase at NOW."
  (let ((started-at (or (terminal-ui-status-started-at ui) now)))
    (mod (floor (* *terminal-ui-status-spinner-frames-per-second*
                   (max 0 (- now started-at))))
         4)))

(-> terminal-ui--status-spinner-spans-at (terminal-ui real) list)
(defun terminal-ui--status-spinner-spans-at (ui now)
  "Return UI's fixed-width READ/EVAL/PRINT/LOOP spinner spans at NOW."
  (multiple-value-bind (text bright-index)
      (ecase (terminal-ui--status-spinner-phase-at ui now)
        (0 (values "READ " 0))
        (1 (values "EVAL " 1))
        (2 (values "PRINT" 2))
        (3 (values "LOOP " 3)))
    (remove nil
            (list (and (plusp bright-index)
                       (terminal-span ':status-dim
                                      (subseq text 0 bright-index)))
                  (terminal-span ':status-plain
                                 (subseq text bright-index
                                         (1+ bright-index)))
                  (and (< (1+ bright-index) (length text))
                       (terminal-span ':status-dim
                                      (subseq text (1+ bright-index))))))))

(-> terminal-ui--status-signature-at (terminal-ui real) list)
(defun terminal-ui--status-signature-at (ui now)
  "Return the visible values identifying UI's status paint at NOW."
  (multiple-value-bind (elapsed idle)
      (terminal-ui--status-times-at ui now)
    (list elapsed
          (and (>= idle *terminal-ui-stale-status-seconds*) idle)
          (terminal-ui--status-spinner-phase-at ui now))))

(-> terminal-ui--animation-signature-at
    (terminal-ui real)
    (option list))
(defun terminal-ui--animation-signature-at (ui now)
  "Return visible status and child-agent values identifying a live paint."
  (let ((activities (terminal-ui-agent-activities ui)))
    (when (or (terminal-ui-status ui) activities)
      (list
       (and (terminal-ui-status ui)
            (terminal-ui--status-signature-at ui now))
       (and activities
            (list
             (and (find ':running activities
                        :key (lambda (activity)
                               (getf activity :state))
                        :test #'eq)
                  (terminal-ui--agent-spinner-phase-at now))
             activities))))))

(-> terminal-ui--status-text-at (terminal-ui real) string)
(defun terminal-ui--status-text-at (ui now)
  "Return UI's activity timing at monotonic NOW."
  (multiple-value-bind (elapsed idle)
      (terminal-ui--status-times-at ui now)
    (if (>= idle *terminal-ui-stale-status-seconds*)
        (format nil "~A · no update ~A"
                (terminal-ui--duration-text elapsed)
                (terminal-ui--duration-text idle))
        (terminal-ui--duration-text elapsed))))

(-> terminal-ui--status-row-at (terminal-ui real integer) list)
(defun terminal-ui--status-row-at (ui now row-width)
  "Return UI's clipped status spans padded across ROW-WIDTH cells."
  (let* ((content
           (append
            (terminal-ui--status-spinner-spans-at ui now)
            (list (terminal-span ':status-accent " ∙ ")
                  (terminal-span ':status-dim
                                 (terminal-ui--status-text-at ui now)))
            (terminal-ui-status-details ui)))
         (clipped (terminal--clip-spans content row-width))
         (padding (and (terminal-styled-p (terminal-ui-terminal ui))
                       (- row-width (terminal--spans-width clipped)))))
    (if (and padding (plusp padding))
        (append clipped
                (list (terminal-span ':status-plain
                                     (make-string padding
                                                  :initial-element #\Space))))
        clipped)))

(-> terminal-ui--rows-content
    (terminal list &key (:cursor-row integer) (:cursor-offset integer))
    (values string string integer))
(defun terminal-ui--rows-content
    (terminal rows &key (cursor-row 0) (cursor-offset 0))
  "Return ROWS as plain and styled text plus their cursor character index."
  (let ((plain-stream (make-string-output-stream))
        (display-stream (make-string-output-stream))
        (plain-length 0)
        (cursor-index nil))
    (loop for row in rows
          for index from 0
          for plain = (terminal--spans-text row)
          for display = (terminal--render-spans terminal row)
          do (when (= index cursor-row)
               (setf cursor-index
                     (+ plain-length
                        (min (max 0 cursor-offset) (length plain)))))
             (write-string plain plain-stream)
             (write-string display display-stream)
             (incf plain-length (length plain))
             (when (< (1+ index) (length rows))
               (write-char #\Newline plain-stream)
               (write-char #\Newline display-stream)
               (incf plain-length)))
    (unless cursor-index
      (error 'terminal-error
             :message "The live-region cursor row is outside its content."
             :operation ':render
             :cause nil))
    (values (get-output-stream-string plain-stream)
            (get-output-stream-string display-stream)
            cursor-index)))

(-> terminal-ui--live-content
    (terminal-ui &optional (option real))
    (values string string integer))
(defun terminal-ui--live-content (ui &optional status-now)
  "Return UI's complete plain and styled live content plus its cursor index."
  (let* ((terminal (terminal-ui-terminal ui))
         (row-width (max 1 (terminal-columns terminal)))
         (status-now (or status-now
                         (and (or (terminal-ui-status ui)
                                  (terminal-ui-agent-activities ui))
                              (funcall (terminal-ui-clock-function ui)))))
         (rows nil))
    (dolist (row (terminal-ui-preview-rows ui))
      (setf rows
            (append rows
                    (list (terminal--clip-spans row row-width)))))
    (let ((tail (terminal-ui-stream-tail ui)))
      (when tail
        (setf rows
              (append rows
                      (list
                       (terminal--clip-spans
                        (if (stringp tail)
                            (list (terminal-span ':plain tail))
                            tail)
                        row-width))))))
    (let ((activity-rows
            (and (terminal-ui-agent-activities ui)
                 (terminal-ui--agent-activity-rows-at
                  ui status-now row-width))))
      (when activity-rows
        (setf rows
              (append rows
                      (list nil)
                      activity-rows))))
    (when (terminal-ui-status ui)
      (setf rows
            (append rows
                    (list
                     nil
                     (terminal-ui--status-row-at
                      ui status-now row-width)))))
    (let ((steering-inputs (terminal-ui-steering-input-previews ui)))
      (when steering-inputs
        (setf rows
              (append
               rows
               (terminal-ui--pending-input-rows
                "steering"
                steering-inputs
                :count (length steering-inputs)
                :row-width row-width)))))
    (let ((queued-inputs (terminal-ui-queued-input-previews ui)))
      (when queued-inputs
        (setf rows
              (append
               rows
               (terminal-ui--pending-input-rows
                "follow-up"
                queued-inputs
                :count (length queued-inputs)
                :row-width row-width)
               (list
                (terminal--clip-spans
                 (list
                  (terminal-span ':hint
                                 "  Empty Tab edits the newest follow-up."))
                 row-width))))))
    (when rows
      (setf rows (append rows (list nil))))
    (let ((selector (terminal-ui-selector ui)))
      (cond
        (selector
         (let ((title-spans
                 (terminal--clip-spans
                  (list (terminal-span ':brand "∙ ")
                        (terminal-span ':plain
                                       (terminal-ui-selector-title ui))
                        (terminal-span ':hint "  enter selects, esc cancels"))
                  row-width)))
           (let ((cursor-row (length rows)))
             (setf rows
                   (append rows
                           (list title-spans)
                           (terminal-ui--choice-rows
                            selector
                            row-width)
                           (list nil)))
             (terminal-ui--rows-content
              terminal
              rows
              :cursor-row cursor-row
              :cursor-offset (length (terminal--spans-text title-spans))))))
        (t
         (multiple-value-bind (prompt-spans cursor-offset)
             (terminal-ui--prompt-content ui)
           (let ((cursor-row (length rows)))
             (setf rows
                   (append rows
                           (list prompt-spans)
                           (terminal-ui--completion-rows
                            ui row-width)
                           (list nil)))
             (terminal-ui--rows-content
              terminal
              rows
              :cursor-row cursor-row
              :cursor-offset cursor-offset))))))))

(-> terminal-ui--stream-output (terminal list) (values string string))
(defun terminal-ui--stream-output (terminal rows)
  "Return streamed ROWS as plain and styled output ending on a fresh line."
  (let ((plain-stream (make-string-output-stream))
        (display-stream (make-string-output-stream)))
    (dolist (row rows)
      (let ((safe-row
              (loop for span in row
                    collect (terminal-span
                             (terminal-span-style span)
                             (sanitize-text (terminal-span-text span)
                                            :single-line-p t)))))
        (write-string (terminal--spans-text safe-row) plain-stream)
        (write-string (terminal--render-spans terminal safe-row) display-stream)
        (write-char #\Newline plain-stream)
        (write-char #\Newline display-stream)))
    (values (get-output-stream-string plain-stream)
            (get-output-stream-string display-stream))))

(-> terminal-ui-stream-update
    (terminal-ui &key (:rows list) (:tail (or null string list)))
    terminal-ui)
(defun terminal-ui-stream-update (ui &key rows tail)
  "Append streamed single-line ROWS to the transcript and show TAIL as unfinished.

Each row is a styled span list appended once without a separating blank row, so
consecutive updates build one continuous transcript block. TAIL replaces the
live unfinished line continuing that block, or removes it when NIL."
  (with-terminal-ui-locked (ui)
    (let ((terminal (terminal-ui-terminal ui)))
      (multiple-value-bind (plain-output display-output)
          (terminal-ui--stream-output terminal rows)
        (setf (terminal-ui-stream-tail ui) tail)
        (if (terminal-interactive-p terminal)
            (terminal-ui--present-live
             ui
             :appended-text plain-output
             :appended-display display-output)
            (progn
              (when (plusp (length display-output))
                (terminal--write-safe-text terminal display-output))
              (terminal-ui--paint-live ui)))
        (terminal-flush terminal))))
  ui)

(-> terminal-ui--present-live
    (terminal-ui &key (:status-now (option real))
                      (:appended-text string)
                      (:appended-display string))
    null)
(defun terminal-ui--present-live
    (ui &key status-now (appended-text "") (appended-display ""))
  "Present UI live content, atomically preceding it with appended scrollback."
  (let* ((status-now (or status-now
                         (and (or (terminal-ui-status ui)
                                  (terminal-ui-agent-activities ui))
                              (funcall (terminal-ui-clock-function ui)))))
         (terminal (terminal-ui-terminal ui)))
    (setf (terminal-ui-status-rendered-signature ui)
          (and status-now
               (terminal-ui--animation-signature-at ui status-now)))
    (when (terminal-interactive-p terminal)
      (multiple-value-bind (text display cursor)
          (terminal-ui--live-content ui status-now)
        (if (plusp (length appended-text))
            (live-region-append-and-present
             (terminal-ui-live-region ui)
             appended-text
             text
             :appended-display appended-display
             :cursor cursor
             :display display)
            (live-region-present (terminal-ui-live-region ui)
                                 text
                                 :cursor cursor
                                 :display display)))))
  nil)

(-> terminal-ui--paint-live
    (terminal-ui &optional (option real))
    null)
(defun terminal-ui--paint-live (ui &optional status-now)
  "Present UI's unfinished content below ordinary terminal scrollback."
  (terminal-ui--present-live ui :status-now status-now)
  nil)

(-> terminal-ui--repaint-live (terminal-ui) null)
(defun terminal-ui--repaint-live (ui)
  "Recompose and repaint only UI's bounded live region."
  (terminal-ui--paint-live ui)
  nil)

(-> terminal-ui--finalized-content
    (terminal-ui (or string list))
    (values string string))
(defun terminal-ui--finalized-content (ui entry)
  "Return finalized ENTRY as plain and styled text with a blank separator."
  (let* ((terminal (terminal-ui-terminal ui))
         (spans (if (stringp entry)
                    (list (terminal-span ':plain entry))
                    entry))
         (plain (terminal--spans-text spans))
         (display (terminal--render-spans terminal spans)))
    (unless (and (plusp (length plain))
                 (char= (char plain (1- (length plain))) #\Newline))
      (setf plain (concatenate 'string plain (string #\Newline))
            display (concatenate 'string display (string #\Newline))))
    (values (concatenate 'string plain (string #\Newline))
            (concatenate 'string display (string #\Newline)))))


(-> terminal-ui-refresh-size
    (terminal-ui (option function))
    boolean)
(defun terminal-ui-refresh-size (ui callback)
  "Apply CALLBACK's pending terminal size to UI and report whether it repainted."
  (let ((size (and callback (funcall callback))))
    (cond
      ((null size)
       nil)
      ((typep size '(cons (integer 1) (integer 1)))
       (terminal-ui-resize ui (rest size) :rows (first size))
       t)
      (t
       (error 'terminal-error
              :message "A terminal resize callback returned an invalid size."
              :operation ':resize
              :cause nil)))))

(-> terminal-ui-select
    (terminal-ui &key (:title string) (:items list)
                 (:resize-callback (option function)))
    (option string))
(defun terminal-ui-select (ui &key (title "select") items resize-callback)
  "Run a modal picker over ITEMS and return the selected name, or NIL on cancel.

Items follow the completion entry shape. Up and Down move the selection. Tab
and Shift-Tab cycle it forward and backward, and Enter accepts it. Other
ordinary input dismisses the picker with the selected item. Escape, Ctrl-C, or
end of input cancels. Returns NIL immediately when ITEMS is empty or the
terminal is not interactive.

RESIZE-CALLBACK is queried before each blocking read and immediately after the
read returns. It returns positive pending rows and columns as a cons, or NIL
when no resize needs to be applied."
  (block nil
    (unless (and items
                 (every #'terminal-completion-p items)
                 (terminal-interactive-p (terminal-ui-terminal ui)))
      (return nil))
    (with-terminal-ui-locked (ui)
      (setf (terminal-ui-selector ui)
            (make-selector
             :items items
             :visible-count *terminal-ui-visible-completions*
             :arrangement ':vertical)
            (terminal-ui-selector-title ui) title))
    (unwind-protect
         (loop
           (with-terminal-ui-locked (ui)
             (unless (terminal-ui-refresh-size ui resize-callback)
               (terminal-ui--repaint-live ui)))
           (let ((event (terminal-read-event (terminal-ui-terminal ui))))
             (with-terminal-ui-locked (ui)
               (terminal-ui-refresh-size ui resize-callback)
               (multiple-value-bind (action item)
                   (selector-handle-event (terminal-ui-selector ui) event)
                 (case action
                   (:accept
                    (return (getf item :name)))
                   (:cancel
                    (return nil))
                   (:dismiss
                    (return (getf item :name)))
                   (t
                    nil))))))
      (with-terminal-ui-locked (ui)
        (setf (terminal-ui-selector ui) nil
              (terminal-ui-selector-title ui) nil)
        (terminal-ui--repaint-live ui)))))


;;;; -- Public UI Lifecycle and Events --

(-> terminal-ui-start (terminal-ui) terminal-ui)
(defun terminal-ui-start (ui)
  "Start UI on the primary screen and render its bounded live region."
  (with-terminal-ui-locked (ui)
    (unless (terminal-ui-started-p ui)
      (terminal-start (terminal-ui-terminal ui))
      (setf (terminal-ui-started-p ui) t)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-stop (terminal-ui) terminal-ui)
(defun terminal-ui-stop (ui)
  "Erase UI's unfinished rows and restore its terminal even after partial startup."
  (with-terminal-ui-locked (ui)
    (unwind-protect
         (when (terminal-ui-started-p ui)
           (live-region-dismiss (terminal-ui-live-region ui)))
      (setf (terminal-ui-started-p ui) nil)
      (terminal-stop (terminal-ui-terminal ui))))
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

(-> terminal-ui-mark-finalized (terminal-ui t) boolean)
(defun terminal-ui-mark-finalized (ui identifier)
  "Remember finalized IDENTIFIER and return true only on its first occurrence."
  (with-terminal-ui-locked (ui)
    (block nil
      (when (gethash identifier (terminal-ui-finalized-identifiers ui))
        (return nil))
      (setf (gethash identifier (terminal-ui-finalized-identifiers ui)) t)
      t)))

(-> terminal-ui-append-finalized (terminal-ui t (or string list)) boolean)
(defun terminal-ui-append-finalized (ui identifier entry)
  "Append finalized transcript ENTRY once for IDENTIFIER and return true when emitted."
  (with-terminal-ui-locked (ui)
    (block nil
      (when (gethash identifier (terminal-ui-finalized-identifiers ui))
        (return nil))
      (handler-case
          (multiple-value-bind (text display)
              (terminal-ui--finalized-content ui entry)
            (if (terminal-interactive-p (terminal-ui-terminal ui))
                (live-region-append (terminal-ui-live-region ui)
                                    text
                                    :display display)
                (progn
                  (terminal--write-safe-text (terminal-ui-terminal ui) display)
                  (terminal-flush (terminal-ui-terminal ui))))
            (setf (gethash identifier
                           (terminal-ui-finalized-identifiers ui))
                  t))
        (error (condition)
          (remhash identifier (terminal-ui-finalized-identifiers ui))
          (error condition)))
      t)))

(-> terminal-ui-append-finalized-batch (terminal-ui list) (integer 0))
(defun terminal-ui-append-finalized-batch (ui entries)
  "Append ordered (IDENTIFIER ENTRY) pairs with one terminal-region update."
  (with-terminal-ui-locked (ui)
    (let ((pending nil)
          (seen (make-hash-table :test #'equal)))
      (dolist (pair entries)
        (destructuring-bind (identifier entry) pair
          (unless (or (gethash identifier
                               (terminal-ui-finalized-identifiers ui))
                      (gethash identifier seen))
            (multiple-value-bind (text display)
                (terminal-ui--finalized-content ui entry)
              (push (list identifier text display) pending)
              (setf (gethash identifier seen) t)))))
      (setf pending (nreverse pending))
      (when pending
        (let ((text-stream (make-string-output-stream))
              (display-stream (make-string-output-stream)))
          (dolist (entry pending)
            (write-string (second entry) text-stream)
            (write-string (third entry) display-stream))
          (let ((text (get-output-stream-string text-stream))
                (display (get-output-stream-string display-stream)))
            (handler-case
                (progn
                  (if (terminal-interactive-p (terminal-ui-terminal ui))
                      (live-region-append (terminal-ui-live-region ui)
                                          text
                                          :display display)
                      (progn
                        (terminal--write-safe-text
                         (terminal-ui-terminal ui)
                         display)
                        (terminal-flush (terminal-ui-terminal ui))))
                  (dolist (entry pending)
                    (setf (gethash
                           (first entry)
                           (terminal-ui-finalized-identifiers ui))
                          t)))
              (error (condition)
                (dolist (entry pending)
                  (remhash (first entry)
                           (terminal-ui-finalized-identifiers ui)))
                (error condition))))))
      (length pending))))

(-> terminal-ui-set-preview-rows (terminal-ui list) terminal-ui)
(defun terminal-ui-set-preview-rows (ui rows)
  "Replace UI's transient styled ROWS and repaint only the live region."
  (unless (every #'terminal-styled-text-p rows)
    (error 'terminal-error
           :message "Every terminal preview row must contain styled spans."
           :operation ':set-preview
           :cause nil))
  (with-terminal-ui-locked (ui)
    (unless (equal rows (terminal-ui-preview-rows ui))
      (setf (terminal-ui-preview-rows ui) rows)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-set-status
    (terminal-ui (option string) &key (:details terminal-styled-text))
    terminal-ui)
(defun terminal-ui-set-status (ui status &key details)
  "Begin or clear UI's timed one-row STATUS activity phase with DETAILS."
  (unless (terminal-styled-text-p details)
    (error 'terminal-error
           :message "Terminal status details must contain styled spans."
           :operation ':set-status
           :cause nil))
  (with-terminal-ui-locked (ui)
    (let ((safe-status (and status
                            (sanitize-text status :single-line-p t))))
      (cond
        (safe-status
         (let ((now (funcall (terminal-ui-clock-function ui))))
           (setf (terminal-ui-status ui) safe-status
                 (terminal-ui-status-details ui) details
                 (terminal-ui-status-started-at ui) now
                 (terminal-ui-status-progress-at ui) now)
           (terminal-ui--paint-live ui now)))
        ((terminal-ui-status ui)
         (setf (terminal-ui-status ui) nil
               (terminal-ui-status-details ui) nil
               (terminal-ui-status-started-at ui) nil
               (terminal-ui-status-progress-at ui) nil
               (terminal-ui-status-rendered-signature ui) nil)
         (terminal-ui--paint-live ui)))))
  ui)

(-> terminal-ui-note-status-progress (terminal-ui) terminal-ui)
(defun terminal-ui-note-status-progress (ui)
  "Record current progress without restarting UI's elapsed activity clock."
  (with-terminal-ui-locked (ui)
    (when (terminal-ui-status ui)
      (setf (terminal-ui-status-progress-at ui)
            (funcall (terminal-ui-clock-function ui)))))
  ui)

(-> terminal-ui--sanitize-agent-activity (list) list)
(defun terminal-ui--sanitize-agent-activity (activity)
  "Return a display-safe detached copy of one child ACTIVITY."
  (let ((current-tool (getf activity :current-tool)))
    (list :id
          (sanitize-text (getf activity :id) :single-line-p t)
          :index (getf activity :index)
          :agent
          (sanitize-text (getf activity :agent) :single-line-p t)
          :state (getf activity :state)
          :current-tool
          (and current-tool
               (sanitize-text current-tool :single-line-p t))
          :assignment
          (sanitize-text (getf activity :assignment) :single-line-p t)
          :detached (not (null (getf activity :detached))))))

(-> terminal-ui-set-agent-activities (terminal-ui list) terminal-ui)
(defun terminal-ui-set-agent-activities (ui activities)
  "Replace UI's queued and running child-agent presentation state.

The responsive input reader coalesces worker notifications with animation
frames, so this function never paints directly from a child thread."
  (unless (every #'terminal-agent-activity-p activities)
    (error 'terminal-error
           :message "Every child-agent activity must be a valid live snapshot."
           :operation ':set-agent-activities
           :cause nil))
  (let ((safe-activities
          (sort (mapcar #'terminal-ui--sanitize-agent-activity activities)
                #'<
                :key (lambda (activity)
                       (getf activity :index)))))
    (with-terminal-ui-locked (ui)
      (unless (equal safe-activities (terminal-ui-agent-activities ui))
        (setf (terminal-ui-agent-activities ui) safe-activities))))
  ui)

(-> terminal-ui-refresh-status (terminal-ui) boolean)
(defun terminal-ui-refresh-status (ui)
  "Repaint UI when a visible status or child-agent value changed."
  (with-terminal-ui-locked (ui)
    (let* ((status-now (and (or (terminal-ui-status ui)
                                (terminal-ui-agent-activities ui))
                            (funcall (terminal-ui-clock-function ui))))
           (signature (and status-now
                           (terminal-ui--animation-signature-at
                            ui status-now))))
      (if (equal signature (terminal-ui-status-rendered-signature ui))
          nil
          (progn
            (terminal-ui--paint-live ui status-now)
            t)))))

(-> terminal-ui-set-pending-inputs (terminal-ui list list) terminal-ui)
(defun terminal-ui-set-pending-inputs (ui steering-inputs queued-inputs)
  "Set UI's pending input previews and repaint them at most once."
  (let ((safe-steering (mapcar #'sanitize-text steering-inputs))
        (safe-queued (mapcar #'sanitize-text queued-inputs)))
    (with-terminal-ui-locked (ui)
      (unless (and (equal safe-steering
                          (terminal-ui-steering-input-previews ui))
                   (equal safe-queued
                          (terminal-ui-queued-input-previews ui)))
        (setf (terminal-ui-steering-input-previews ui) safe-steering
              (terminal-ui-queued-input-previews ui) safe-queued)
        (terminal-ui--paint-live ui))))
  ui)

(-> terminal-ui-recall-follow-up
    (terminal-ui (or string user-message-input)
     &key (:steering-inputs list) (:queued-inputs list))
    terminal-ui)
(defun terminal-ui-recall-follow-up
    (ui input &key steering-inputs queued-inputs)
  "Recall INPUT into UI while atomically refreshing pending input previews."
  (let ((safe-steering (mapcar #'sanitize-text steering-inputs))
        (safe-queued (mapcar #'sanitize-text queued-inputs)))
    (with-terminal-ui-locked (ui)
      (setf (terminal-ui-steering-input-previews ui) safe-steering
            (terminal-ui-queued-input-previews ui) safe-queued)
      (terminal-ui--set-draft-input ui input)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-set-input
    (terminal-ui (or string user-message-input)) terminal-ui)
(defun terminal-ui-set-input (ui input)
  "Replace UI's editable input with INPUT and repaint it."
  (with-terminal-ui-locked (ui)
    (terminal-ui--set-draft-input ui input)
    (terminal-ui--paint-live ui))
  ui)

(-> terminal-ui-set-cursor-visible (terminal-ui boolean) terminal-ui)
(defun terminal-ui-set-cursor-visible (ui visible-p)
  "Set whether UI leaves its input cursor visible between terminal updates."
  (with-terminal-ui-locked (ui)
    (when (terminal-interactive-p (terminal-ui-terminal ui))
      (live-region-set-cursor-visible (terminal-ui-live-region ui) visible-p)))
  ui)

(-> terminal-ui-resize
    (terminal-ui integer &key (:rows (option integer)))
    terminal-ui)
(defun terminal-ui-resize (ui columns &key rows)
  "Set UI terminal dimensions and repaint only unfinished rows."
  (with-terminal-ui-locked (ui)
    (let* ((new-columns (max 1 columns))
           (new-rows (and rows (max 1 rows)))
           (region (terminal-ui-live-region ui)))
      (setf (terminal-columns (terminal-ui-terminal ui)) new-columns)
      (when new-rows
        (setf (terminal-rows (terminal-ui-terminal ui)) new-rows))
      (live-region-resize
       region
       new-columns
       :maximum-rows
       (terminal-ui--maximum-live-rows (terminal-ui-terminal ui))
       :repaint-p nil)
      (terminal-ui--paint-live ui)))
  ui)

(-> terminal-ui-read-event (terminal-ui) t)
(defun terminal-ui-read-event (ui)
  "Read one semantic input event for UI without emitting fallback prompt controls."
  (terminal-read-event (terminal-ui-terminal ui)))

(-> terminal-ui--safe-editor-event (t) t)
(defun terminal-ui--safe-editor-event (event)
  "Return EVENT with direct text input sanitized for terminal presentation."
  (if (and (consp event)
           (member (first event) '(:insert :paste :line))
           (consp (rest event))
           (stringp (second event)))
      (list (first event) (sanitize-text (second event)))
      event))

(-> terminal-ui--apply-editor-event
    (terminal-ui t)
    (values keyword (option string)))
(defun terminal-ui--apply-editor-event (ui event)
  "Apply EVENT through Clinedi while preserving Autolith interaction policy."
  (let ((editor (terminal-ui-editor ui)))
    (cond
      ((and (eq event :interrupt)
            (plusp (length (line-editor-text editor))))
       (line-editor-clear editor)
       (setf (terminal-ui-image-attachments ui) nil)
       (values :cleared nil))
      ((and (consp event)
            (eq (first event) :paste)
            (stringp (second event))
            (terminal-ui--attach-pasted-image ui (second event)))
       (values :changed nil))
      ((eq event :complete)
       (line-editor-handle-event editor '(:insert "    "))
       (values :changed nil))
      ((eq event :edit-queue)
       (values :edit-queue nil))
      ((member event '(:up :down))
       (let* ((terminal (terminal-ui-terminal ui))
              (prompt-width
                (text-cell-width
                 (sanitize-text (terminal-ui-prompt ui)
                                :single-line-p t)))
              (direction (if (eq event :up) -1 1)))
         (if (line-editor-move-vertical
              editor direction
              :columns (terminal-columns terminal)
              :prompt-width prompt-width)
             (values :changed nil)
             (multiple-value-bind (action payload)
                 (line-editor-handle-event
                  editor
                  (if (eq event :up)
                      ':history-previous
                      ':history-next))
               (values (if (eq action :continue) ':changed action)
                       payload)))))
      ((eq event :queue-submit)
       (multiple-value-bind (action payload)
           (line-editor-handle-event editor :submit)
         (values (if (eq action :submit) ':queue action) payload)))
      ((eq event :clear-screen)
       (values :changed nil))
      (t
       (multiple-value-bind (action payload)
           (line-editor-handle-event
            editor
            (terminal-ui--safe-editor-event event))
         (values (if (eq action :continue) ':changed action)
                 payload))))))

(-> terminal-ui-process-event
    (terminal-ui t &key (:queue-completion-p boolean))
    (values keyword (option (or string user-message-input))))
(defun terminal-ui-process-event (ui event &key queue-completion-p)
  "Apply EVENT to UI's suggestions or editor and return its action and payload."
  (with-terminal-ui-locked (ui)
    (let* ((editor (terminal-ui-editor ui))
           (text-before (copy-seq (line-editor-text editor)))
           (images-before
             (terminal-ui--copy-image-attachments
              (terminal-ui-image-attachments ui)))
           (completion-items
             (and (eq event :complete)
                  (terminal-ui--reconcile-completions ui)))
           (effective-event
             (if (and (eq event :complete)
                      (null completion-items))
                 (cond
                   ((not queue-completion-p)
                    ':submit)
                   ((plusp (length (line-editor-text editor)))
                    ':queue-submit)
                   (t
                    ':edit-queue))
                 event)))
      (multiple-value-bind (completion-action completion-payload)
          (terminal-ui--handle-completion-event ui effective-event)
        (if completion-action
            (values completion-action completion-payload)
            (multiple-value-bind (action payload)
                (terminal-ui--apply-editor-event ui effective-event)
              (when (eq action :changed)
                (terminal-ui--restore-history-images ui))
              (when (and (member action '(:submit :queue))
                         (stringp payload))
                (terminal-ui--remember-image-submission
                 ui text-before images-before)
                (setf payload (terminal-ui--submission-input
                               ui payload)
                      (terminal-ui-image-attachments ui) nil))
              (when (member action '(:changed :cleared :submit :queue))
                (terminal-ui--repaint-live ui))
              (values action payload)))))))
