(in-package #:frob)

;;;; -- Markdown Renderer Object --

(defclass markdown-renderer ()
  ((width
    :initarg :width
    :reader markdown-renderer-width
    :type (integer 24)
    :documentation "The total cell budget for one rendered row including indents.")
   (code-open-p
    :initform nil
    :accessor markdown-renderer-code-open-p
    :type boolean
    :documentation "Whether rendering is inside a fenced code block.")
   (code-line-number
    :initform 1
    :accessor markdown-renderer-code-line-number
    :type (integer 1)
    :documentation "The gutter number given to the next fenced code line.")
   (continuation
    :initform nil
    :accessor markdown-renderer-continuation
    :type list
    :documentation "The prefix spans continuing a partially emitted logical line."))
  (:documentation
   "A line-oriented renderer turning a restrained markdown subset into styled rows."))

(-> markdown-renderer-create (&key (:width integer)) markdown-renderer)
(defun markdown-renderer-create (&key (width 80))
  "Create a markdown renderer wrapping rendered rows within WIDTH cells."
  (make-instance 'markdown-renderer :width (max 24 width)))


;;;; -- Inline Emphasis Spans --

(-> markdown--inline-close-position (string integer string) (option integer))
(defun markdown--inline-close-position (text start marker)
  "Return the first valid closing MARKER position in TEXT at or after START."
  (loop for position = (search marker text :start2 start)
          then (search marker text :start2 (1+ position))
        while position
        when (and (plusp position)
                  (not (char= (char text (1- position)) #\Space)))
          return position))

(-> markdown--inline-openable-p (string integer string) boolean)
(defun markdown--inline-openable-p (text index marker)
  "Return true when MARKER at INDEX opens an emphasis span that also closes."
  (let ((after (+ index (length marker))))
    (and (< after (length text))
         (not (char= (char text after) #\Space))
         (not (null (markdown--inline-close-position text after marker))))))

(-> markdown--parse-inline (string) (values string vector vector))
(defun markdown--parse-inline (text)
  "Return TEXT without inline markers plus per-character styles and source indices.

Recognizes `code`, **strong**, and *emphasis* spans whose delimiters pair
within TEXT. Unpaired delimiters stay literal. The second value maps each
rendered character to a style keyword and the third to its source index."
  (let ((rendered (make-array (length text)
                              :element-type 'character
                              :adjustable t
                              :fill-pointer 0))
        (styles (make-array (length text) :adjustable t :fill-pointer 0))
        (sources (make-array (length text) :adjustable t :fill-pointer 0))
        (mode ':plain)
        (index 0))
    (labels ((emit ()
               "Copy the current source character with the active style."
               (vector-push-extend (char text index) rendered)
               (vector-push-extend mode styles)
               (vector-push-extend index sources)
               (incf index))

             (marker-p (marker)
               "Return true when MARKER occurs at the current index."
               (let ((end (+ index (length marker))))
                 (and (<= end (length text))
                      (string= text marker :start1 index :end1 end))))

             (closable-p ()
               "Return true when a closing delimiter is valid at this index."
               (and (plusp index)
                    (not (char= (char text (1- index)) #\Space)))))
      (loop while (< index (length text))
            do (cond
                 ((eq mode ':code)
                  (if (char= (char text index) #\`)
                      (progn
                        (setf mode ':plain)
                        (incf index))
                      (emit)))
                 ((and (char= (char text index) #\`)
                       (position #\` text :start (1+ index)))
                  (setf mode ':code)
                  (incf index))
                 ((eq mode ':strong)
                  (if (and (marker-p "**") (closable-p))
                      (progn
                        (setf mode ':plain)
                        (incf index 2))
                      (emit)))
                 ((eq mode ':emphasis)
                  (if (and (char= (char text index) #\*) (closable-p))
                      (progn
                        (setf mode ':plain)
                        (incf index))
                      (emit)))
                 ((and (marker-p "**")
                       (markdown--inline-openable-p text index "**"))
                  (setf mode ':strong)
                  (incf index 2))
                 ((and (char= (char text index) #\*)
                       (not (marker-p "**"))
                       (markdown--inline-openable-p text index "*"))
                  (setf mode ':emphasis)
                  (incf index))
                 (t
                  (emit)))))
    (values (coerce rendered 'string) styles sources)))

(-> markdown--style-runs (string vector integer integer) list)
(defun markdown--style-runs (rendered styles start end)
  "Return spans for RENDERED between START and END grouped by equal style."
  (let ((spans nil)
        (run-start start))
    (loop for position from start below end
          when (and (> position run-start)
                    (not (eq (aref styles position)
                             (aref styles (1- position)))))
            do (push (terminal-span (aref styles (1- position))
                                    (subseq rendered run-start position))
                     spans)
               (setf run-start position))
    (when (< run-start end)
      (push (terminal-span (aref styles (1- end))
                           (subseq rendered run-start end))
            spans))
    (nreverse spans)))

(-> markdown--style-marker (terminal-style) string)
(defun markdown--style-marker (style)
  "Return the source delimiter reopening STYLE in retained streaming text."
  (case style
    (:strong "**")
    (:emphasis "*")
    (:code "`")
    (otherwise "")))


;;;; -- Line Classification --

(-> markdown--fence-line-p (string) (values boolean string))
(defun markdown--fence-line-p (line)
  "Return whether LINE is a code fence, plus its trimmed language tag."
  (if (uiop:string-prefix-p "```" line)
      (values t (string-trim " `" (subseq line 3)))
      (values nil "")))

(-> markdown--leading-spaces (string) integer)
(defun markdown--leading-spaces (line)
  "Return the number of leading space characters in LINE."
  (or (position #\Space line :test-not #'char=)
      (length line)))

(-> markdown--bullet-content (string) (values (option string) integer))
(defun markdown--bullet-content (line)
  "Return LINE's bullet item content and leading indent, or NIL and zero."
  (let ((lead (markdown--leading-spaces line)))
    (if (and (< (1+ lead) (length line))
             (find (char line lead) "-*+")
             (char= (char line (1+ lead)) #\Space))
        (values (subseq line (+ lead 2)) lead)
        (values nil 0))))

(-> markdown--numbered-content (string) (values (option string) integer string))
(defun markdown--numbered-content (line)
  "Return LINE's numbered item content, leading indent, and marker text."
  (block nil
    (let* ((lead (markdown--leading-spaces line))
           (digits-end (loop for position from lead below (length line)
                             while (digit-char-p (char line position))
                             finally (return position))))
      (unless (and (> digits-end lead)
                   (<= (- digits-end lead) 3)
                   (< (1+ digits-end) (length line))
                   (find (char line digits-end) ".)")
                   (char= (char line (1+ digits-end)) #\Space))
        (return (values nil 0 "")))
      (values (subseq line (+ digits-end 2))
              lead
              (subseq line lead (1+ digits-end))))))

(-> markdown--line-layout (string) (values string list list))
(defun markdown--line-layout (line)
  "Return LINE's inline content plus its first-row and continuation prefixes."
  (multiple-value-bind (bullet-content bullet-lead)
      (markdown--bullet-content line)
    (multiple-value-bind (numbered-content numbered-lead numbered-marker)
        (markdown--numbered-content line)
      (cond
        (bullet-content
         (let ((lead (make-string (+ 2 bullet-lead) :initial-element #\Space)))
           (values bullet-content
                   (list (terminal-span ':plain lead)
                         (terminal-span ':brand "• "))
                   (list (terminal-span ':plain
                                        (concatenate 'string lead "  "))))))
        (numbered-content
         (let ((lead (make-string (+ 2 numbered-lead) :initial-element #\Space))
               (marker (concatenate 'string numbered-marker " ")))
           (values numbered-content
                   (list (terminal-span ':plain lead)
                         (terminal-span ':brand marker))
                   (list (terminal-span
                          ':plain
                          (concatenate 'string
                                       lead
                                       (make-string (length marker)
                                                    :initial-element #\Space)))))))
        (t
         (values line
                 (list (terminal-span ':plain "  "))
                 (list (terminal-span ':plain "  "))))))))


;;;; -- Row Assembly --

(-> markdown--code-prefixes (markdown-renderer) (values list list))
(defun markdown--code-prefixes (renderer)
  "Return RENDERER's numbered first-row and unnumbered continuation gutters."
  (values (list (terminal-span
                 ':dim
                 (format nil "  ~3D │ "
                         (markdown-renderer-code-line-number renderer))))
          (list (terminal-span ':dim "      │ "))))

(-> markdown--wrapped-rows
    (markdown-renderer string list list)
    (values list list list))
(defun markdown--wrapped-rows (renderer content first-prefix continuation-prefix)
  "Return CONTENT's prefixed styled rows plus rendered start offsets and parse state.

The second value gives each row's start offset within the rendered inline
text, and the third value is the list (RENDERED STYLES SOURCES) from inline
parsing, letting streaming callers map rendered offsets back to source text."
  (multiple-value-bind (rendered styles sources)
      (markdown--parse-inline content)
    (let* ((prefix-width (max (terminal--spans-width first-prefix)
                              (terminal--spans-width continuation-prefix)))
           (content-width (max 8 (- (markdown-renderer-width renderer)
                                    prefix-width)))
           (rows nil)
           (starts nil)
           (cursor 0))
      (loop for row-text in (terminal--wrap-text rendered content-width)
            for first-row-p = t then nil
            do (let ((start (if (zerop (length row-text))
                                cursor
                                (search row-text rendered :start2 cursor))))
                 (push (append (if first-row-p
                                   first-prefix
                                   continuation-prefix)
                               (markdown--style-runs rendered
                                                     styles
                                                     start
                                                     (+ start
                                                        (length row-text))))
                       rows)
                 (push start starts)
                 (setf cursor (+ start (length row-text)))))
      (values (nreverse rows)
              (nreverse starts)
              (list rendered styles sources)))))

(-> markdown--code-rows (markdown-renderer string) (values list list))
(defun markdown--code-rows (renderer line)
  "Return fenced code LINE as gutter-numbered rows plus rendered start offsets."
  (multiple-value-bind (first-prefix continuation-prefix)
      (markdown--code-prefixes renderer)
    (let* ((continuation (markdown-renderer-continuation renderer))
           (content-width (max 8 (- (markdown-renderer-width renderer) 8)))
           (rows nil)
           (starts nil)
           (cursor 0))
      (loop for row-text in (terminal--wrap-text line content-width)
            for first-row-p = t then nil
            do (let ((start (if (zerop (length row-text))
                                cursor
                                (search row-text line :start2 cursor))))
                 (push (append (if first-row-p
                                   (or continuation first-prefix)
                                   continuation-prefix)
                               (when (plusp (length row-text))
                                 (list (terminal-span ':plain row-text))))
                       rows)
                 (push start starts)
                 (setf cursor (+ start (length row-text)))))
      (unless continuation
        (incf (markdown-renderer-code-line-number renderer)))
      (values (nreverse rows) (nreverse starts)))))


;;;; -- Public Rendering Operations --

(-> markdown-render-line (markdown-renderer string) list)
(defun markdown-render-line (renderer line)
  "Return sanitized logical LINE as styled transcript rows, updating RENDERER."
  (multiple-value-bind (fence-p language)
      (markdown--fence-line-p line)
    (cond
      ((and (markdown-renderer-code-open-p renderer) fence-p)
       (setf (markdown-renderer-code-open-p renderer) nil
             (markdown-renderer-continuation renderer) nil)
       (list (list (terminal-span ':dim "  ```"))))
      ((markdown-renderer-code-open-p renderer)
       (multiple-value-bind (rows starts)
           (markdown--code-rows renderer line)
         (declare (ignore starts))
         (setf (markdown-renderer-continuation renderer) nil)
         rows))
      (fence-p
       (setf (markdown-renderer-code-open-p renderer) t
             (markdown-renderer-code-line-number renderer) 1
             (markdown-renderer-continuation renderer) nil)
       (list (list (terminal-span ':dim (format nil "  ```~A" language)))))
      ((markdown-renderer-continuation renderer)
       (let ((prefix (markdown-renderer-continuation renderer)))
         (setf (markdown-renderer-continuation renderer) nil)
         (markdown--wrapped-rows renderer line prefix prefix)))
      ((zerop (length (string-trim " " line)))
       (list nil))
      (t
       (multiple-value-bind (content first-prefix continuation-prefix)
           (markdown--line-layout line)
         (markdown--wrapped-rows renderer
                                 content
                                 first-prefix
                                 continuation-prefix))))))

(-> markdown-render-partial
    (markdown-renderer string)
    (values list list string))
(defun markdown-render-partial (renderer partial)
  "Return PARTIAL's overflow rows, its live tail row, and the retained source.

Rows for every completed wrapped row are returned for immediate transcript
commitment, the tail row previews the unfinished remainder, and the retained
source keeps unconsumed delimiters so later text continues correctly."
  (block nil
    (when (markdown-renderer-code-open-p renderer)
      (when (and (null (markdown-renderer-continuation renderer))
                 (uiop:string-prefix-p "```" partial))
        (return (values nil
                        (list (terminal-span ':dim
                                             (format nil "  ~A" partial)))
                        partial)))
      (multiple-value-bind (rows starts)
          (markdown--code-rows renderer partial)
        (when (= (length rows) 1)
          ;; Undo the speculative line number; the line has not committed yet.
          (unless (markdown-renderer-continuation renderer)
            (decf (markdown-renderer-code-line-number renderer)))
          (return (values nil (first rows) partial)))
        (multiple-value-bind (first-prefix continuation-prefix)
            (markdown--code-prefixes renderer)
          (declare (ignore first-prefix))
          (setf (markdown-renderer-continuation renderer) continuation-prefix)
          (return (values (butlast rows)
                          (first (last rows))
                          (subseq partial (first (last starts))))))))
    (when (and (null (markdown-renderer-continuation renderer))
               (uiop:string-prefix-p "```" partial))
      (return (values nil
                      (list (terminal-span ':dim
                                           (format nil "  ~A" partial)))
                      partial)))
    (let ((continuation (markdown-renderer-continuation renderer)))
      (multiple-value-bind (content first-prefix continuation-prefix)
          (if continuation
              (values partial continuation continuation)
              (markdown--line-layout partial))
        (multiple-value-bind (rows starts parse)
            (markdown--wrapped-rows renderer content
                                    first-prefix
                                    continuation-prefix)
          (when (<= (length rows) 1)
            (return (values nil (first rows) partial)))
          (destructuring-bind (rendered styles sources) parse
            (declare (ignore rendered))
            (let* ((retained-start (first (last starts)))
                   (marker (markdown--style-marker
                            (aref styles retained-start)))
                   (source-start (aref sources retained-start))
                   (source-offset (- (length partial) (length content))))
              (setf (markdown-renderer-continuation renderer)
                    continuation-prefix)
              (values (butlast rows)
                      (first (last rows))
                      (concatenate 'string
                                   marker
                                   (subseq partial
                                           (+ source-start
                                              source-offset)))))))))))
