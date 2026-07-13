(in-package #:autolith)

;;;; -- Semantic Terminal Styles --

(deftype terminal-style ()
  "A semantic terminal style resolved to color and emphasis by the renderer."
  '(member :plain :brand
           :brand-gradient-1 :brand-gradient-2 :brand-gradient-3
           :brand-gradient-4 :brand-gradient-5 :brand-gradient-6
           :user :tool :success :failure :notice :dim :hint :selected
           :strong :emphasis :code))

;; General interface styles use the basic ANSI palette so Autolith follows the
;; terminal theme. Only the startup mark opts into the indexed brand gradient.
(define-constant +terminal-style-table+
  '((:brand    . "1;35")
    (:user     . "1;36")
    (:tool     . "1;33")
    (:success  . "32")
    (:failure  . "1;31")
    (:notice   . "33")
    (:dim      . "2")
    (:hint     . "2;3")
    (:selected . "7")
    (:strong   . "1")
    (:emphasis . "3")
    (:code     . "36"))
  :test #'equal
  :documentation "Select-graphic-rendition parameters for each semantic style.")

(define-constant +terminal-brand-gradient-table+
  '((:brand-gradient-1 . 193)
    (:brand-gradient-2 . 157)
    (:brand-gradient-3 . 121)
    (:brand-gradient-4 . 85)
    (:brand-gradient-5 . 84)
    (:brand-gradient-6 . 83))
  :test #'equal
  :documentation "Light-green xterm-256 colors for successive startup-mark rows.")

(define-constant +terminal-style-reset+
  (format nil "~C[0m" +terminal-escape-character+)
  :test #'string=
  :documentation "The trusted control that restores default terminal rendition.")

(-> terminal-indexed-color-environment-p
    ((option string) (option string))
    boolean)
(defun terminal-indexed-color-environment-p (term color-term)
  "Return true when TERM or COLOR-TERM advertises indexed-color support."
  (not
   (null
    (or (and term (search "256color" term :test #'char-equal))
        (and color-term
             (member color-term '("truecolor" "24bit")
                     :test #'string-equal))))))

(-> terminal-environment-indexed-color-p () boolean)
(defun terminal-environment-indexed-color-p ()
  "Return true when the process environment advertises indexed colors."
  (terminal-indexed-color-environment-p (uiop:getenv "TERM")
                                        (uiop:getenv "COLORTERM")))

(-> terminal-style-sequence
    (terminal-style &optional boolean)
    (option string))
(defun terminal-style-sequence
    (style &optional (indexed-color-p (terminal-environment-indexed-color-p)))
  "Return STYLE's trusted control, using INDEXED-COLOR-P for brand gradients."
  (let* ((indexed-color
           (rest (assoc style +terminal-brand-gradient-table+)))
         (parameters
           (cond
             ((and indexed-color indexed-color-p)
              (format nil "1;38;5;~D" indexed-color))
             (indexed-color
              "1;32")
             (t
              (rest (assoc style +terminal-style-table+))))))
    (and parameters
         (format nil "~C[~Am" +terminal-escape-character+ parameters))))

(-> terminal-environment-styling-p () boolean)
(defun terminal-environment-styling-p ()
  "Return true when the process environment permits color and emphasis output."
  (and (not (non-empty-string-p (uiop:getenv "NO_COLOR")))
       (not (string= (or (uiop:getenv "TERM") "") "dumb"))))


;;;; -- Styled Spans --

(-> terminal-span-p (t) boolean)
(defun terminal-span-p (value)
  "Return true when VALUE pairs a known terminal style with untrusted text."
  (and (consp value)
       (typep (first value) 'terminal-style)
       (stringp (rest value))))

(-> terminal-span (terminal-style string) cons)
(defun terminal-span (style text)
  "Return one styled span pairing STYLE with untrusted TEXT."
  (cons style text))

(-> terminal-span-style (cons) terminal-style)
(defun terminal-span-style (span)
  "Return SPAN's semantic style."
  (first span))

(-> terminal-span-text (cons) string)
(defun terminal-span-text (span)
  "Return SPAN's untrusted text."
  (rest span))

(-> terminal-styled-text-p (t) boolean)
(defun terminal-styled-text-p (value)
  "Return true when VALUE is a proper list of styled spans."
  (loop for tail = value then (rest tail)
        while tail
        always (and (consp tail)
                    (terminal-span-p (first tail)))))

(deftype terminal-styled-text ()
  "A proper list of styled spans rendered in order."
  '(satisfies terminal-styled-text-p))


;;;; -- Single-Row Span Layout --

(-> terminal--spans-width (list) (integer 0))
(defun terminal--spans-width (spans)
  "Return the total single-row cell width of sanitized SPANS."
  (loop for span in spans
        sum (text-cell-width
             (sanitize-text (terminal-span-text span)
                            :single-line-p t))))

(-> terminal--clip-spans (list integer) list)
(defun terminal--clip-spans (spans maximum-width)
  "Return single-row SPANS sanitized and clipped to at most MAXIMUM-WIDTH cells."
  (let ((remaining (max 0 maximum-width))
        (clipped nil))
    (dolist (span spans (nreverse clipped))
      (when (plusp remaining)
        (let* ((text (sanitize-text (terminal-span-text span)
                                    :single-line-p t))
               (visible (text-cell-prefix text remaining)))
          (when (plusp (length visible))
            (decf remaining (text-cell-width visible))
            (push (terminal-span (terminal-span-style span) visible)
                  clipped)))))))
