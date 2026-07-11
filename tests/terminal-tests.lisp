(in-package #:frob)

;;;; -- Recording Terminal --

(defclass recording-terminal (terminal)
  ((chunks
    :initform nil
    :accessor recording-terminal-chunks
    :type list
    :documentation "Trusted renderer writes captured in reverse chronological order."))
  (:documentation "A deterministic interactive terminal used by terminal seam tests."))

(defmethod terminal--write ((terminal recording-terminal) (text string))
  "Capture trusted TEXT written through TERMINAL."
  (push text (recording-terminal-chunks terminal))
  nil)

(defmethod terminal-flush ((terminal recording-terminal))
  "Finish a recording TERMINAL write batch without external effects."
  (declare (ignore terminal))
  nil)

(defmethod terminal-start ((terminal recording-terminal))
  "Start TERMINAL and emulate only bracketed paste activation."
  (unless (terminal-started-p terminal)
    (setf (terminal-started-p terminal) t
          (terminal-interactive-p terminal) t)
    (terminal--write terminal +terminal-bracketed-paste-enable+))
  terminal)

(defmethod terminal-stop ((terminal recording-terminal))
  "Stop TERMINAL and emulate only bracketed paste deactivation."
  (when (terminal-started-p terminal)
    (terminal--write terminal +terminal-bracketed-paste-disable+)
    (setf (terminal-started-p terminal) nil
          (terminal-interactive-p terminal) nil))
  terminal)

(defmethod terminal-read-event ((terminal recording-terminal))
  "Return end-of-input because recording terminals have no input queue."
  (declare (ignore terminal))
  :end-of-input)


;;;; -- Scripted Terminal --

(defclass scripted-terminal (recording-terminal)
  ((events
    :initarg :events
    :initform nil
    :accessor scripted-terminal-events
    :type list
    :documentation "Queued semantic input events served to the reader in order."))
  (:documentation "A recording terminal replaying scripted input events."))

(defmethod terminal-read-event ((terminal scripted-terminal))
  "Serve the next scripted event, or end of input when exhausted."
  (or (pop (scripted-terminal-events terminal)) :end-of-input))


;;;; -- Test Helpers --

(-> recording-terminal-output (recording-terminal) string)
(defun recording-terminal-output (terminal)
  "Return all output captured by TERMINAL in write order."
  (with-output-to-string (stream)
    (dolist (chunk (reverse (recording-terminal-chunks terminal)))
      (write-string chunk stream))))

(-> recording-terminal-reset (recording-terminal) recording-terminal)
(defun recording-terminal-reset (terminal)
  "Discard output previously captured by TERMINAL."
  (setf (recording-terminal-chunks terminal) nil)
  terminal)

(-> terminal-tests--substring-count (string string) (integer 0))
(defun terminal-tests--substring-count (needle haystack)
  "Return the number of non-overlapping NEEDLE occurrences in HAYSTACK."
  (loop with start = 0
        for position = (search needle haystack :start2 start)
        while position
        count t
        do (setf start (+ position (length needle)))))

(-> terminal-tests--csi-final-index (string integer) (option integer))
(defun terminal-tests--csi-final-index (text start)
  "Return the final-byte index for a CSI in TEXT beginning at START."
  (loop for index from start below (length text)
        when (<= #x40 (char-code (char text index)) #x7e)
          return index))

(-> terminal-tests--private-mode-parameters-p (string integer integer) boolean)
(defun terminal-tests--private-mode-parameters-p (text start end)
  "Return true when TEXT parameters between START and END select an alternate screen."
  (and (< start end)
       (char= (char text start) #\?)
       (loop with parameter-start = (1+ start)
             for separator = (or (position #\; text
                                           :start parameter-start
                                           :end end)
                                 end)
             for value = (parse-integer text
                                        :start parameter-start
                                        :end separator
                                        :junk-allowed t)
             thereis (member value '(47 1047 1049))
             while (< separator end)
             do (setf parameter-start (1+ separator)))))

(-> terminal-tests--forbidden-control-p (string) boolean)
(defun terminal-tests--forbidden-control-p (text)
  "Return true when TEXT enters an alternate screen or erases a display or scrollback."
  (block nil
    (loop with index = 0
          while (< index (length text))
          for character = (char text index)
          for code = (char-code character)
          do (cond
               ((and (= code 27)
                     (< (1+ index) (length text))
                     (char= (char text (1+ index)) #\c))
                (return t))
               ((or (and (= code 27)
                         (< (1+ index) (length text))
                         (char= (char text (1+ index)) #\[))
                    (= code #x9b))
                (let* ((parameter-start (if (= code #x9b)
                                            (1+ index)
                                            (+ index 2)))
                       (final-index
                         (terminal-tests--csi-final-index text parameter-start)))
                  (unless final-index
                    (return t))
                  (let ((final (char text final-index)))
                    (when (or (char= final #\J)
                              (and (member final '(#\h #\l))
                                   (terminal-tests--private-mode-parameters-p
                                    text parameter-start final-index)))
                      (return t)))
                  (setf index final-index)))
               (t
                nil))
             (incf index))
    nil))

(-> terminal-tests--contains-control-character-p (string) boolean)
(defun terminal-tests--contains-control-character-p (text)
  "Return true when TEXT contains an untrusted ESC or C1 control character."
  (loop for character across text
        for code = (char-code character)
        thereis (or (= code 27)
                    (<= 128 code 159))))


;;;; -- Focused Terminal Tests --

(-> test-terminal-primary-screen-controls () null)
(defun test-terminal-primary-screen-controls ()
  "Test primary-screen rendering, bounded live updates, and finalized deduplication."
  (let* ((terminal (make-instance 'recording-terminal :columns 24))
         (ui (terminal-ui-create :terminal terminal :prompt "frob> ")))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui "working")
      (terminal-ui-process-event active-ui '(:insert "hello"))
      (test-assert
       (terminal-ui-append-finalized active-ui 1 "FINAL-SENTINEL")
       "the first finalized transcript event is emitted")
      (test-assert
       (not (terminal-ui-append-finalized active-ui 1 "DUPLICATE"))
       "a finalized transcript identifier is emitted only once")
      (terminal-ui-set-status active-ui "tool complete")
      (terminal-ui-resize active-ui 12))
    (let ((output (recording-terminal-output terminal)))
      (test-assert
       (= (terminal-tests--substring-count "FINAL-SENTINEL" output) 1)
       "finalized transcript text appears exactly once")
      (test-assert
       (not (search "DUPLICATE" output))
       "duplicate finalized transcript text is absent")
      (test-assert
       (not (terminal-tests--forbidden-control-p output))
       "terminal output never clears a display or enters an alternate screen")))
  nil)

(-> test-terminal-untrusted-text () null)
(defun test-terminal-untrusted-text ()
  "Test that every untrusted text path neutralizes terminal control injection."
  (let* ((escape (string +terminal-escape-character+))
         (c1 (string (code-char #x9b)))
         (malicious
           (concatenate 'string
                        "before"
                        escape "[?1049h"
                        escape "[3J"
                        escape "c"
                        c1 "?47h"
                        "after"))
         (terminal (make-instance 'recording-terminal :columns 40))
         (ui (terminal-ui-create :terminal terminal :prompt malicious)))
    (with-terminal-ui (active-ui ui)
      (terminal-ui-set-status active-ui malicious)
      (terminal-ui-process-event active-ui (list :paste malicious))
      (terminal-ui-append-finalized active-ui :malicious malicious))
    (let ((output (recording-terminal-output terminal))
          (editor-text (line-editor-text (terminal-ui-editor ui))))
      (test-assert
       (not (terminal-tests--forbidden-control-p output))
       "untrusted content cannot inject forbidden terminal controls")
      (test-assert
       (not (terminal-tests--contains-control-character-p editor-text))
       "pasted input stores no ESC or C1 control characters")))
  nil)

(-> test-terminal-finalized-scrollback () null)
(defun test-terminal-finalized-scrollback ()
  "Test that resize and live activity never replay finalized transcript rows."
  (let* ((terminal (make-instance 'recording-terminal :columns 18))
         (ui (terminal-ui-create :terminal terminal)))
    (terminal-ui-start ui)
    (loop for identifier from 1 to 20
          do (terminal-ui-append-finalized
              ui
              identifier
              (format nil "IMMUTABLE-~2,'0D" identifier)))
    (recording-terminal-reset terminal)
    (terminal-ui-set-status ui "streaming token one")
    (terminal-ui-set-status ui "streaming token two")
    (terminal-ui-process-event ui '(:insert "draft"))
    (terminal-ui-resize ui 9)
    (let ((live-output (recording-terminal-output terminal)))
      (loop for identifier from 1 to 20
            do (test-assert
                (not (search (format nil "IMMUTABLE-~2,'0D" identifier)
                             live-output))
                "live repaint does not replay finalized transcript text"))
      (test-assert
       (not (terminal-tests--forbidden-control-p live-output))
       "live repaint and resize preserve terminal scrollback"))
    (terminal-ui-stop ui))
  nil)

(-> test-terminal-line-editor () null)
(defun test-terminal-line-editor ()
  "Test editing, history draft restoration, control keys, arrows, and paste safety."
  (let ((editor (line-editor-create :history-limit 2)))
    (line-editor-handle-event editor '(:insert "abc"))
    (line-editor-handle-event editor :left)
    (line-editor-handle-event editor '(:insert "X"))
    (test-assert (string= (line-editor-text editor) "abXc")
                 "left arrow changes the insertion point")
    (line-editor-handle-event editor :home)
    (line-editor-handle-event editor '(:insert ">"))
    (line-editor-handle-event editor :end)
    (line-editor-handle-event editor :backspace)
    (test-assert (string= (line-editor-text editor) ">abX")
                 "home, end, and backspace edit the expected characters")
    (multiple-value-bind (action submitted)
        (line-editor-handle-event editor :submit)
      (test-assert (eq action :submit)
                   "enter produces a submit action")
      (test-assert (string= submitted ">abX")
                   "enter returns the complete input"))
    (line-editor-handle-event editor '(:insert "draft"))
    (line-editor-handle-event editor :history-previous)
    (test-assert (string= (line-editor-text editor) ">abX")
                 "up arrow recalls the newest history entry")
    (line-editor-handle-event editor :history-next)
    (test-assert (string= (line-editor-text editor) "draft")
                 "down arrow restores the draft")
    (multiple-value-bind (action payload)
        (line-editor-handle-event editor :interrupt)
      (declare (ignore payload))
      (test-assert (eq action :cleared)
                   "Ctrl-C clears non-empty editor input"))
    (multiple-value-bind (action payload)
        (line-editor-handle-event editor :interrupt)
      (declare (ignore payload))
      (test-assert (eq action :interrupt)
                   "Ctrl-C interrupts when the editor is empty"))
    (multiple-value-bind (action payload)
        (line-editor-handle-event editor :end-of-input)
      (declare (ignore payload))
      (test-assert (eq action :end-of-input)
                   "Ctrl-D exits when the editor is empty")))
  nil)

(-> test-terminal-input-decoding () null)
(defun test-terminal-input-decoding ()
  "Test production escape decoding and complete bracketed paste collection."
  (let* ((escape +terminal-escape-character+)
         (input
           (concatenate 'string
                        (format nil "~C[A" escape)
                        +terminal-bracketed-paste-start+
                        "paste"
                        (format nil "~C[3J" escape)
                        +terminal-bracketed-paste-end+))
         (terminal
           (make-instance 'stream-terminal
                          :input-stream (make-string-input-stream input)
                          :output-stream (make-string-output-stream)
                          :input-file-descriptor 0
                          :interactive-p t
                          :columns 40)))
    (test-assert (eq (terminal-read-event terminal) :history-previous)
                 "the production decoder recognizes an up arrow")
    (let ((paste-event (terminal-read-event terminal)))
      (test-assert (eq (first paste-event) :paste)
                   "the production decoder recognizes bracketed paste")
      (let ((editor (line-editor-create)))
        (line-editor-handle-event editor paste-event)
        (test-assert
         (not (terminal-tests--contains-control-character-p
               (line-editor-text editor)))
         "bracketed paste terminal controls are neutralized before display"))))
  nil)

(-> test-terminal-live-region-layout () null)
(defun test-terminal-live-region-layout ()
  "Test placeholder hints, styled span emission, and finalized entry separation."
  (let* ((terminal (make-instance 'recording-terminal
                                  :columns 40
                                  :styled-p t))
         (ui (terminal-ui-create :terminal terminal
                                 :prompt "❯ "
                                 :placeholder "hint text")))
    (with-terminal-ui (active-ui ui)
      (test-assert (search "hint text" (recording-terminal-output terminal))
                   "an empty prompt row shows the placeholder hint")
      (recording-terminal-reset terminal)
      (terminal-ui-process-event active-ui '(:insert "a"))
      (let ((typing (recording-terminal-output terminal)))
        (test-assert (not (search "hint text" typing))
                     "typed input replaces the placeholder hint")
        (test-assert (search "a" typing)
                     "typed input is painted on the prompt row"))
      (recording-terminal-reset terminal)
      (terminal-ui-set-status active-ui "working")
      (test-assert (search "∙ " (recording-terminal-output terminal))
                   "the status row carries its activity glyph")
      (recording-terminal-reset terminal)
      (terminal-ui-append-finalized
       active-ui
       :styled
       (list (terminal-span :brand "frob")
             (terminal-span :plain " ready")))
      (let ((finalized (recording-terminal-output terminal)))
        (test-assert
         (search (format nil "~C[1;35mfrob" +terminal-escape-character+)
                 finalized)
         "styled finalized spans emit basic rendition controls")
        (test-assert
         (search (format nil " ready~C~C~C~C" #\Return #\Newline
                         #\Return #\Newline)
                 finalized)
         "finalized entries end with one separating blank row")
        (test-assert (not (terminal-tests--forbidden-control-p finalized))
                     "styled transcript output never erases the display"))))
  (let* ((plain-terminal (make-instance 'recording-terminal :columns 40))
         (plain-ui (terminal-ui-create :terminal plain-terminal)))
    (with-terminal-ui (active-ui plain-ui)
      (terminal-ui-append-finalized active-ui
                                    :styled
                                    (list (terminal-span :brand "frob"))))
    (test-assert (not (search "[1;35m" (recording-terminal-output plain-terminal)))
                 "styling is omitted when the terminal does not permit it"))
  nil)

(-> test-terminal-narrow-live-region () null)
(defun test-terminal-narrow-live-region ()
  "Test typed prompt layout and repaint bookkeeping at minimal terminal widths."
  (dolist (columns '(1 2))
    (let* ((terminal (make-instance 'recording-terminal :columns columns))
           (ui (terminal-ui-create :terminal terminal :prompt "❯ ")))
      (with-terminal-ui (active-ui ui)
        (terminal-ui-process-event active-ui '(:insert "a"))
        (multiple-value-bind (rows cursor-row cursor-column)
            (terminal-ui--live-rows active-ui)
          (let ((row-width (max 0 (1- columns))))
            (test-assert
             (every (lambda (row)
                      (<= (terminal--spans-width row) row-width))
                    rows)
             (format nil "every live row fits a ~D-column terminal" columns))
            (test-assert
             (<= cursor-column row-width)
             (format nil "the live cursor fits a ~D-column terminal" columns))
            (test-assert
             (= (terminal-ui-live-row-count active-ui) (length rows))
             (format nil
                     "repaint bookkeeping counts rows at ~D columns"
                     columns))
            (test-assert
             (= (terminal-ui-live-cursor-row active-ui) cursor-row)
             (format nil
                     "repaint bookkeeping tracks the cursor at ~D columns"
                     columns)))))))
  nil)

(-> test-terminal-stream-update () null)
(defun test-terminal-stream-update ()
  "Test continuous streamed blocks, fluid tail repaint, and block completion."
  (let* ((terminal (make-instance 'recording-terminal :columns 40))
         (ui (terminal-ui-create :terminal terminal :placeholder "hint")))
    (with-terminal-ui (active-ui ui)
      (recording-terminal-reset terminal)
      (terminal-ui-stream-update
       active-ui
       :rows (list (list (terminal-span :brand "● frob"))
                   (list (terminal-span :plain "  first line")))
       :tail "  partial")
      (let ((output (recording-terminal-output terminal)))
        (test-assert (search "● frob" output)
                     "streamed rows append the block header")
        (test-assert (search "  first line" output)
                     "streamed rows append committed lines")
        (test-assert (search "  partial" output)
                     "the fluid tail is painted live")
        (test-assert (not (terminal-tests--forbidden-control-p output))
                     "streamed rows never erase the display"))
      (recording-terminal-reset terminal)
      (terminal-ui-stream-update active-ui :rows (list nil) :tail nil)
      (test-assert (not (search "partial" (recording-terminal-output terminal)))
                   "completing a block removes the fluid tail")
      (test-assert (null (terminal-ui-stream-tail active-ui))
                   "a completed block clears the stored tail")))
  nil)

(-> test-terminal-command-completion () null)
(defun test-terminal-command-completion ()
  "Test suggestion filtering, selection movement, acceptance, and submission."
  (let* ((terminal (make-instance 'recording-terminal :columns 60))
         (completions
           '((:name "/help" :argument nil :description "show this reference")
             (:name "/resume" :argument "ID" :description "load a conversation")
             (:name "/rollback" :argument "ID" :description "select a generation")
             (:name "/quit" :argument nil :description "leave Frob")))
         (ui (terminal-ui-create :terminal terminal
                                 :completions completions)))
    (with-terminal-ui (active-ui ui)
      (let ((editor (terminal-ui-editor active-ui)))
        (recording-terminal-reset terminal)
        (terminal-ui-process-event active-ui '(:insert "/r"))
        (let ((painted (recording-terminal-output terminal)))
          (test-assert (search "/resume ID" painted)
                       "typing a command prefix paints matching suggestions")
          (test-assert (search "/rollback ID" painted)
                       "every matching command is suggested")
          (test-assert (not (search "/quit" painted))
                       "commands outside the typed prefix are not suggested"))
        (terminal-ui-process-event active-ui :complete)
        (test-assert (string= (line-editor-text editor) "/resume ")
                     "tab fills the selected command and its argument space")
        (test-assert (null (terminal-ui--matching-completions active-ui))
                     "a completed argument command leaves suggestion mode")
        (terminal-ui-process-event active-ui :interrupt)
        (terminal-ui-process-event active-ui '(:insert "/r"))
        (terminal-ui-process-event active-ui :history-next)
        (terminal-ui-process-event active-ui :complete)
        (test-assert (string= (line-editor-text editor) "/rollback ")
                     "arrow keys move the completion selection")
        (terminal-ui-process-event active-ui :interrupt)
        (terminal-ui-process-event active-ui '(:insert "/q"))
        (multiple-value-bind (action payload)
            (terminal-ui-process-event active-ui :submit)
          (test-assert (eq action :submit)
                       "enter on an argument-free suggestion submits")
          (test-assert (string= payload "/quit")
                       "enter submits the completed command name"))
        (terminal-ui-process-event active-ui '(:insert "plain text"))
        (terminal-ui-process-event active-ui :complete)
        (test-assert (string= (line-editor-text editor) "plain text    ")
                     "tab outside suggestion mode keeps inserting spaces")
        (terminal-ui-process-event active-ui :interrupt))))
  nil)

(-> test-terminal-modal-selection () null)
(defun test-terminal-modal-selection ()
  "Test modal picker navigation, acceptance, cancellation, and cleanup."
  (let* ((items '((:name "alpha" :argument nil :description "first entry")
                  (:name "beta" :argument nil :description "second entry")
                  (:name "gamma" :argument nil :description "third entry")))
         (terminal (make-instance 'scripted-terminal
                                  :columns 60
                                  :events (list :history-next :submit)))
         (ui (terminal-ui-create :terminal terminal)))
    (with-terminal-ui (active-ui ui)
      (recording-terminal-reset terminal)
      (test-assert (string= (terminal-ui-select active-ui
                                                :title "pick one"
                                                :items items)
                            "beta")
                   "arrow keys move the modal selection before enter")
      (let ((painted (recording-terminal-output terminal)))
        (test-assert (search "pick one" painted)
                     "the picker paints its title")
        (test-assert (search "alpha" painted)
                     "the picker paints its items")
        (test-assert (not (terminal-tests--forbidden-control-p painted))
                     "the picker never erases the display"))
      (test-assert (null (terminal-ui-selector active-ui))
                   "the selector state clears after selection")
      (setf (scripted-terminal-events terminal) (list :escape))
      (test-assert (null (terminal-ui-select active-ui
                                             :title "pick one"
                                             :items items))
                   "escape cancels the picker")))
  (let* ((terminal (make-instance 'scripted-terminal :columns 60))
         (ui (terminal-ui-create :terminal terminal)))
    (test-assert (null (terminal-ui-select
                        ui
                        :title "pick"
                        :items '((:name "a" :argument nil :description "d"))))
                 "non-interactive terminals never open the picker"))
  nil)

(-> test-terminal-styling-primitives () null)
(defun test-terminal-styling-primitives ()
  "Test semantic style resolution, span safety, clipping, and word wrapping."
  (test-assert
   (let ((sequence (terminal-style-sequence :dim)))
     (and (stringp sequence)
          (char= (char sequence 0) +terminal-escape-character+)
          (char= (char sequence (1- (length sequence))) #\m)))
   "semantic styles resolve to rendition controls")
  (test-assert (null (terminal-style-sequence :plain))
               "the plain style resolves to no control sequence")
  (test-assert
   (terminal-styled-text-p (list (terminal-span :brand "frob")
                                 (terminal-span :plain " ready")))
   "lists of spans form styled text")
  (test-assert (not (terminal-styled-text-p (list "bare string")))
               "bare strings are not styled text")
  (test-assert (not (terminal-styled-text-p (terminal-span :dim "x")))
               "a dotted span alone is not styled text")
  (let ((clipped (terminal--clip-spans
                  (list (terminal-span :user "abc")
                        (terminal-span :dim "defg"))
                  5)))
    (test-assert
     (equal clipped (list (terminal-span :user "abc")
                          (terminal-span :dim "de")))
     "span clipping preserves styles across the width boundary"))
  (test-assert
   (= (terminal--spans-width (list (terminal-span :user "abc")
                                   (terminal-span :dim "de")))
      5)
   "span width sums sanitized cell widths")
  (let ((hostile (terminal--clip-spans
                  (list (terminal-span :plain
                                       (format nil "a~C[31mb~%c"
                                               +terminal-escape-character+)))
                  40)))
    (test-assert
     (not (terminal-tests--contains-control-character-p
           (terminal-span-text (first hostile))))
     "span clipping neutralizes untrusted controls and newlines"))
  (let ((cases '(("" 10 (""))
                 ("hello" 10 ("hello"))
                 ("aa bb" 2 ("aa" "bb"))
                 ("ab cd" 4 ("ab" "cd"))
                 ("abcdef" 3 ("abc" "def"))
                 ("one two three" 7 ("one two" "three"))
                 ("日本語" 4 ("日本" "語")))))
    (loop for (text width expected) in cases
          do (test-assert
              (equal (terminal--wrap-text text width) expected)
              (format nil "wrapping ~S at ~D produces ~S" text width expected))))
  (test-assert
   (equal (terminal--wrap-text (format nil "alpha~%beta gamma") 5)
          '("alpha" "beta" "gamma"))
   "wrapping preserves explicit line breaks before width breaks")
  nil)

(-> test-terminal-non-tty-fallback () null)
(defun test-terminal-non-tty-fallback ()
  "Test line-oriented fallback input and output on a non-TTY descriptor."
  (multiple-value-bind (read-descriptor write-descriptor)
      (sb-posix:pipe)
    (unwind-protect
         (let* ((output (make-string-output-stream))
                (terminal
                  (stream-terminal-create
                   :input-stream (make-string-input-stream
                                  (format nil "fallback input~%"))
                   :output-stream output
                   :input-file-descriptor read-descriptor
                   :columns 20))
                (ui (terminal-ui-create :terminal terminal)))
           (terminal-ui-start ui)
           (test-assert (not (terminal-interactive-p terminal))
                        "a pipe selects the non-TTY fallback")
           (multiple-value-bind (action submitted)
               (terminal-ui-process-event ui (terminal-ui-read-event ui))
             (test-assert (eq action :submit)
                          "fallback line input produces a submit action")
             (test-assert (string= submitted "fallback input")
                          "fallback line input preserves its text"))
           (terminal-ui-set-status ui "not printed")
           (terminal-ui-append-finalized ui 1 "fallback output")
           (terminal-ui-stop ui)
           (let ((captured (get-output-stream-string output)))
             (test-assert (search "fallback output" captured)
                          "fallback mode writes finalized transcript output")
             (test-assert (not (find +terminal-escape-character+ captured))
                          "fallback mode emits no terminal controls")))
      (sb-posix:close read-descriptor)
      (sb-posix:close write-descriptor)))
  nil)

(-> run-terminal-tests () boolean)
(defun run-terminal-tests ()
  "Run focused terminal seam tests and return true when every assertion succeeds."
  (test-terminal-primary-screen-controls)
  (test-terminal-untrusted-text)
  (test-terminal-finalized-scrollback)
  (test-terminal-line-editor)
  (test-terminal-input-decoding)
  (test-terminal-live-region-layout)
  (test-terminal-narrow-live-region)
  (test-terminal-stream-update)
  (test-terminal-command-completion)
  (test-terminal-modal-selection)
  (test-terminal-styling-primitives)
  (test-terminal-non-tty-fallback)
  t)
