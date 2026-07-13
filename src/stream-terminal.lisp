(in-package #:autolith)

;;;; -- Terminal Methods --

(define-constant +linux-terminal-window-size-request+ #x5413
  :documentation "The Linux TIOCGWINSZ ioctl request on supported x86-64 systems.")

(-> terminal-file-descriptor-size
    (integer)
    (values (option integer) (option integer)))
(defun terminal-file-descriptor-size (file-descriptor)
  "Return positive terminal rows and columns for FILE-DESCRIPTOR, or NIL values."
  (handler-case
      (sb-alien:with-alien ((size (array sb-alien:unsigned-short 4)))
        (sb-posix:ioctl file-descriptor
                        +linux-terminal-window-size-request+
                        (sb-alien:addr (sb-alien:deref size 0)))
        (let ((rows (sb-alien:deref size 0))
              (columns (sb-alien:deref size 1)))
          (values (and (plusp rows) rows)
                  (and (plusp columns) columns))))
    (sb-posix:syscall-error ()
      (values nil nil))))

(defmethod terminal--write ((terminal stream-terminal) (text string))
  "Write trusted TEXT to TERMINAL's output stream."
  (write-string text (stream-terminal-output-stream terminal))
  nil)

(defmethod terminal-flush ((terminal stream-terminal))
  "Flush TERMINAL's output stream."
  (finish-output (stream-terminal-output-stream terminal))
  nil)

(defmethod terminal-input-ready-p ((terminal stream-terminal))
  "Return true when TERMINAL input can be consumed without blocking."
  (or (not (terminal-interactive-p terminal))
      (listen (stream-terminal-input-stream terminal))))

(-> terminal--terminal-mode-or-nil (stream-terminal) t)
(defun terminal--terminal-mode-or-nil (terminal)
  "Return TERMINAL's termios value, or NIL when its input is not a TTY."
  (handler-case
      (sb-posix:tcgetattr (stream-terminal-input-file-descriptor terminal))
    (sb-posix:syscall-error (condition)
      (if (= (sb-posix:syscall-errno condition) sb-posix:enotty)
          nil
          (error 'terminal-error
                 :message "Could not inspect terminal input mode."
                 :operation ':start
                 :cause condition)))))

(-> terminal--configure-input-mode (sb-posix:termios) sb-posix:termios)
(defun terminal--configure-input-mode (mode)
  "Configure MODE for noncanonical, no-echo, application-managed input."
  (setf (sb-posix:termios-lflag mode)
        (logandc2 (sb-posix:termios-lflag mode)
                  (logior sb-posix:icanon
                          sb-posix:echo
                          sb-posix:isig
                          sb-posix:iexten))
        (sb-posix:termios-iflag mode)
        (logandc2 (sb-posix:termios-iflag mode) sb-posix:ixon))
  (let ((control-characters (sb-posix:termios-cc mode)))
    (setf (aref control-characters sb-posix:vmin) 1
          (aref control-characters sb-posix:vtime) 0))
  mode)

(defmethod terminal-start ((terminal stream-terminal))
  "Start TERMINAL in noncanonical mode, or select its non-TTY fallback."
  (when (terminal-started-p terminal)
    (return-from terminal-start terminal))
  (let ((saved-mode (terminal--terminal-mode-or-nil terminal)))
    (if (null saved-mode)
        (setf (terminal-interactive-p terminal) nil
              (terminal-styled-p terminal) nil
              (terminal-started-p terminal) t)
        (handler-case
            (let ((active-mode
                    (terminal--configure-input-mode
                     (sb-posix:tcgetattr
                      (stream-terminal-input-file-descriptor terminal)))))
              (sb-posix:tcsetattr
               (stream-terminal-input-file-descriptor terminal)
               sb-posix:tcsanow
               active-mode)
              (setf (stream-terminal-saved-terminal-mode terminal) saved-mode
                    (terminal-interactive-p terminal) t
                    (terminal-styled-p terminal) (terminal-environment-styling-p)
                    (terminal-started-p terminal) t)
              (terminal--write terminal +terminal-bracketed-paste-enable+)
              (terminal-flush terminal))
          (error (condition)
            (ignore-errors
              (sb-posix:tcsetattr
               (stream-terminal-input-file-descriptor terminal)
               sb-posix:tcsanow
               saved-mode))
            (setf (stream-terminal-saved-terminal-mode terminal) nil
                  (terminal-interactive-p terminal) nil
                  (terminal-started-p terminal) nil)
            (error 'terminal-error
                   :message "Could not enter terminal input mode."
                   :operation ':start
                   :cause condition)))))
  terminal)

(defmethod terminal-stop ((terminal stream-terminal))
  "Stop TERMINAL and restore the exact termios value captured at startup."
  (unless (terminal-started-p terminal)
    (return-from terminal-stop terminal))
  (let ((failure nil)
        (saved-mode (stream-terminal-saved-terminal-mode terminal)))
    (when (terminal-interactive-p terminal)
      (handler-case
          (progn
            (terminal--write terminal +terminal-bracketed-paste-disable+)
            (terminal-flush terminal))
        (error (condition)
          (setf failure condition)))
      (when saved-mode
        (handler-case
            (sb-posix:tcsetattr
             (stream-terminal-input-file-descriptor terminal)
             sb-posix:tcsanow
             saved-mode)
          (error (condition)
            (unless failure
              (setf failure condition))))))
    (setf (stream-terminal-saved-terminal-mode terminal) nil
          (terminal-interactive-p terminal) nil
          (terminal-styled-p terminal) nil
          (terminal-started-p terminal) nil)
    (when failure
      (error 'terminal-error
             :message "Could not completely restore terminal state."
             :operation ':stop
             :cause failure)))
  terminal)

(defmethod terminal-read-event ((terminal stream-terminal))
  "Read one key, escape sequence, paste, or fallback line from TERMINAL."
  (if (terminal-interactive-p terminal)
      (let ((event (read-event :stream (stream-terminal-input-stream terminal))))
        (if (eq event :stream-end)
            :end-of-input
            event))
      (let ((line (read-line (stream-terminal-input-stream terminal) nil nil)))
        (if line
            (list :line line)
            :end-of-input))))

;; Generic functions require broad FTYPEs so downstream terminal adapters can
;; add methods without SBCL replacing a class-restricted proclamation.
(-> terminal-start (t) *)
(-> terminal-stop (t) *)
(-> terminal-read-event (t) *)
(-> terminal--write (t t) *)
(-> terminal-flush (t) *)


;;;; -- Public Construction --

(-> stream-terminal-create
    (&key
     (:input-stream stream)
     (:output-stream stream)
     (:input-file-descriptor integer)
     (:rows integer)
     (:columns integer))
    stream-terminal)
(defun stream-terminal-create
    (&key
       (input-stream *standard-input*)
       (output-stream *standard-output*)
       (input-file-descriptor 0)
       (rows +terminal-default-rows+)
       (columns +terminal-default-columns+))
  "Create a stream terminal using INPUT-STREAM, OUTPUT-STREAM, and a POSIX descriptor."
  (make-instance 'stream-terminal
                 :input-stream input-stream
                 :output-stream output-stream
                 :input-file-descriptor input-file-descriptor
                 :rows (if (plusp rows)
                           rows
                           +terminal-default-rows+)
                 :columns (if (plusp columns)
                              columns
                              +terminal-default-columns+)))
