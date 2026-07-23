(in-package #:autolith)

;;;; -- Epoch Conversion --

(defparameter *unix-epoch-universal-time* 2208988800
  "The Common Lisp universal time corresponding to the Unix epoch.")

(-> unix-time->universal-time (integer) integer)
(defun unix-time->universal-time (unix-time)
  "Convert integer UNIX-TIME seconds to Common Lisp universal time."
  (+ unix-time *unix-epoch-universal-time*))

(-> universal-time->unix-time (integer) integer)
(defun universal-time->unix-time (universal-time)
  "Convert Common Lisp UNIVERSAL-TIME to integer Unix-time seconds."
  (- universal-time *unix-epoch-universal-time*))
