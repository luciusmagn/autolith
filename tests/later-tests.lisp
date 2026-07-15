(in-package #:autolith)

;;;; -- Deferred Input Tests --

(-> test-later-persistence () null)
(defun test-later-persistence ()
  "Test deferred inputs persist in order and cancel atomically."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((state (later-load configuration))
                (directory (configuration-working-directory configuration))
                (later-entry
                  (later-schedule :configuration configuration
                                  :state state
                                  :input "second"
                                  :directory directory
                                  :due-at 200
                                  :window "weekly"
                                  :created-at 20))
                (earlier-entry
                  (later-schedule :configuration configuration
                                  :state state
                                  :input "first"
                                  :directory directory
                                  :due-at 100
                                  :window "5h"
                                  :created-at 10))
                (loaded (later-load configuration)))
           (test-assert
            (equal (mapcar #'later-entry-input (later-state-entries loaded))
                   '("first" "second"))
            "deferred inputs reload in due-time order")
           (test-assert
            (and (later-cancel configuration loaded
                               (later-entry-identifier earlier-entry))
                 (not (later-cancel configuration loaded "missing")))
            "deferred cancellation reports exact identifiers")
           (test-assert
            (equal (mapcar #'later-entry-identifier
                           (later-state-entries (later-load configuration)))
                   (list (later-entry-identifier later-entry)))
            "deferred cancellation persists without disturbing other entries")
           (test-assert (= (logand (sb-posix:stat-mode
                                    (sb-posix:stat
                                     (namestring
                                      (configuration-later-path configuration))))
                                   #o777)
                           #o600)
                        "deferred state is private to the current user"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-later-malformed-state () null)
(defun test-later-malformed-state ()
  "Test malformed deferred state cannot evaluate reader forms."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (configuration-later-path configuration))
         (*later-reader-evaluated-p* nil))
    (declare (special *later-reader-evaluated-p*))
    (unwind-protect
         (progn
           (ensure-directories-exist pathname)
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-string
              "#.(setf autolith::*later-reader-evaluated-p* t)"
              stream))
           (handler-bind ((later-load-warning #'muffle-warning))
             (test-assert (null (later-state-entries
                                 (later-load configuration)))
                          "malformed deferred state loads as an empty queue"))
           (test-assert (null *later-reader-evaluated-p*)
                        "deferred state disables reader evaluation"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-later-reset-selection () null)
(defun test-later-reset-selection ()
  "Test primary, secondary, and estimated reset selection."
  (multiple-value-bind (due-at window)
      (later-reset-deadline
       '(:primary (:used-percent 100 :window-minutes 300 :resets-at 2000)
         :secondary (:used-percent 50 :window-minutes 10080 :resets-at 9000))
       :now 1000)
    (test-assert (and (= due-at 2005) (string= window "5h"))
                 "a usable primary reset schedules the five-hour window"))
  (multiple-value-bind (due-at window)
      (later-reset-deadline
       '(:primary (:used-percent 100 :window-minutes 300 :resets-at 2000)
         :secondary (:used-percent 100 :window-minutes 10080 :resets-at 9000))
       :now 1000)
    (test-assert (and (= due-at 9005) (string= window "weekly"))
                 "an exhausted secondary window overrides the primary reset"))
  (multiple-value-bind (due-at window)
      (later-reset-deadline
       '(:secondary (:used-percent 50 :window-minutes 10080 :resets-at 50000))
       :now 1000)
    (test-assert (and (= due-at 19000) (string= window "estimated 5h"))
                 "a missing primary window uses a bounded five-hour estimate"))
  (multiple-value-bind (due-at window)
      (later-reset-deadline
       '(:secondary (:used-percent 100 :window-minutes 10080))
       :now 1000)
    (test-assert (and (null due-at) (null window))
                 "an exhausted window without a reset refuses to guess"))
  nil)
