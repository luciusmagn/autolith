(in-package #:autolith)

;;;; -- User Initialization --

(defvar *user-init-loading-p* nil
  "True only while Autolith loads the user's executable configuration.")

(-> user-init-load (configuration) (option pathname))
(defun user-init-load (configuration)
  "Load CONFIGURATION's user initialization file and return its pathname.

The file is read in the AUTOLITH package after tracked and privately committed
definitions have loaded. It executes with the user's full privileges."
  (let ((pathname (configuration-user-init-path configuration)))
    (when (uiop:file-exists-p pathname)
      (handler-case
          (let ((*package* (find-package '#:autolith))
                (*user-init-loading-p* t))
            (load pathname :verbose nil :print nil)
            pathname)
        (serious-condition (cause)
          (error 'user-init-error
                 :message (format nil "Could not load user initialization at ~A: ~A"
                                  pathname cause)
                 :pathname pathname
                 :cause cause))))))
