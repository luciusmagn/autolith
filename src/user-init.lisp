(in-package #:autolith)

;;;; -- User Initialization --

(-> user-init-load (configuration) (option pathname))
(defun user-init-load (configuration)
  "Load CONFIGURATION's user initialization file and return its pathname.

The file is read in the AUTOLITH package after tracked and privately committed
definitions have loaded. It executes with the user's full privileges."
  (with-extension-registry-transaction
    (let ((pathname (configuration-user-init-path configuration))
          (context-registrations (context--registry-snapshot))
          (command-registrations (application-command--registry-snapshot))
          (mcp-registrations (mcp--registry-snapshot)))
      (handler-case
          (progn
            (context--remove-registration-source ':user)
            (application-command--remove-registration-source ':user)
            (mcp--remove-registration-source ':user)
            (when (uiop:file-exists-p pathname)
              (let ((*package* (find-package '#:autolith))
                    (*user-init-loading-p* t))
                (load pathname :verbose nil :print nil)
                pathname)))
        (serious-condition (cause)
          (context--registry-restore context-registrations)
          (application-command--registry-restore command-registrations)
          (mcp--registry-restore mcp-registrations)
          (error 'user-init-error
                 :message
                 (format nil "Could not load user initialization at ~A: ~A"
                         pathname cause)
                 :pathname pathname
                 :cause cause))))))
