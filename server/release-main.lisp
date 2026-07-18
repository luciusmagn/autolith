(in-package #:autolith)

;;;; -- Release Service Entry --

(-> release-server-main () null)
(defun release-server-main ()
  "Run the release service mode selected by the command line."
  (let ((arguments (uiop:command-line-arguments)))
    (cond
      ((or (null arguments) (equal arguments '("serve")))
       (release-server-serve (release-server-configuration-create)))
      ((equal arguments '("build"))
       (release-builder-run (release-builder-configuration-create)))
      ((equal arguments '("build-once"))
       (release-builder-build-pending
        (release-builder-configuration-create)))
      (t
       (error 'configuration-error
              :message "Usage: server/run [serve|build|build-once]"))))
  nil)
