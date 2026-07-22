(in-package #:autolith)

;;;; -- Workspace Identity --

(define-constant +workspace-project-depth-limit+ 64
  :documentation "The most ancestor directories inspected for a project root.")

(-> workspace-project-root (pathname) pathname)
(defun workspace-project-root (working-directory)
  "Return the nearest ancestor holding a .git marker, or WORKING-DIRECTORY.

The walk never continues above the nearest project root. A workspace without a
Git marker is its own project identity."
  (labels ((marker-p (directory)
             "Return true when DIRECTORY contains a .git entry."
             (and (or (uiop:directory-exists-p
                       (merge-pathnames ".git/" directory))
                      (uiop:file-exists-p (merge-pathnames ".git" directory)))
                  t)))
    (loop repeat +workspace-project-depth-limit+
          for directory = working-directory
            then (uiop:pathname-parent-directory-pathname directory)
          for parent = (uiop:pathname-parent-directory-pathname directory)
          when (marker-p directory)
            return directory
          when (equal directory parent)
            return working-directory
          finally (return working-directory))))

(-> workspace-autolith-notes-path (pathname) pathname)
(defun workspace-autolith-notes-path (working-directory)
  "Return the root AUTOLITH.org pathname for WORKING-DIRECTORY."
  (merge-pathnames "AUTOLITH.org"
                   (workspace-project-root working-directory)))
