(in-package #:frob)

;;;; -- Self-Modification Overlays --

;;; Durable self-modifications persist here as complete top-level definitions
;;; in one file per definition, loaded at startup after the tracked system.
;;; The tracked source repository is never patched by Frob itself.

(-> overlay-pathname (configuration string) pathname)
(defun overlay-pathname (configuration target)
  "Return the overlay file for the definition named by TARGET."
  (let ((slug (string-downcase
               (substitute-if-not
                #\-
                #'alphanumericp
                (string-trim "()" target)))))
    (merge-pathnames (make-pathname :name (bounded-string slug :limit 80)
                                    :type "lisp")
                     (configuration-overlay-root configuration))))

(-> overlay-read (configuration string) (option string))
(defun overlay-read (configuration target)
  "Return TARGET's current overlay source, or NIL when none exists."
  (let ((pathname (overlay-pathname configuration target)))
    (when (uiop:file-exists-p pathname)
      (handler-case
          (uiop:read-file-string pathname)
        (error ()
          nil)))))

(-> overlay-write (configuration string string) pathname)
(defun overlay-write (configuration target source)
  "Atomically write SOURCE as TARGET's overlay and return its pathname."
  (let* ((pathname (overlay-pathname configuration target))
         (temporary (merge-pathnames
                     (make-pathname :type "tmp")
                     pathname)))
    (ensure-directories-exist pathname)
    (with-open-file (stream temporary
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (format stream ";;;; Frob overlay for ~A~%" target)
      (format stream ";;;; Written ~D. Loaded at startup after the system.~2%"
              (get-universal-time))
      (write-string source stream)
      (fresh-line stream))
    (rename-file temporary pathname)
    pathname))

(-> overlay-load-all (configuration) list)
(defun overlay-load-all (configuration)
  "Load every overlay file for CONFIGURATION and return load failures.

Each failure is a (PATHNAME . CONDITION-REPORT) pair. A failing overlay is
skipped so the remaining overlays and the application still start."
  (let ((root (configuration-overlay-root configuration))
        (failures nil))
    (when (uiop:directory-exists-p root)
      (dolist (pathname (sort (uiop:directory-files root "*.lisp")
                              #'string<
                              :key #'namestring))
        (handler-case
            (let ((*package* (find-package '#:frob)))
              (load pathname))
          (error (condition)
            (push (cons pathname (format nil "~A" condition))
                  failures)))))
    (nreverse failures)))
