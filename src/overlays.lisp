(in-package #:autolith)

;;;; -- Self-Modification Overlays --

;;; Autolith versions before private image commits persisted complete definitions
;;; here. Normal startup loads these files only when no private commit is selected;
;;; the first private commit imports them into its complete replay snapshot.

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

(-> overlay--constant-target-p (string) boolean)
(defun overlay--constant-target-p (target)
  "Return true when TARGET names a constant-defining operation."
  (and (or (search "define-constant" target)
           (search "defconstant" target))
       t))

(-> overlay--write-constant-form (stream string) null)
(defun overlay--write-constant-form (stream source)
  "Write SOURCE wrapped so its constant redefinition continues deliberately.

Constant definitions signal their redefinition check during the compile-time
processing of a loaded top-level form, where no outer handler can reach the
restart. Evaluating the definition as data at load time keeps the restart
inside this wrapper's dynamic extent."
  (format stream
          "(handler-bind ((error~%~
           ~16T(lambda (overlay-condition)~%~
           ~18T(let ((overlay-restart~%~
           ~24T(find-restart 'continue overlay-condition)))~%~
           ~20T(when overlay-restart~%~
           ~22T(invoke-restart overlay-restart))))))~%~
           ~2T(eval (quote~%~A~%~2T)))~%"
          source)
  nil)

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
      (format stream ";;;; Autolith overlay for ~A~%" target)
      (format stream ";;;; Written ~D. Loaded at startup after the system.~2%"
              (get-universal-time))
      (if (overlay--constant-target-p target)
          (overlay--write-constant-form stream source)
          (progn
            (write-string source stream)
            (fresh-line stream))))
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
            (let ((*package* (find-package '#:autolith)))
              (load pathname))
          (error (condition)
            (push (cons pathname (format nil "~A" condition))
                  failures)))))
    (nreverse failures)))
