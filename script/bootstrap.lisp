(require :asdf)

(let* ((script-path (truename *load-truename*))
       (script-directory (uiop:pathname-directory-pathname script-path))
       (source-root (uiop:pathname-parent-directory-pathname script-directory))
       (version-pathname (merge-pathnames "sbcl.version" source-root))
       (sbcl-command (or (uiop:getenv "AUTOLITH_SBCL") "sbcl"))
       (quicklisp-setup (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
  (let ((expected-version
          (string-trim '(#\Space #\Tab #\Newline #\Return)
                       (uiop:read-file-string version-pathname))))
    (unless (string= expected-version (lisp-implementation-version))
      (error "Autolith pins SBCL ~A, but this process is ~A. Set AUTOLITH_SBCL to the pinned executable."
             expected-version
             (lisp-implementation-version))))
  (unless (probe-file quicklisp-setup)
    (error "Autolith bootstrap needs Quicklisp at ~A" quicklisp-setup))
  (format t "~&Loading bootstrap dependencies.~%")
  (finish-output)
  (load quicklisp-setup)
  (uiop:symbol-call '#:ql '#:quickload :cffi :silent t)
  (let ((profile-library-directory
          (merge-pathnames ".guix-profile/lib/" (user-homedir-pathname)))
        (library-directories
          (find-symbol "*FOREIGN-LIBRARY-DIRECTORIES*" "CFFI")))
    (when (probe-file profile-library-directory)
      (pushnew profile-library-directory
               (symbol-value library-directories)
               :test #'equal)))
  (uiop:symbol-call '#:ql '#:quickload :qlot :silent t)
  (uiop:chdir source-root)
  (format t "~&Materializing locked Lisp dependencies.~%")
  (finish-output)
  (uiop:symbol-call '#:qlot '#:install)
  (load (merge-pathnames ".qlot/setup.lisp" source-root))
  (format t "~&Building the private command sandbox helper.~%")
  (finish-output)
  (load (merge-pathnames "script/build-sandbox.lisp" source-root))
  (format t "~&Building the private fff search library.~%")
  (finish-output)
  (load (merge-pathnames "script/build-fff.lisp" source-root))
  (format t "~&Building the pristine recovery image.~%")
  (finish-output)
  (uiop:run-program (list sbcl-command
                          "--script"
                          (namestring (merge-pathnames "script/build-recovery.lisp"
                                                       source-root)))
                    :input :interactive
                    :output :interactive
                    :error-output :interactive)
  (format t "~&Building the fast startup image.~%")
  (finish-output)
  (uiop:run-program (list sbcl-command
                          "--script"
                          (namestring (merge-pathnames "script/build-active.lisp"
                                                       source-root)))
                    :input :interactive
                    :output :interactive
                    :error-output :interactive)
  (format t "~&Autolith dependencies, private search library, recovery image, and fast startup image are installed.~%"))
