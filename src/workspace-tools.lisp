(in-package #:frob)

;;;; -- Workspace Tool Classes --

(defclass workspace-tool (tool)
  ()
  (:documentation
   "A tool touching only workspace files and subprocesses, never the active image."))

(defclass fs-read-tool (workspace-tool)
  ()
  (:documentation "Read one workspace file with numbered lines."))

(defclass fs-list-tool (workspace-tool)
  ()
  (:documentation "List one workspace directory with entry kinds and sizes."))

(defclass shell-run-tool (workspace-tool)
  ()
  (:documentation "Run one bounded external command in the workspace."))


;;;; -- Workspace Constants --

(define-constant +fs-read-default-line-count+ 400
  :documentation "The file lines returned by fs.read when no window is given.")

(define-constant +shell-default-timeout-seconds+ 60
  :documentation "The seconds one shell.run command may take by default.")

(define-constant +shell-maximum-timeout-seconds+ 600
  :documentation "The largest timeout one shell.run command may request.")


;;;; -- Path Resolution --

(-> workspace-tool-path (tool-context (option string)) pathname)
(defun workspace-tool-path (context path)
  "Return PATH resolved against CONTEXT's working directory."
  (let ((working-directory (configuration-working-directory
                            (tool-context-configuration context))))
    (if (non-empty-string-p path)
        (merge-pathnames (pathname path) working-directory)
        working-directory)))

(-> workspace-tool-integer-argument
    (json-object string &key (:fallback (option integer)))
    (option integer))
(defun workspace-tool-integer-argument (arguments name &key fallback)
  "Return integer argument NAME from ARGUMENTS, or FALLBACK when absent."
  (let ((value (tool-argument arguments name)))
    (cond
      ((null value)
       fallback)
      ((integerp value)
       value)
      ((and (numberp value) (= value (round value)))
       (round value))
      (t
       (error 'tool-error
              :message (format nil "Tool argument ~S must be an integer." name)
              :tool-name name)))))


;;;; -- Tool Executions --

(defmethod tool-execute ((tool fs-read-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return a numbered window of the requested file."
  (let* ((path (workspace-tool-path
                context
                (tool-argument arguments "path" :required t)))
         (start-line (max 1 (or (workspace-tool-integer-argument
                                 arguments "start-line")
                                1)))
         (line-count (max 1 (or (workspace-tool-integer-argument
                                 arguments "line-count")
                                +fs-read-default-line-count+))))
    (cond
      ((uiop:directory-exists-p path)
       (tool-failure
        (format nil "~A is a directory; use fs.list instead." path)))
      ((not (probe-file path))
       (tool-failure (format nil "~A does not exist." path)))
      (t
       (let* ((split (uiop:split-string (uiop:read-file-string path)
                                        :separator '(#\Newline)))
              (lines (if (and (rest split)
                              (string= (first (last split)) ""))
                         (butlast split)
                         split))
              (total (length lines))
              (window-start (min (1- start-line) total))
              (window-end (min (+ window-start line-count) total)))
         (tool-success
          (format nil "~A lines ~D-~D of ~D~%~{~A~^~%~}"
                  path
                  (1+ window-start)
                  window-end
                  total
                  (loop for line in (subseq lines window-start window-end)
                        for number from (1+ window-start)
                        collect (format nil "~4D  ~A" number line)))))))))

(defmethod tool-execute ((tool fs-list-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return the requested directory's entries with kinds and byte sizes."
  (let ((path (workspace-tool-path context (tool-argument arguments "path"))))
    (if (not (uiop:directory-exists-p path))
        (tool-failure (format nil "~A is not a directory." path))
        (let ((directories (sort (mapcar (lambda (directory)
                                           (first (last (pathname-directory
                                                         directory))))
                                         (uiop:subdirectories path))
                                 #'string<))
              (files (sort (uiop:directory-files path)
                           #'string<
                           :key #'namestring)))
          (tool-success
           (format nil "~A~%~{~A~%~}~{~A~%~}"
                   path
                   (loop for name in directories
                         collect (format nil "d           ~A/" name))
                   (loop for file in files
                         collect (format nil "f ~9D  ~A"
                                         (handler-case
                                             (with-open-file (stream file
                                                              :element-type
                                                              '(unsigned-byte 8))
                                               (file-length stream))
                                           (error ()
                                             0))
                                         (file-namestring file)))))))))

(defmethod tool-execute ((tool shell-run-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Run one bounded external command and return its exit code and output."
  (let* ((command (tool-argument arguments "command" :required t))
         (directory (workspace-tool-path
                     context
                     (tool-argument arguments "directory")))
         (timeout (min +shell-maximum-timeout-seconds+
                       (max 1 (or (workspace-tool-integer-argument
                                   arguments "timeout-seconds")
                                  +shell-default-timeout-seconds+)))))
    (unless (non-empty-string-p command)
      (error 'tool-error
             :message "shell.run requires a non-empty command."
             :tool-name "shell.run"))
    (uiop:with-temporary-file (:pathname output-path :prefix "frob-shell")
      (let* ((process (uiop:launch-program
                       command
                       :output output-path
                       :error-output output-path
                       :if-output-exists :supersede
                       :directory directory))
             (deadline (+ (get-universal-time) timeout))
             (timed-out-p nil))
        (loop while (uiop:process-alive-p process)
              do (when (> (get-universal-time) deadline)
                   (setf timed-out-p t)
                   (uiop:terminate-process process :urgent t))
                 (sleep 0.05))
        (let ((code (uiop:wait-process process))
              (output (handler-case
                          (uiop:read-file-string output-path)
                        (error ()
                          ""))))
          (if timed-out-p
              (tool-failure
               (format nil "The command was stopped after ~D seconds.~%~A"
                       timeout
                       output))
              (tool-success
               (format nil "exit ~D~%~A" code output))))))))
