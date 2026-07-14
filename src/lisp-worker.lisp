(in-package #:autolith)

;;;; -- Worker Process --

(defvar *worker-image-identifier* +pristine-lisp-image-identifier+
  "The pristine or saved image identity reported by this worker process.")

(defclass lisp-worker ()
  ((configuration
    :initarg :configuration
    :accessor lisp-worker-configuration
    :type configuration
    :documentation "The source and current workspace paths used by the worker.")
   (name
    :initarg :name
    :reader lisp-worker-name
    :type non-empty-string
    :documentation "The stable REPL name used to route Lisp tool calls.")
   (image-identifier
    :initarg :image-identifier
    :reader lisp-worker-image-identifier
    :type non-empty-string
    :documentation "The pristine or saved worker image used at process start.")
   (core-pathname
    :initarg :core-pathname
    :initform nil
    :reader lisp-worker-core-pathname
    :type (option pathname)
    :documentation "The compatible saved core, or NIL for pristine SBCL.")
   (process
    :initform nil
    :accessor lisp-worker-process
    :type t
    :documentation "The active UIOP process info, or NIL.")
   (input
    :initform nil
    :accessor lisp-worker-input
    :type (option stream)
    :documentation "The worker's request stream.")
   (output
    :initform nil
    :accessor lisp-worker-output
    :type (option stream)
    :documentation "The worker's response stream.")
   (next-request-id
    :initform 1
    :accessor lisp-worker-next-request-id
    :type integer
    :documentation "The next protocol request identifier.")
   (lock
    :initform (make-lock "Autolith Lisp worker")
    :reader lisp-worker-lock
    :documentation "The lock serializing worker protocol requests."))
  (:documentation "One named persistent, heap-isolated SBCL REPL process."))

(-> lisp-repl-name-p (t) boolean)
(defun lisp-repl-name-p (value)
  "Return true when VALUE is safe as one named worker REPL."
  (and (non-empty-string-p value)
       (<= (length value) 80)
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "-_")))
              value)
       t))

(-> lisp-repl--validate-name (string) string)
(defun lisp-repl--validate-name (name)
  "Return valid REPL NAME or signal WORKER-ERROR."
  (unless (lisp-repl-name-p name)
    (error 'worker-error
           :message
           (format nil
                   "Invalid Lisp REPL name ~S. Use 1 to 80 letters, digits, hyphens, or underscores."
                   name)
           :tool-name "lisp.repls"))
  name)

(-> lisp-worker-create
    (configuration &key (:name string) (:image-identifier string))
    lisp-worker)
(defun lisp-worker-create
    (configuration &key (name "default")
                        (image-identifier +pristine-lisp-image-identifier+))
  "Create a stopped named worker based on IMAGE-IDENTIFIER."
  (lisp-repl--validate-name name)
  (let ((image
          (unless (string= image-identifier
                           +pristine-lisp-image-identifier+)
            (lisp-image-load configuration image-identifier))))
    (when (and image (not (lisp-image-compatible-p image)))
      (error 'lisp-image-error
             :message (format nil "Lisp image ~A is incompatible with this runtime."
                              image-identifier)
             :tool-name "lisp.start"
             :pathname (lisp-image-manifest-pathname image)
             :stage ':compatibility))
    (lisp-worker--create-record
     configuration
     :name name
     :image-identifier image-identifier
     :core-pathname (and image (lisp-image-core-pathname image)))))

(-> lisp-worker--create-record
    (configuration
     &key (:name string)
          (:image-identifier string)
          (:core-pathname (option pathname)))
    lisp-worker)
(defun lisp-worker--create-record
    (configuration &key name image-identifier core-pathname)
  "Create a stopped worker record with an already resolved CORE-PATHNAME."
  (make-instance 'lisp-worker
                 :configuration configuration
                 :name name
                 :image-identifier image-identifier
                 :core-pathname core-pathname))

(-> lisp-worker-running-p (lisp-worker) boolean)
(defun lisp-worker-running-p (worker)
  "Return true when WORKER has a live subprocess."
  (let ((process (lisp-worker-process worker)))
    (and process (uiop:process-alive-p process) t)))

(-> lisp-worker-sbcl-command () string)
(defun lisp-worker-sbcl-command ()
  "Return the configured SBCL executable used by disposable workers."
  (let ((configured-command (uiop:getenv "AUTOLITH_SBCL")))
    (if (non-empty-string-p configured-command)
        configured-command
        "sbcl")))

(-> lisp-worker-start (lisp-worker) lisp-worker)
(defun lisp-worker-start (worker)
  "Start WORKER when necessary and verify its protocol handshake."
  (unless (lisp-worker-running-p worker)
    (let* ((configuration (lisp-worker-configuration worker))
           (worker-launcher (merge-pathnames
                             "bin/autolith-active"
                             (configuration-source-root configuration)))
           (sbcl-command (lisp-worker-sbcl-command))
           (command
             (if (lisp-worker-core-pathname worker)
                 (list sbcl-command
                       "--noinform"
                       "--core"
                       (namestring (lisp-worker-core-pathname worker))
                       "--end-runtime-options")
                 (list sbcl-command
                       "--script"
                       (namestring worker-launcher)
                       "--worker")))
           (process
             (uiop:launch-program
              command
              :directory (configuration-working-directory configuration)
              :input :stream
              :output :stream
              :error-output *error-output*
              :wait nil)))
      (setf (lisp-worker-process worker) process
            (lisp-worker-input worker) (uiop:process-info-input process)
            (lisp-worker-output worker) (uiop:process-info-output process))
      (handler-case
          (loop for line = (read-line (lisp-worker-output worker) nil nil)
                do (unless line
                     (error 'worker-error
                            :message "The Lisp worker exited before its handshake."
                            :tool-name "lisp.worker"))
                   (let* ((*read-eval* nil)
                          (form (handler-case
                                    (read-from-string line)
                                  (error ()
                                    nil)))
                          (properties (and (listp form) (rest form))))
                     (when (eq (first form) :autolith-worker)
                       (unless (and (= (or (getf properties :version) 0) 2)
                                    (string=
                                     (or (getf properties :image) "")
                                     (lisp-worker-image-identifier worker)))
                         (error 'worker-error
                                :message
                                "The Lisp worker reported the wrong protocol or image identity."
                                :tool-name "lisp.worker"))
                       (return))))
        (error (condition)
          (lisp-worker-stop worker)
          (error condition)))))
  worker)

(-> lisp-worker-stop (lisp-worker) null)
(defun lisp-worker-stop (worker)
  "Terminate WORKER and discard all process streams and heap state."
  (let ((process (lisp-worker-process worker)))
    (when process
      (when (uiop:process-alive-p process)
        (ignore-errors (uiop:terminate-process process :urgent t)))
      (ignore-errors (uiop:wait-process process))))
  (dolist (stream (list (lisp-worker-input worker)
                        (lisp-worker-output worker)))
    (when (and stream (open-stream-p stream))
      (ignore-errors (close stream))))
  (setf (lisp-worker-process worker) nil
        (lisp-worker-input worker) nil
        (lisp-worker-output worker) nil
        (lisp-worker-next-request-id worker) 1)
  nil)

(-> lisp-worker-reset (lisp-worker) lisp-worker)
(defun lisp-worker-reset (worker)
  "Discard WORKER's process and restart it from the same base image."
  (lisp-worker-stop worker)
  (lisp-worker-start worker))

(-> lisp-worker--working-directory-form (pathname) string)
(defun lisp-worker--working-directory-form (directory)
  "Return a readable worker form that changes to DIRECTORY."
  (format nil
          "(progn (uiop:chdir ~S) ~
                  (setf *default-pathname-defaults* (uiop:getcwd)) ~
                  (namestring (uiop:getcwd)))"
          (namestring directory)))

(-> lisp-worker-change-working-directory
    (lisp-worker configuration)
    lisp-worker)
(defun lisp-worker-change-working-directory (worker configuration)
  "Move WORKER to CONFIGURATION's workspace without discarding its heap."
  (when (lisp-worker-running-p worker)
    (let* ((directory (configuration-working-directory configuration))
           (response
             (lisp-worker-request
              worker
              :eval
              (list :form (lisp-worker--working-directory-form directory))))
           (properties (rest response)))
      (unless (eq (getf properties :status) :ok)
        (error 'worker-error
               :message
               (format nil "Lisp REPL ~A could not change to ~A: ~A"
                       (lisp-worker-name worker)
                       directory
                       (or (getf properties :message) "unknown worker failure"))
               :tool-name "lisp.cwd"))))
  (setf (lisp-worker-configuration worker) configuration)
  worker)


;;;; -- Named Worker Pool --

(defclass lisp-worker-pool ()
  ((configuration
    :initarg :configuration
    :accessor lisp-worker-pool-configuration
    :type configuration
    :documentation "The paths and current workspace shared by managed REPLs.")
   (workers
    :initform (make-hash-table :test #'equal)
    :reader lisp-worker-pool-workers
    :type hash-table
    :documentation "Named REPLs mapped to their isolated worker managers.")
   (lock
    :initform (make-lock "Autolith Lisp worker pool")
    :reader lisp-worker-pool-lock
    :documentation "The lock serializing REPL creation, reset, and removal."))
  (:documentation "A manager for independent named persistent SBCL REPLs."))

(-> lisp-worker-pool-create (configuration) lisp-worker-pool)
(defun lisp-worker-pool-create (configuration)
  "Create an empty named Lisp worker pool for CONFIGURATION."
  (make-instance 'lisp-worker-pool :configuration configuration))

(-> lisp-worker-pool-start
    (lisp-worker-pool string (option string))
    lisp-worker)
(defun lisp-worker-pool-start (pool name image-identifier)
  "Start or return NAME, enforcing IMAGE-IDENTIFIER when it is supplied."
  (lisp-repl--validate-name name)
  (with-lock-held ((lisp-worker-pool-lock pool))
    (let ((existing (gethash name (lisp-worker-pool-workers pool))))
      (when existing
        (unless (or (null image-identifier)
                    (string= image-identifier
                             (lisp-worker-image-identifier existing)))
          (error 'worker-error
                 :message
                 (format nil
                         "Lisp REPL ~A already uses image ~A; reset it explicitly to switch to ~A."
                         name
                         (lisp-worker-image-identifier existing)
                         image-identifier)
                 :tool-name "lisp.start"))
        (return-from lisp-worker-pool-start (lisp-worker-start existing)))
      (let* ((image-identifier
               (or image-identifier +pristine-lisp-image-identifier+))
             (worker
              (lisp-worker-create
               (lisp-worker-pool-configuration pool)
               :name name
               :image-identifier image-identifier)))
        (setf (gethash name (lisp-worker-pool-workers pool)) worker)
        (handler-case
            (lisp-worker-start worker)
          (error (condition)
            (remhash name (lisp-worker-pool-workers pool))
            (lisp-worker-stop worker)
            (error condition)))))))

(-> lisp-worker-pool-worker (lisp-worker-pool string) lisp-worker)
(defun lisp-worker-pool-worker (pool name)
  "Return NAME, starting it from pristine SBCL when it does not exist."
  (lisp-worker-pool-start pool name nil))

(-> lisp-worker-pool-stop (lisp-worker-pool string &key (:if-missing keyword)) null)
(defun lisp-worker-pool-stop (pool name &key (if-missing :error))
  "Stop and forget named REPL NAME."
  (lisp-repl--validate-name name)
  (with-lock-held ((lisp-worker-pool-lock pool))
    (let ((worker (gethash name (lisp-worker-pool-workers pool))))
      (cond
        (worker
         (lisp-worker-stop worker)
         (remhash name (lisp-worker-pool-workers pool)))
        ((eq if-missing :error)
         (error 'worker-error
                :message (format nil "No Lisp REPL named ~A exists." name)
                :tool-name "lisp.stop")))))
  nil)

(-> lisp-worker-pool-reset (lisp-worker-pool string string) lisp-worker)
(defun lisp-worker-pool-reset (pool name image-identifier)
  "Replace named REPL NAME with a fresh process from IMAGE-IDENTIFIER."
  (lisp-worker-pool-stop pool name :if-missing :ignore)
  (lisp-worker-pool-start pool name image-identifier))

(-> lisp-worker-pool-stop-all (lisp-worker-pool) null)
(defun lisp-worker-pool-stop-all (pool)
  "Stop and forget every REPL managed by POOL."
  (with-lock-held ((lisp-worker-pool-lock pool))
    (maphash (lambda (name worker)
               (declare (ignore name))
               (lisp-worker-stop worker))
             (lisp-worker-pool-workers pool))
    (clrhash (lisp-worker-pool-workers pool)))
  nil)

(-> lisp-worker-pool-change-working-directory
    (lisp-worker-pool configuration)
    lisp-worker-pool)
(defun lisp-worker-pool-change-working-directory (pool configuration)
  "Move every REPL in POOL to CONFIGURATION while preserving successful heaps."
  (with-lock-held ((lisp-worker-pool-lock pool))
    (let ((previous (lisp-worker-pool-configuration pool))
          (changed nil))
      (handler-case
          (progn
            (maphash
             (lambda (name worker)
               (declare (ignore name))
               (lisp-worker-change-working-directory worker configuration)
               (push worker changed))
             (lisp-worker-pool-workers pool))
            (setf (lisp-worker-pool-configuration pool) configuration)
            pool)
        (error (condition)
          (let ((rollback-failures nil))
            (dolist (worker changed)
              (handler-case
                  (lisp-worker-change-working-directory worker previous)
                (error (rollback-condition)
                  (lisp-worker-stop worker)
                  (setf (lisp-worker-configuration worker) previous)
                  (push (cons worker rollback-condition) rollback-failures))))
            (when rollback-failures
              (error 'worker-error
                     :message
                     (format nil
                             "Changing Lisp REPL workspaces failed (~A), and rollback stopped: ~{~A~^; ~}."
                             condition
                             (loop for (worker . rollback-condition)
                                     in (nreverse rollback-failures)
                                   collect
                                   (format nil "~A: ~A"
                                           (lisp-worker-name worker)
                                           rollback-condition)))
                     :tool-name "lisp.cwd"))
            (error condition)))))))

(-> lisp-worker-manager-change-working-directory (t configuration) t)
(defun lisp-worker-manager-change-working-directory (manager configuration)
  "Move MANAGER's current and future REPLs to CONFIGURATION's workspace."
  (typecase manager
    (null
     nil)
    (lisp-worker
     (lisp-worker-change-working-directory manager configuration))
    (lisp-worker-pool
     (lisp-worker-pool-change-working-directory manager configuration))
    (otherwise
     (error 'worker-error
            :message "No Lisp worker manager can change working directory."
            :tool-name "lisp.cwd"))))

(-> lisp-worker-manager-stop (t) null)
(defun lisp-worker-manager-stop (manager)
  "Stop every live worker represented by MANAGER."
  (typecase manager
    (lisp-worker
     (lisp-worker-stop manager))
    (lisp-worker-pool
     (lisp-worker-pool-stop-all manager)))
  nil)

(-> lisp-worker-pool-render (lisp-worker-pool) string)
(defun lisp-worker-pool-render (pool)
  "Return a concise model-visible list of named REPLs and their images."
  (let ((workers nil))
    (with-lock-held ((lisp-worker-pool-lock pool))
      (maphash (lambda (name worker)
                 (declare (ignore name))
                 (push worker workers))
               (lisp-worker-pool-workers pool)))
    (if workers
        (with-output-to-string (stream)
          (dolist (worker (sort workers #'string< :key #'lisp-worker-name))
            (format stream "~A  ~A  image ~A~%"
                    (lisp-worker-name worker)
                    (if (lisp-worker-running-p worker) "running" "stopped")
                    (lisp-worker-image-identifier worker))))
        "No named Lisp REPLs exist.")))


;;;; -- Worker Image Snapshots --

(-> lisp-worker--probe-core (configuration string pathname) null)
(defun lisp-worker--probe-core (configuration identifier core-pathname)
  "Boot unpublished CORE-PATHNAME and verify its protocol and image identity."
  (let ((probe
          (lisp-worker--create-record
           configuration
           :name "image-probe"
           :image-identifier identifier
           :core-pathname core-pathname)))
    (unwind-protect
         (let ((response
                 (lisp-worker-request probe :eval '(:form "(+ 20 22)"))))
           (unless (and (eq (getf (rest response) :status) :ok)
                        (equal (getf (rest response) :values) '("42")))
             (error 'lisp-image-error
                    :message "The unpublished Lisp image failed its protocol probe."
                    :tool-name "lisp.save-image"
                    :pathname core-pathname
                    :stage ':probe)))
      (lisp-worker-stop probe)))
  nil)

(-> lisp-worker-save-image
    (configuration lisp-worker string string)
    lisp-image)
(defun lisp-worker-save-image (configuration worker identifier note)
  "Save WORKER as immutable IDENTIFIER with durable NOTE and return its image."
  (lisp-image--validate-identifier identifier)
  (unless (and (non-empty-string-p note) (<= (length note) 4000))
    (error 'lisp-image-error
           :message "A saved Lisp image needs a non-empty note of at most 4000 characters."
           :tool-name "lisp.save-image"
           :pathname nil
           :stage ':manifest))
  (let* ((directory (lisp-image--directory configuration identifier))
         (staging (lisp-image-staging-directory configuration identifier))
         (core (merge-pathnames "worker.core" staging)))
    (when (probe-file directory)
      (error 'lisp-image-error
             :message (format nil "Lisp image ~A already exists." identifier)
             :tool-name "lisp.save-image"
             :pathname directory
             :stage ':publish))
    (ensure-directories-exist core)
    (unwind-protect
         (let ((response
                 (lisp-worker-request
                  worker
                  :save-image
                  (list :pathname (namestring core)
                        :identifier identifier))))
           (unless (eq (getf (rest response) :status) :ok)
             (error 'lisp-image-error
                    :message
                    (format nil "The worker could not save image ~A: ~A"
                            identifier
                            (or (getf (rest response) :message)
                                "unknown worker failure"))
                    :tool-name "lisp.save-image"
                    :pathname core
                    :stage ':save))
           (lisp-worker--probe-core configuration identifier core)
           (lisp-image-publish-saved-core
            configuration
            :identifier identifier
            :parent-identifier (lisp-worker-image-identifier worker)
            :note note
            :staging-directory staging))
      (when (uiop:directory-exists-p staging)
        (uiop:delete-directory-tree staging
                                    :validate t
                                    :if-does-not-exist :ignore)))))

(-> lisp-worker-request (lisp-worker keyword list) list)
(defun lisp-worker-request (worker operation arguments)
  "Send OPERATION and portable ARGUMENTS to WORKER and return its response."
  (with-lock-held ((lisp-worker-lock worker))
    (lisp-worker-start worker)
    (let* ((request-id (lisp-worker-next-request-id worker))
           (request (list :request
                          :id request-id
                          :operation operation
                          :arguments arguments)))
      (incf (lisp-worker-next-request-id worker))
      (handler-case
          (progn
            (let ((*print-readably* t)
                  (*print-circle* t))
              (prin1 request (lisp-worker-input worker))
              (terpri (lisp-worker-input worker))
              (finish-output (lisp-worker-input worker)))
            (let ((*read-eval* nil)
                  (response (read (lisp-worker-output worker) t nil)))
              (unless (and (listp response)
                           (eq (first response) :response)
                           (= (or (getf (rest response) :id) -1) request-id))
                (error 'worker-error
                       :message "The Lisp worker returned an invalid response."
                       :tool-name (format nil "lisp.~(~A~)" operation)))
              response))
        (error (condition)
          (lisp-worker-stop worker)
          (if (typep condition 'worker-error)
              (error condition)
              (error 'worker-error
                     :message (format nil "The Lisp worker protocol failed: ~A" condition)
                     :tool-name (format nil "lisp.~(~A~)" operation))))))))

(-> worker-response-tool-result (list) tool-result)
(defun worker-response-tool-result (response)
  "Convert a portable worker RESPONSE into a bounded tool result."
  (let* ((properties (rest response))
         (status (getf properties :status))
         (output (getf properties :output))
         (result-values (getf properties :values))
         (message (getf properties :message))
         (backtrace (getf properties :backtrace)))
    (if (eq status :ok)
        (tool-success
         (with-output-to-string (stream)
           (when (non-empty-string-p output)
             (format stream "Output:~%~A~%" output))
           (format stream "Values:~%~{~A~%~}" result-values)))
        (tool-failure
         (with-output-to-string (stream)
           (format stream "~A" (or message "Worker operation failed."))
           (when (non-empty-string-p backtrace)
             (format stream "~%~%Backtrace:~%~A" backtrace)))))))


;;;; -- Lisp Tool Methods --

(-> lisp-tool-repl-name (hash-table) string)
(defun lisp-tool-repl-name (arguments)
  "Return the validated REPL selected by tool ARGUMENTS."
  (lisp-repl--validate-name
   (or (tool-argument arguments "repl") "default")))

(-> lisp-tool-worker (tool-context hash-table) lisp-worker)
(defun lisp-tool-worker (context arguments)
  "Return the named worker selected by ARGUMENTS inside CONTEXT."
  (let ((manager (tool-context-worker context))
        (name (lisp-tool-repl-name arguments)))
    (typecase manager
      (lisp-worker-pool
       (lisp-worker-pool-worker manager name))
      (lisp-worker
       (unless (string= name (lisp-worker-name manager))
         (error 'worker-error
                :message "This legacy context provides only its default Lisp REPL."
                :tool-name "lisp.worker"))
       manager)
      (otherwise
       (error 'worker-error
              :message "No Lisp worker manager is available."
              :tool-name "lisp.worker")))))

(defmethod tool-execute ((tool lisp-eval-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Evaluate the required form through CONTEXT's isolated worker."
  (declare (ignore tool))
  (worker-response-tool-result
   (lisp-worker-request
    (lisp-tool-worker context arguments)
    :eval
    (list :form (tool-argument arguments "form" :required t)))))

(defmethod tool-execute ((tool lisp-compile-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Compile and execute the required form through CONTEXT's isolated worker."
  (declare (ignore tool))
  (worker-response-tool-result
   (lisp-worker-request
    (lisp-tool-worker context arguments)
    :compile
    (list :form (tool-argument arguments "form" :required t)))))

(defmethod tool-execute ((tool lisp-load-system-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Load the required system through CONTEXT's isolated worker."
  (declare (ignore tool))
  (worker-response-tool-result
   (lisp-worker-request
    (lisp-tool-worker context arguments)
    :load-system
    (list :system (tool-argument arguments "system" :required t)))))

(defmethod tool-execute ((tool lisp-describe-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Describe the required designator through CONTEXT's isolated worker."
  (declare (ignore tool))
  (worker-response-tool-result
   (lisp-worker-request
    (lisp-tool-worker context arguments)
    :describe
    (list :designator (tool-argument arguments "designator" :required t)))))

(defmethod tool-execute ((tool lisp-source-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Read exact matching source through CONTEXT's selected worker."
  (declare (ignore tool))
  (worker-response-tool-result
   (lisp-worker-request
    (lisp-tool-worker context arguments)
    :source
    (list :name (tool-argument arguments "name" :required t)
          :kind (tool-argument arguments "kind")))))

(defmethod tool-execute ((tool lisp-run-tests-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Run the required system's tests through CONTEXT's isolated worker."
  (declare (ignore tool))
  (worker-response-tool-result
   (lisp-worker-request
    (lisp-tool-worker context arguments)
    :run-tests
    (list :system (tool-argument arguments "system" :required t)))))

(defmethod tool-execute ((tool lisp-reset-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Reset the named REPL to pristine or an explicitly selected saved image."
  (declare (ignore tool))
  (let* ((manager (tool-context-worker context))
         (name (lisp-tool-repl-name arguments))
         (image (or (tool-argument arguments "image")
                    +pristine-lisp-image-identifier+)))
    (typecase manager
      (lisp-worker-pool
       (lisp-worker-pool-reset manager name image))
      (lisp-worker
       (unless (and (string= name (lisp-worker-name manager))
                    (string= image (lisp-worker-image-identifier manager)))
         (error 'worker-error
                :message "A legacy Lisp worker cannot switch its name or image."
                :tool-name "lisp.reset"))
       (lisp-worker-reset manager))
      (otherwise
       (error 'worker-error
              :message "No Lisp worker manager is available."
              :tool-name "lisp.reset")))
    (tool-success
     (format nil "Lisp REPL ~A was reset from image ~A." name image))))

(defmethod tool-execute ((tool lisp-start-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Start a named REPL from pristine or one compatible saved image."
  (declare (ignore tool))
  (let ((manager (tool-context-worker context))
        (name (lisp-tool-repl-name arguments))
        (image (or (tool-argument arguments "image")
                   +pristine-lisp-image-identifier+)))
    (unless (typep manager 'lisp-worker-pool)
      (error 'worker-error
             :message "Named REPL creation requires a Lisp worker pool."
             :tool-name "lisp.start"))
    (let ((worker (lisp-worker-pool-start manager name image)))
      (tool-success
       (format nil "Lisp REPL ~A is running from image ~A."
               name
               (lisp-worker-image-identifier worker))))))

(defmethod tool-execute ((tool lisp-stop-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Stop and forget one named REPL."
  (declare (ignore tool))
  (let ((manager (tool-context-worker context))
        (name (lisp-tool-repl-name arguments)))
    (unless (typep manager 'lisp-worker-pool)
      (error 'worker-error
             :message "Named REPL removal requires a Lisp worker pool."
             :tool-name "lisp.stop"))
    (lisp-worker-pool-stop manager name)
    (tool-success (format nil "Lisp REPL ~A was stopped." name))))

(defmethod tool-execute ((tool lisp-repls-tool)
                         (context tool-context)
                         (arguments hash-table))
  "List every named REPL in CONTEXT's worker pool."
  (declare (ignore tool arguments))
  (let ((manager (tool-context-worker context)))
    (unless (typep manager 'lisp-worker-pool)
      (error 'worker-error
             :message "Named REPL listing requires a Lisp worker pool."
             :tool-name "lisp.repls"))
    (tool-success (lisp-worker-pool-render manager))))

(defmethod tool-execute ((tool lisp-images-tool)
                         (context tool-context)
                         (arguments hash-table))
  "List pristine and saved Lisp worker images visible to CONTEXT."
  (declare (ignore tool arguments))
  (tool-success
   (lisp-image-render-inventory (tool-context-configuration context))))

(defmethod tool-execute ((tool lisp-save-image-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Save one named REPL as an immutable worker image with a durable note."
  (declare (ignore tool))
  (let* ((worker (lisp-tool-worker context arguments))
         (identifier (tool-argument arguments "image" :required t))
         (note (tool-argument arguments "note" :required t))
         (image
           (lisp-worker-save-image
            (tool-context-configuration context)
            worker
            identifier
            note)))
    (tool-success
     (format nil "Saved Lisp REPL ~A as image ~A.~%Parent: ~A~%Note: ~A"
             (lisp-worker-name worker)
             (lisp-image-identifier image)
             (lisp-image-parent-identifier image)
             (lisp-image-note image)))))


;;;; -- Worker Runtime --

(-> worker-read-form (string) t)
(defun worker-read-form (source)
  "Read exactly one executable Common Lisp form from SOURCE."
  (let ((*read-eval* t)
        (*package* (find-package '#:autolith))
        (end-marker (cons nil nil)))
    (multiple-value-bind (form position)
        (read-from-string source t nil)
      (let ((remainder (read-from-string source nil end-marker :start position)))
        (unless (eq remainder end-marker)
          (error "Expected exactly one Common Lisp form.")))
      form)))

(-> worker-render-value (t) string)
(defun worker-render-value (value)
  "Return a bounded readable representation of worker VALUE."
  (bounded-string
   (write-to-string value
                    :readably nil
                    :circle t
                    :level 10
                    :length 100)))

(-> worker-capture-evaluation (function) (values list string))
(defun worker-capture-evaluation (function)
  "Call FUNCTION while capturing output, returning rendered values and output."
  (let ((result-values nil))
    (let ((output
            (with-output-to-string (stream)
              (let ((*standard-output* stream)
                    (*error-output* stream)
                    (*trace-output* stream)
                    (*debug-io* stream)
                    (*package* (find-package '#:autolith)))
                (setf result-values
                      (multiple-value-list (funcall function)))))))
      (values (mapcar #'worker-render-value result-values) output))))


;;;; -- Matching Implementation Source --

(defparameter +worker-source-kinds+
  '(:class :compiler-macro :condition :constant :function :generic-function
    :macro :method :method-combination :package :setf-expander :structure
    :symbol-macro :type :alien-type :variable :declaration :optimizer
    :source-transform :transform :vop :ir1-convert)
  "SB-INTROSPECT definition kinds accepted by lisp.source.")

(-> worker-source--kind ((option string)) (option keyword))
(defun worker-source--kind (name)
  "Return the supported definition kind named NAME, or NIL for every kind."
  (when (non-empty-string-p name)
    (let ((kind (find (string-upcase name)
                      +worker-source-kinds+
                      :key #'symbol-name
                      :test #'string=)))
      (unless kind
        (error 'worker-error
               :message
               (format nil
                       "Unknown SBCL definition kind ~S. Choose one of ~{~(~A~)~^, ~}."
                       name
                       +worker-source-kinds+)
               :tool-name "lisp.source"))
      kind)))

(-> worker-source--name (string) t)
(defun worker-source--name (source)
  "Read and validate one definition name from SOURCE."
  (let ((name (worker-read-form source)))
    (unless (or (symbolp name)
                (stringp name)
                (and (consp name)
                     (eq (first name) 'setf)
                     (symbolp (second name))
                     (null (rest (rest name)))))
      (error 'worker-error
             :message "lisp.source needs a symbol, package string, or (SETF symbol) name."
             :tool-name "lisp.source"))
    name))

(-> worker-source--root () pathname)
(defun worker-source--root ()
  "Return the hash-verified source root matching this SBCL runtime."
  (let ((source-root (uiop:getenv "AUTOLITH_SBCL_SOURCE_ROOT")))
    (unless (and (non-empty-string-p source-root)
                 (uiop:directory-exists-p source-root))
      (error 'worker-error
             :message
             "Matching SBCL source is unavailable; run ./script/bootstrap to install it."
             :tool-name "lisp.source"))
    (uiop:ensure-directory-pathname (truename source-root))))

(-> worker-source--relative-pathname (pathname) pathname)
(defun worker-source--relative-pathname (pathname)
  "Map a recorded SBCL source PATHNAME into its exact archive-relative path."
  (let* ((directories
           (mapcar #'string-downcase
                   (remove-if-not #'stringp (pathname-directory pathname))))
         (start
           (position-if
            (lambda (component)
              (member component '("src" "contrib" "tests" "tools")
                      :test #'string=))
            directories))
         (name (pathname-name pathname))
         (type (pathname-type pathname)))
    (unless (and start name)
      (error 'worker-error
             :message (format nil "Cannot map recorded SBCL source pathname ~S."
                              pathname)
             :tool-name "lisp.source"))
    (pathname
     (format nil "~{~A/~}~A~@[.~A~]"
             (subseq directories start)
             (string-downcase name)
             (and type (string-downcase type))))))

(-> worker-source--pathname (pathname) pathname)
(defun worker-source--pathname (recorded-pathname)
  "Resolve RECORDED-PATHNAME only within the verified matching source tree."
  (let* ((source-root (worker-source--root))
         (pathname
           (merge-pathnames
            (worker-source--relative-pathname recorded-pathname)
            source-root)))
    (unless (and (uiop:subpathp pathname source-root)
                 (probe-file pathname))
      (error 'worker-error
             :message
             (format nil "Recorded source ~S is absent from matching SBCL source."
                     recorded-pathname)
             :tool-name "lisp.source"))
    (truename pathname)))

(-> worker-source--line-number (string integer) integer)
(defun worker-source--line-number (source offset)
  "Return the one-based line number containing OFFSET in SOURCE."
  (1+ (count #\Newline source :end (min offset (length source)))))

(-> worker-source--line-window (string integer) string)
(defun worker-source--line-window (source offset)
  "Return a numbered source window surrounding OFFSET."
  (let* ((target-line (worker-source--line-number source offset))
         (first-line (max 1 (- target-line 12)))
         (last-line (+ target-line 28)))
    (with-output-to-string (output)
      (with-input-from-string (input source)
        (loop for line = (read-line input nil nil)
              for line-number from 1
              while line
              when (<= first-line line-number last-line)
                do (format output "~5D  ~A~%" line-number line)
              when (> line-number last-line)
                do (return))))))

(-> worker-source--complete-form (string integer) (option string))
(defun worker-source--complete-form (source offset)
  "Return the complete readable top-level form at OFFSET when possible."
  (handler-case
      (let ((*package* (find-package '#:cl-user))
            (*read-eval* nil))
        (multiple-value-bind (form end)
            (read-from-string source t nil :start offset)
          (declare (ignore form))
          (subseq source offset end)))
    (error ()
      nil)))

(-> worker-source--fallback-offset (t string) integer)
(defun worker-source--fallback-offset (name source)
  "Return a useful textual location for NAME when debug data lacks an offset."
  (let* ((symbol
           (if (and (consp name) (eq (first name) 'setf))
               (second name)
               name))
         (needle
           (etypecase symbol
             (symbol (symbol-name symbol))
             (string symbol))))
    (or (search needle source :test #'char-equal) 0)))

(-> worker-source--render-location (t keyword t) string)
(defun worker-source--render-location (source-location kind name)
  "Render one SB-INTROSPECT SOURCE-LOCATION from verified matching source."
  (let* ((recorded
           (uiop:symbol-call '#:sb-introspect
                             '#:definition-source-pathname
                             source-location))
         (pathname (and recorded (worker-source--pathname recorded)))
         (source (and pathname (uiop:read-file-string pathname)))
         (recorded-offset
           (uiop:symbol-call '#:sb-introspect
                             '#:definition-source-character-offset
                             source-location))
         (offset (and source
                      (or recorded-offset
                          (worker-source--fallback-offset
                           name
                           source)))))
    (unless (and pathname source offset)
      (error 'worker-error
             :message "SBCL recorded no readable file location for this definition."
             :tool-name "lisp.source"))
    (let ((complete-form
            (and recorded-offset
                 (worker-source--complete-form source offset))))
      (with-output-to-string (output)
        (format output "Kind: ~(~A~)~%Path: ~A~%Line: ~D~%"
                kind
                (enough-namestring pathname (worker-source--root))
                (worker-source--line-number source offset))
        (if complete-form
            (write-string (bounded-string complete-form :limit 5000) output)
            (write-string (worker-source--line-window source offset) output))))))

(-> worker-source (string (option string)) (values list string))
(defun worker-source (name-source kind-source)
  "Return matching source locations for NAME-SOURCE and optional KIND-SOURCE."
  (require :sb-introspect)
  (let* ((name (worker-source--name name-source))
         (selected-kind (worker-source--kind kind-source))
         (kinds (if selected-kind
                    (list selected-kind)
                    +worker-source-kinds+))
         (locations nil)
         (seen (make-hash-table :test #'equal)))
    (dolist (kind kinds)
      (dolist (source-location
               (uiop:symbol-call '#:sb-introspect
                                 '#:find-definition-sources-by-name
                                 name
                                 kind))
        (let ((key
                (list kind
                      (uiop:symbol-call '#:sb-introspect
                                        '#:definition-source-pathname
                                        source-location)
                      (uiop:symbol-call '#:sb-introspect
                                        '#:definition-source-form-path
                                        source-location))))
          (unless (gethash key seen)
            (setf (gethash key seen) t)
            (push (cons kind source-location) locations)))))
    (unless locations
      (error 'worker-error
             :message
             (format nil "No ~:[SBCL ~;~(~A~) ~]definition source was found for ~S."
                     selected-kind selected-kind name)
             :tool-name "lisp.source"))
    (values
     nil
     (bounded-string
      (with-output-to-string (output)
        (loop for (kind . source-location) in (nreverse locations)
              for index from 1 to 8
              do (when (> index 1)
                   (format output "~%~%"))
                 (write-string
                  (worker-source--render-location source-location kind name)
                  output)
              finally
                 (when (> (length locations) 8)
                   (format output "~%~%~D additional source locations omitted."
                           (- (length locations) 8)))))
      :limit 12000))))

(-> worker--single-threaded-p () boolean)
(defun worker--single-threaded-p ()
  "Return true when the worker has no live Lisp thread besides this one."
  (notany (lambda (thread)
            (and (not (eq thread sb-thread:*current-thread*))
                 (sb-thread:thread-alive-p thread)))
          (sb-thread:list-all-threads)))

(-> worker--save-image-child (pathname string) null)
(defun worker--save-image-child (pathname identifier)
  "Save this forked worker heap to PATHNAME with embedded IDENTIFIER."
  (handler-case
      (progn
        (setf *worker-image-identifier* identifier)
        (sb-ext:save-lisp-and-die
         (namestring pathname)
         :toplevel #'worker-main
         :executable nil
         :purify nil
         :compression nil))
    (error ()
      (sb-posix:_exit 1)))
  nil)

(-> worker-save-image (pathname string) (values list string))
(defun worker-save-image (pathname identifier)
  "Fork a saver for this worker heap and return its portable result values."
  (unless (worker--single-threaded-p)
    (error 'worker-error
           :message "A Lisp worker image requires exactly one live Lisp thread."
           :tool-name "lisp.save-image"))
  (when (probe-file pathname)
    (error 'worker-error
           :message "The unpublished Lisp worker core already exists."
           :tool-name "lisp.save-image"))
  (let ((saver-pid (sb-posix:fork)))
    (if (zerop saver-pid)
        (worker--save-image-child pathname identifier)
        (multiple-value-bind (waited-pid status)
            (sb-posix:waitpid saver-pid 0)
          (unless (and (= waited-pid saver-pid)
                       (sb-posix:wifexited status)
                       (zerop (sb-posix:wexitstatus status)))
            (error 'worker-error
                   :message "The Lisp worker image saver failed."
                   :tool-name "lisp.save-image")))))
  (values (list (namestring pathname)) ""))

(-> worker-dispatch (keyword list) (values list string))
(defun worker-dispatch (operation arguments)
  "Execute worker OPERATION with portable ARGUMENTS."
  (ecase operation
    (:save-image
     (worker-save-image (pathname (getf arguments :pathname))
                        (getf arguments :identifier)))
    (:eval
     (worker-capture-evaluation
      (lambda ()
        (eval (worker-read-form (getf arguments :form))))))
    (:compile
     (worker-capture-evaluation
      (lambda ()
        (funcall
         (compile nil
                  `(lambda ()
                     ,(worker-read-form (getf arguments :form))))))))
    (:load-system
     (worker-capture-evaluation
      (lambda ()
        (uiop:symbol-call '#:ql '#:quickload (getf arguments :system)))))
    (:describe
     (worker-capture-evaluation
      (lambda ()
        (describe (worker-read-form (getf arguments :designator)))
        (values))))
    (:source
     (worker-source (getf arguments :name)
                    (getf arguments :kind)))
    (:run-tests
     (worker-capture-evaluation
      (lambda ()
        (asdf:test-system (getf arguments :system)))))))

(-> worker-condition-backtrace () string)
(defun worker-condition-backtrace ()
  "Return a bounded SBCL backtrace for the current worker condition."
  (bounded-string
   (with-output-to-string (stream)
     (sb-debug:print-backtrace :stream stream :count 20))
   :limit 6000))

(-> worker-handle-request (list) list)
(defun worker-handle-request (request)
  "Execute one portable worker REQUEST and return a protocol response."
  (let ((request-id (getf (rest request) :id))
        (operation (getf (rest request) :operation))
        (arguments (getf (rest request) :arguments)))
    (handler-case
        (multiple-value-bind (result-values output)
            (worker-dispatch operation arguments)
          (list :response
                :id request-id
                :status :ok
                :values result-values
                :output output))
      (error (condition)
        (list :response
              :id request-id
              :status :error
              :condition-type (string (type-of condition))
              :message (princ-to-string condition)
              :backtrace (worker-condition-backtrace))))))

(-> worker-main () null)
(defun worker-main ()
  "Run the isolated worker's line-oriented S-expression protocol until EOF."
  (let ((*package* (find-package '#:autolith))
        (*read-eval* nil)
        (*print-readably* t)
        (*print-circle* t))
    (prin1 (list :autolith-worker
                 :version 2
                 :image *worker-image-identifier*))
    (terpri)
    (finish-output)
    (loop for request = (read *standard-input* nil :end)
          until (eq request :end)
          do (let ((response
                     (if (and (listp request) (eq (first request) :request))
                         (worker-handle-request request)
                         (list :response
                               :id nil
                               :status :error
                               :message "Malformed worker request."
                               :backtrace ""))))
               (prin1 response)
               (terpri)
               (finish-output))))
  nil)
