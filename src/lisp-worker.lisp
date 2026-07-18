(in-package #:autolith)

;;;; -- SBCL Worker Adapters --

(deftype lisp-worker ()
  "A persistent isolated process managed by sbcl-workers."
  'sbcl-worker)

(deftype lisp-worker-pool ()
  "A named collection of isolated processes managed by sbcl-workers."
  'sbcl-worker-pool)

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

(-> lisp-worker--tool-name ((or string keyword null)) string)
(defun lisp-worker--tool-name (operation)
  "Return Autolith's dotted tool name for library OPERATION."
  (case operation
    (:change-working-directory "lisp.cwd")
    (:workers "lisp.repls")
    (:save-image "lisp.save-image")
    ((nil)
     "lisp.worker")
    (otherwise
     (format nil "lisp.~(~A~)" operation))))

(-> lisp-worker--call (function) t)
(defun lisp-worker--call (function)
  "Call FUNCTION and translate sbcl-workers conditions for Autolith."
  (handler-case
      (funcall function)
    (sbcl-worker-image-error (condition)
      (error 'lisp-image-error
             :message (sbcl-worker-error-message condition)
             :tool-name
             (lisp-worker--tool-name (sbcl-worker-error-operation condition))
             :pathname (sbcl-worker-error-pathname condition)
             :stage (or (sbcl-worker-error-stage condition) ':manifest)))
    (sbcl-worker-error (condition)
      (error 'worker-error
             :message (sbcl-worker-error-message condition)
             :tool-name
             (lisp-worker--tool-name
              (sbcl-worker-error-operation condition))))))

(-> lisp-worker-sbcl-command () string)
(defun lisp-worker-sbcl-command ()
  "Return the configured SBCL executable used by disposable workers."
  (let ((configured-command (uiop:getenv "AUTOLITH_SBCL")))
    (if (non-empty-string-p configured-command)
        configured-command
        "sbcl")))

(-> lisp-worker-create
    (configuration &key (:name string) (:image-identifier string))
    lisp-worker)
(defun lisp-worker-create
    (configuration &key (name "default")
                        (image-identifier +pristine-lisp-image-identifier+))
  "Create a stopped named worker based on IMAGE-IDENTIFIER."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-create
      (lisp-worker--environment configuration)
      :name name
      :image-identifier image-identifier))))

(-> lisp-worker-name (lisp-worker) string)
(defun lisp-worker-name (worker)
  "Return WORKER's stable REPL name."
  (sbcl-worker-name worker))

(-> lisp-worker-image-identifier (lisp-worker) string)
(defun lisp-worker-image-identifier (worker)
  "Return the pristine or saved image used by WORKER."
  (sbcl-worker-used-image-identifier worker))

(-> lisp-worker-running-p (lisp-worker) boolean)
(defun lisp-worker-running-p (worker)
  "Return true when WORKER has a live subprocess."
  (sbcl-worker-running-p worker))

(-> lisp-worker-start (lisp-worker) lisp-worker)
(defun lisp-worker-start (worker)
  "Start WORKER when necessary and verify its protocol handshake."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-start worker))))

(-> lisp-worker-stop (lisp-worker) null)
(defun lisp-worker-stop (worker)
  "Terminate WORKER and discard its process streams and heap state."
  (sbcl-worker-stop worker))

(-> lisp-worker-reset (lisp-worker) lisp-worker)
(defun lisp-worker-reset (worker)
  "Restart WORKER from the same pristine or saved image."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-reset worker))))

(-> lisp-worker-change-working-directory
    (lisp-worker configuration)
    lisp-worker)
(defun lisp-worker-change-working-directory (worker configuration)
  "Move WORKER to CONFIGURATION's workspace without discarding its heap."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-change-working-directory
      worker
      (lisp-worker--environment configuration)))))

(-> lisp-worker-request (lisp-worker keyword list) list)
(defun lisp-worker-request (worker operation arguments)
  "Send OPERATION and portable ARGUMENTS to WORKER and return its response."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-request worker operation arguments))))


;;;; -- Named Worker Pool Adapters --

(-> lisp-worker-pool-create (configuration) lisp-worker-pool)
(defun lisp-worker-pool-create (configuration)
  "Create an empty named Lisp worker pool for CONFIGURATION."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-pool-create
      (lisp-worker--environment configuration)))))

(-> lisp-worker-pool-configuration (lisp-worker-pool) configuration)
(defun lisp-worker-pool-configuration (pool)
  "Return the Autolith configuration currently associated with POOL."
  (sbcl-worker-environment-context
   (sbcl-worker-pool-environment pool)))

(-> lisp-worker-pool-start
    (lisp-worker-pool string (option string))
    lisp-worker)
(defun lisp-worker-pool-start (pool name image-identifier)
  "Start or return NAME, enforcing IMAGE-IDENTIFIER when supplied."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-pool-start pool name image-identifier))))

(-> lisp-worker-pool-worker (lisp-worker-pool string) lisp-worker)
(defun lisp-worker-pool-worker (pool name)
  "Return NAME, starting it from pristine SBCL when absent."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-pool-worker pool name))))

(-> lisp-worker-pool-stop
    (lisp-worker-pool string &key (:if-missing keyword))
    null)
(defun lisp-worker-pool-stop (pool name &key (if-missing :error))
  "Stop and forget named REPL NAME."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-pool-stop pool name :if-missing if-missing))))

(-> lisp-worker-pool-reset (lisp-worker-pool string string) lisp-worker)
(defun lisp-worker-pool-reset (pool name image-identifier)
  "Replace named REPL NAME with a fresh process from IMAGE-IDENTIFIER."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-pool-reset pool name image-identifier))))

(-> lisp-worker-pool-stop-all (lisp-worker-pool) null)
(defun lisp-worker-pool-stop-all (pool)
  "Stop and forget every REPL managed by POOL."
  (sbcl-worker-pool-stop-all pool))

(-> lisp-worker-pool-change-working-directory
    (lisp-worker-pool configuration)
    lisp-worker-pool)
(defun lisp-worker-pool-change-working-directory (pool configuration)
  "Move every REPL in POOL to CONFIGURATION with rollback on failure."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-pool-change-working-directory
      pool
      (lisp-worker--environment configuration)))))

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
  (sbcl-worker-pool-render pool))

(-> lisp-worker-save-image
    (configuration lisp-worker &key (:identifier string) (:note string))
    lisp-image)
(defun lisp-worker-save-image (configuration worker &key identifier note)
  "Save WORKER as immutable IDENTIFIER with durable NOTE."
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-save-image
      (lisp-worker--environment configuration)
      worker
      :identifier identifier
      :note note))))


;;;; -- Tool Result Adaptation --

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
    (list :designator
          (tool-argument arguments "designator" :required t)))))

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
            :identifier identifier
            :note note)))
    (tool-success
     (format nil "Saved Lisp REPL ~A as image ~A.~%Parent: ~A~%Note: ~A"
             (lisp-worker-name worker)
             (lisp-image-identifier image)
             (lisp-image-parent-identifier image)
             (lisp-image-note image)))))


;;;; -- Worker Runtime Entry Points --

(-> worker-source (string (option string)) (values list string))
(defun worker-source (name kind)
  "Return exact matching SBCL source for NAME and optional KIND."
  (sbcl-worker-runtime-configure
   :evaluation-package "AUTOLITH"
   :protocol-tag ':autolith-worker
   :protocol-version 2
   :source-root-environment-variable "AUTOLITH_SBCL_SOURCE_ROOT")
  (lisp-worker--call
   (lambda ()
     (sbcl-worker-source name kind))))

(-> worker-handle-request (list) list)
(defun worker-handle-request (request)
  "Execute one portable worker REQUEST through sbcl-workers."
  (sbcl-worker-runtime-configure
   :evaluation-package "AUTOLITH"
   :protocol-tag ':autolith-worker
   :protocol-version 2
   :source-root-environment-variable "AUTOLITH_SBCL_SOURCE_ROOT")
  (sbcl-worker-handle-request request))

(-> worker-main () null)
(defun worker-main ()
  "Run Autolith's isolated worker protocol until standard-input reaches EOF."
  (sbcl-worker-main
   :evaluation-package "AUTOLITH"
   :protocol-tag ':autolith-worker
   :protocol-version 2
   :source-root-environment-variable "AUTOLITH_SBCL_SOURCE_ROOT"))
