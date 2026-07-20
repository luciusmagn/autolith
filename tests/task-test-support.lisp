(in-package #:autolith)

;;;; -- In-Process Task Orchestration Tests --

(defvar *task-test-command-decision* nil
  "The command decision observed by the task child executor test.")

(defvar *task-test-effect-count* 0
  "The number of deliberately observable trailing tool executions.")

(defvar *task-test-reader-evaluated-p* nil
  "Whether unsafe reader evaluation ran while parsing a native role file.")

(defclass task-test-authorization-tool (tool)
  ()
  (:documentation "Ask the supplied tool context to authorize one harmless command."))

(defmethod tool-execute
    ((tool task-test-authorization-tool)
     (context tool-context)
     (arguments hash-table))
  "Record and return the command decision supplied through CONTEXT."
  (declare (ignore tool arguments))
  (setf *task-test-command-decision*
        (tool-context-authorize-command
         context
         "true"
         (configuration-working-directory
          (tool-context-configuration context))))
  (tool-success (string-downcase *task-test-command-decision*)))

(defclass task-test-effect-tool (tool)
  ()
  (:documentation "Record whether a call after terminal yield was executed."))

(defclass task-test-abort-tool (tool)
  ()
  (:documentation "Signal the task cancellation control condition on execution."))

(defclass task-test-default-deny-tool (tool)
  ()
  (:documentation "Represent an ordinary extension that children must not inherit."))

(defclass task-test-child-safe-tool (task-test-default-deny-tool)
  ()
  (:documentation "Represent an extension that deliberately opts into child use."))

(defclass task-test-blocking-tool (task-test-child-safe-tool)
  ((lock
    :initform (make-lock "Autolith blocking tool test")
    :reader task-test-blocking-tool-lock
    :documentation "The lock protecting the test barrier.")
   (condition-variable
    :initform (make-condition-variable)
    :reader task-test-blocking-tool-condition-variable
    :documentation "The condition coordinating tool entry and release.")
   (started-p
    :initform nil
    :accessor task-test-blocking-tool-started-p
    :type boolean
    :documentation "True after a child enters the ordinary tool call.")
   (released-p
    :initform nil
    :accessor task-test-blocking-tool-released-p
    :type boolean
    :documentation "True when the test permits normal tool completion."))
  (:documentation "Block an ordinary child-safe tool call at a test barrier."))

(defclass task-test-publication-barrier ()
  ((lock
    :initform (make-lock "Autolith publication print test")
    :reader task-test-publication-barrier-lock
    :documentation "The lock protecting the publication barrier.")
   (condition-variable
    :initform (make-condition-variable)
    :reader task-test-publication-barrier-condition-variable
    :documentation "The condition coordinating artifact printing.")
   (reached-p
    :initform nil
    :accessor task-test-publication-barrier-reached-p
    :type boolean
    :documentation "True while terminal publication is printing the artifact.")
   (released-p
    :initform nil
    :accessor task-test-publication-barrier-released-p
    :type boolean
    :documentation "True when artifact printing may finish.")
   (failure
    :initarg :failure
    :initform nil
    :reader task-test-publication-barrier-failure
    :type (option keyword)
    :documentation "The optional ordinary error or task abort signalled on release."))
  (:documentation "A readable test value that pauses terminal artifact output."))

(defmethod tool-child-safe-p ((tool task-test-child-safe-tool))
  "Permit this test extension to cross the child capability boundary."
  (declare (ignore tool))
  t)

(defmethod tool-execute
    ((tool task-test-blocking-tool)
     (context tool-context)
     (arguments hash-table))
  "Wait at a test barrier until cancellation unwinds or the test releases it."
  (declare (ignore context arguments))
  (with-lock-held ((task-test-blocking-tool-lock tool))
    (setf (task-test-blocking-tool-started-p tool) t)
    (task--condition-broadcast
     (task-test-blocking-tool-condition-variable tool))
    (loop until (task-test-blocking-tool-released-p tool)
          do (condition-wait
              (task-test-blocking-tool-condition-variable tool)
              (task-test-blocking-tool-lock tool))))
  (tool-success "blocking tool released"))

(defmethod print-object
    ((barrier task-test-publication-barrier) stream)
  "Pause artifact printing until BARRIER is released, then print one keyword."
  (with-lock-held ((task-test-publication-barrier-lock barrier))
    (setf (task-test-publication-barrier-reached-p barrier) t)
    (task--condition-broadcast
     (task-test-publication-barrier-condition-variable barrier))
    (loop until (task-test-publication-barrier-released-p barrier)
          do (condition-wait
              (task-test-publication-barrier-condition-variable barrier)
              (task-test-publication-barrier-lock barrier))))
  (case (task-test-publication-barrier-failure barrier)
    (:error
     (error "Deliberate artifact publication failure."))
    (:abort
     (error 'task-aborted
            :message "Deliberate post-claim task abort."
            :reason :test-cancel))
    (otherwise
     (write-string ":TASK-TEST-PUBLICATION-BARRIER" stream))))

(defclass task-test-provider (model-provider)
  ((lock
    :initform (make-lock "Autolith task test provider")
    :reader task-test-provider-lock
    :documentation "The lock protecting deterministic request counters.")
   (mode
    :initarg :mode
   :reader task-test-provider-mode
   :type keyword
   :documentation
    "The :CONCURRENT, :NESTED, :NESTED-CANCEL, :BLOCKING-TOOL, :ASYNC-WAIT, or :MANIFEST script.")
   (active-count
    :initform 0
    :accessor task-test-provider-active-count
    :type (integer 0)
    :documentation "The provider requests currently executing.")
   (maximum-active-count
    :initform 0
    :accessor task-test-provider-maximum-active-count
    :type (integer 0)
    :documentation "The largest observed concurrent request count.")
   (request-count
    :initform 0
    :accessor task-test-provider-request-count
    :type (integer 0)
    :documentation "The total scripted requests observed.")
   (threads
    :initform nil
    :accessor task-test-provider-threads
    :type list
    :documentation "The distinct reusable workers that reached the provider."))
  (:documentation "A thread-safe provider for scheduler integration tests."))

(defmethod provider-with-configuration
    ((provider task-test-provider) (configuration configuration))
  "Share PROVIDER across test children while ignoring CONFIGURATION."
  (declare (ignore configuration))
  provider)

(defmethod provider-stream-turn
    ((provider task-test-provider)
     (conversation conversation)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Return a deterministic yield or nested task call for PROVIDER."
  (declare (ignore conversation tool-namespaces goal-context compaction-p))
  (let ((request-number nil))
    (with-lock-held ((task-test-provider-lock provider))
      (incf (task-test-provider-request-count provider))
      (incf (task-test-provider-active-count provider))
      (pushnew (current-thread) (task-test-provider-threads provider) :test #'eq)
      (setf request-number (task-test-provider-request-count provider)
            (task-test-provider-maximum-active-count provider)
            (max (task-test-provider-maximum-active-count provider)
                 (task-test-provider-active-count provider))))
    (unwind-protect
         (progn
           (when (eq (task-test-provider-mode provider) :concurrent)
             (sleep 0.05))
           (funcall event-callback
                    (make-instance 'assistant-delta-event :text "task test"))
           (agent-test-result
            (format nil "task-test-~D" request-number)
            (list
             (cond
               ((and (member (task-test-provider-mode provider)
                             '(:nested :nested-cancel)
                             :test #'eq)
                     (= request-number 1))
                (agent-test-call
                 :call-id "nested-task"
                 :namespace "task"
                 :name "run"
                 :arguments
                 (json-encode
                  (json-object "agent" "task"
                               "task" "Return the nested leaf result."))))
               ((or (and (eq (task-test-provider-mode provider)
                             :blocking-tool)
                         (= request-number 1))
                    (and (eq (task-test-provider-mode provider)
                             :nested-cancel)
                         (= request-number 2)))
                (agent-test-call
                 :call-id "blocking-tool"
                 :namespace "test"
                 :name "block"
                 :arguments "{}"))
               ((and (eq (task-test-provider-mode provider) :async-wait)
                     (= request-number 1))
                (agent-test-call
                 :call-id "spawn-detached-leaf"
                 :namespace "task"
                 :name "run"
                 :arguments
                 (json-encode
                  (json-object "name" "saturation-leaf"
                               "agent" "task"
                               "task" "Return the detached leaf result."
                               "async" t))))
               ((and (eq (task-test-provider-mode provider) :async-wait)
                     (= request-number 2))
                (agent-test-call
                 :call-id "wait-detached-leaf"
                 :namespace "job"
                 :name "wait"
                 :arguments
                 (json-encode
                  (json-object "id" "saturation-leaf-2"
                               "timeout-seconds" 1))))
               ((and (eq (task-test-provider-mode provider) :manifest)
                     (= request-number +task-maximum-batch-size+))
                (agent-test-call
                 :call-id (format nil "yield-~D" request-number)
                 :namespace "yield"
                 :name "submit"
                 :arguments
                 (json-encode
                  (json-object
                   "status" "failed"
                   "text" "The final manifest child failed."
                   "error" "AUTOLITH-LAST-MANIFEST-CHILD-FAILED"))))
               (t
                (agent-test-call
                 :call-id (format nil "yield-~D" request-number)
                 :namespace "yield"
                 :name "submit"
                 :arguments
                 (json-encode
                  (json-object
                   "status" "success"
                   "text"
                   (if (and (eq (task-test-provider-mode provider) :manifest)
                            (= request-number 1))
                       (make-string 100000 :initial-element #\X)
                       (format nil "result ~D" request-number))))))))))
      (with-lock-held ((task-test-provider-lock provider))
        (decf (task-test-provider-active-count provider))))))

(defmethod tool-execute
    ((tool task-test-effect-tool)
     (context tool-context)
     (arguments hash-table))
  "Increment the observable effect count for a real execution."
  (declare (ignore tool context arguments))
  (incf *task-test-effect-count*)
  (tool-success "effect executed"))

(defmethod tool-execute
    ((tool task-test-abort-tool)
     (context tool-context)
     (arguments hash-table))
  "Signal a deliberate task abort through ordinary registry dispatch."
  (declare (ignore tool context arguments))
  (error 'task-aborted
         :message "Task test was cancelled."
         :reason :test-cancel))

(-> task-tests--write-text (pathname string) pathname)
(defun task-tests--write-text (pathname contents)
  "Write CONTENTS to PATHNAME and return PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string contents stream))
  pathname)

(-> task-tests--wait-until (function (real 0)) boolean)
(defun task-tests--wait-until (predicate timeout-seconds)
  "Wait up to TIMEOUT-SECONDS for PREDICATE to become true."
  (let ((deadline
          (+ (get-internal-real-time)
             (* timeout-seconds internal-time-units-per-second))))
    (loop
      when (funcall predicate)
        return t
      when (>= (get-internal-real-time) deadline)
        return nil
      do (sleep 0.001))))

(-> task-tests--write-native-form (pathname t) pathname)
(defun task-tests--write-native-form (pathname form)
  "Write exactly one readable native FORM to PATHNAME and return PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (let ((*print-readably* t)
          (*print-pretty* t)
          (*print-circle* t))
      (prin1 form stream)
      (terpri stream)))
  pathname)

(-> task-tests--agent-definition-error (pathname keyword)
    task-agent-definition-error)
(defun task-tests--agent-definition-error (pathname source)
  "Parse PATHNAME and return its expected typed role diagnostic."
  (handler-case
      (progn
        (task-parse-agent-file pathname source)
        (error "Expected a task-agent-definition-error for ~A." pathname))
    (task-agent-definition-error (condition)
      condition)))

(-> task-tests--role-form (string string string &rest t) list)
(defun task-tests--role-form (name description instructions &rest properties)
  "Return a minimal native role form extended by PROPERTIES."
  (append (list :name name
                :description description
                :instructions instructions)
          properties))

(-> task-tests--yield-fixture
    (configuration task-agent-definition string)
    list)
(defun task-tests--yield-fixture (configuration definition identifier)
  "Return an isolated child, yield registry, and completion fixture."
  (let* ((orchestrator (task-orchestrator-create))
         (parent-registry (make-instance 'tool-registry))
         (parent-conversation
           (conversation-create
            configuration
            :identifier (format nil "~A-parent" identifier)))
         (parent
           (agent-create
            :configuration configuration
            :provider (make-instance 'model-provider)
            :conversation parent-conversation
            :tool-registry parent-registry
            :worker nil))
         (job
           (make-instance
            'task-job
            :orchestrator orchestrator
            :identity (list :id identifier :index 1)
            :execution-identifier (make-identifier)
            :definition definition
            :item (list :task "Exercise the terminal yield contract.")
            :parent-agent parent
            :root-conversation-identifier
            (conversation-identifier parent-conversation)
            :owner-identifiers nil
            :detached-p nil))
         (completion (make-instance 'task-completion))
         (registry
           (task-child-tool-registry
            parent-registry definition orchestrator 1))
         (conversation
           (conversation-create
            configuration
            :identifier (format nil "~A-child" identifier)))
         (child
           (make-instance
            'task-child-agent
            :configuration configuration
            :provider (make-instance 'model-provider)
            :conversation conversation
            :tool-registry registry
            :worker nil
            :definition definition
            :identity (task-job-identity job)
            :depth 1
            :completion completion
            :orchestrator orchestrator
            :job job))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation conversation
                          :registry registry
                          :agent child)))
    (list :registry registry
          :context context
          :completion completion
          :job job
          :child child
          :conversation conversation)))

(-> task-tests--execute-yield (list string) tool-result)
(defun task-tests--execute-yield (fixture arguments)
  "Execute provider JSON ARGUMENTS through FIXTURE's actual tool registry."
  (tool-registry-execute-call
   (getf fixture :registry)
   (json-object "namespace" "yield"
                "name" "submit"
                "arguments" arguments)
   (getf fixture :context)))

(-> task-tests--read-exact-native-value (string) t)
(defun task-tests--read-exact-native-value (source)
  "Read and return exactly one safe native value from SOURCE."
  (with-input-from-string (stream source)
    (let ((*read-eval* nil)
          (*readtable* (copy-readtable nil))
          (end (gensym "END")))
      (let ((value (read stream nil end)))
        (when (eq value end)
          (error "Expected one readable native value."))
        (unless (eq (read stream nil end) end)
          (error "Expected exactly one readable native value."))
        value))))

(-> task-tests--primary-agent
    (configuration string &optional tool-registry)
    agent)
(defun task-tests--primary-agent
    (configuration identifier &optional (registry (make-instance 'tool-registry)))
  "Return a primary test agent with conversation IDENTIFIER."
  (agent-create
   :configuration configuration
   :provider (make-instance 'model-provider)
   :conversation (conversation-create configuration :identifier identifier)
   :tool-registry registry
   :worker nil))

(-> task-tests--register-job
    (task-orchestrator agent task-agent-definition
     &key (:name (option string))
          (:owner-identifiers list)
          (:root-conversation-identifier (option string))
          (:detached-p boolean))
    task-job)
(defun task-tests--register-job
    (orchestrator parent definition
     &key name
       (owner-identifiers nil owner-identifiers-supplied-p)
       root-conversation-identifier
       (detached-p t))
  "Register one inert queued job with scheduler accounting for focused tests."
  (let ((root
          (or root-conversation-identifier
              (task-parent-root-conversation-identifier parent)))
        (owners
          (if owner-identifiers-supplied-p
              owner-identifiers
              (task-parent-owner-identifiers parent))))
    (with-lock-held ((task-orchestrator-lock orchestrator))
      (let* ((identity
               (task-orchestrator--create-identity
                orchestrator name
                (task-agent-definition-name definition)))
             (job
               (make-instance
                'task-job
                :orchestrator orchestrator
                :identity identity
                :execution-identifier (make-identifier)
                :definition definition
                :item (list :name name
                            :agent (task-agent-definition-name definition)
                            :task
                            (format nil "Hold ~A for a scheduler test."
                                    (or name "this unnamed job"))
                            :context nil
                            :async detached-p)
                :parent-agent parent
                :root-conversation-identifier root
                :owner-identifiers owners
                :parent-call-id nil
                :detached-p detached-p
                :command-authorization-function nil)))
        (setf (gethash (getf identity :id)
                       (task-orchestrator-jobs orchestrator))
              job)
        (incf (task-orchestrator-live-count orchestrator))
        job))))

(-> task-tests--child-viewer
    (configuration task-job
     &key (:depth (integer 1)) (:registry (option tool-registry)))
    task-child-agent)
(defun task-tests--child-viewer
    (configuration job &key (depth 1) registry)
  "Return a non-running child agent whose identity is JOB."
  (let ((identifier (getf (task-job-identity job) :id)))
    (make-instance
     'task-child-agent
     :configuration configuration
     :provider (make-instance 'model-provider)
     :conversation
     (conversation-create
      configuration
      :identifier (format nil "~A-viewer" identifier))
     :tool-registry (or registry (make-instance 'tool-registry))
     :worker nil
     :definition (task-job-definition job)
     :identity (task-job-identity job)
     :depth depth
     :completion (make-instance 'task-completion)
     :orchestrator (task-job-orchestrator job)
     :job job)))

(-> task-tests--terminal-result
    (task-job &key (:status keyword) (:output string)
                   (:error (option string)))
    list)
(defun task-tests--terminal-result
    (job &key (status :success) (output "done") error)
  "Return one portable terminal RESULT for JOB."
  (let ((identifier (getf (task-job-identity job) :id)))
    (list :id identifier
          :name identifier
          :agent (task-agent-definition-name (task-job-definition job))
          :agent-source (task-agent-definition-source
                         (task-job-definition job))
          :assignment (getf (task-job-item job) :task)
          :status status
          :output output
          :error error
          :yielded-p t
          :structured-output-present-p nil
          :structured-output nil
          :label nil
          :request-count 1
          :usage nil
          :duration-ms 1
          :model (configuration-model
                  (agent-configuration (task-job-parent-agent job)))
          :conversation-file nil
          :detached (task-job-detached-p job))))

(-> task-tests--job-tool-error-report
    (task-orchestrator agent &key (:operation string) (:identifier string))
    string)
(defun task-tests--job-tool-error-report
    (orchestrator viewer &key operation identifier)
  "Return the expected direct job-tool error report for VIEWER."
  (let* ((tool
           (make-instance
            'task-job-tool
            :orchestrator orchestrator
            :namespace "job"
            :name operation
            :description "Exercise nondisclosing job lookup."
            :parameters (tool-object-schema (json-object) nil)))
         (context
           (make-instance 'tool-context
                          :configuration (agent-configuration viewer)
                          :worker nil
                          :conversation (agent-conversation viewer)
                          :registry (agent-tool-registry viewer)
                          :agent viewer))
         (arguments
           (if (string= operation "wait")
               (json-object "id" identifier "timeout-seconds" 0)
               (json-object "id" identifier))))
    (handler-case
        (progn
          (tool-execute tool context arguments)
          (error "Expected job.~A lookup of ~A to fail."
                 operation identifier))
      (task-error (condition)
        (princ-to-string condition)))))
