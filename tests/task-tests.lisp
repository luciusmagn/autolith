(in-package #:autolith)

;;;; -- In-Process Task Orchestration Tests --

(defvar *task-test-command-decision* nil
  "The command decision observed by the task child executor test.")

(defvar *task-test-effect-count* 0
  "The number of deliberately observable trailing tool executions.")

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

(defmethod tool-execute
    ((tool task-test-effect-tool)
     (context tool-context)
     (arguments hash-table))
  "Increment the observable effect count for a real execution."
  (declare (ignore tool context arguments))
  (incf *task-test-effect-count*)
  (tool-success "effect executed"))

(-> task-tests--child-registry
    (task-agent-definition task-orchestrator)
    tool-registry)
(defun task-tests--child-registry (definition orchestrator)
  "Return a child registry with authorization, yield, and trailing-effect tools."
  (let ((registry
          (task-child-tool-registry
           (make-instance 'tool-registry)
           definition
           orchestrator
           1)))
    (tool-registry-register
     registry
     (make-instance 'task-test-authorization-tool
                    :namespace "test"
                    :name "authorize"
                    :description "Authorize a harmless command."
                    :parameters (tool-object-schema (json-object) nil)))
    (tool-registry-register
     registry
     (make-instance 'task-test-effect-tool
                    :namespace "test"
                    :name "effect"
                    :description "Record an observable test effect."
                    :parameters (tool-object-schema (json-object) nil)))
    registry))

(-> test-task-orchestration () null)
(defun test-task-orchestration ()
  "Test task registry setup, request validation, agent discovery, and yields."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let ((registry (task-augment-tool-registry
                            (make-default-tool-registry))))
             (test-assert (= (length (tool-registry-tools registry)) 51)
                          "task augmentation adds one task and four job tools")
             (dolist (name '("list" "get" "wait" "cancel"))
               (test-assert (tool-registry-find registry "job" name)
                            (format nil "task augmentation registers job.~A" name)))
             (test-assert (eq registry (task-augment-tool-registry registry))
                          "task augmentation is idempotent")
             (let* ((orchestrator
                      (task-run-tool-orchestrator
                       (tool-registry-find registry "task" "run")))
                    (definition
                      (task-find-agent-definition
                       (task-bundled-agent-definitions)
                       "task"))
                    (child-registry
                      (task-child-tool-registry
                       registry definition orchestrator 1)))
               (test-assert
                (tool-registry-find child-registry "search" "content")
                "general task children inherit native repository search")
               (test-assert
                (null (tool-registry-find child-registry "self" "status"))
                "task children never inherit active-image tools")))
           (let ((item (first (task-normalize-arguments
                               (json-object "task" "Inspect the parser."
                                            "agent" "SCOUT"
                                            "async" t)))))
             (test-assert (string= (getf item :agent) "scout")
                          "task normalization canonicalizes agent names")
             (test-assert (getf item :async)
                          "task normalization preserves detached execution"))
           (test-assert
            (handler-case
                (progn
                  (task-normalize-arguments
                   (json-object "tasks"
                                (json-array
                                 (json-object "task" "First")
                                 (json-object "task" "Second"))))
                  nil)
              (task-error ()
                t))
            "batch task normalization requires shared context")
           (let* ((agent-directory (merge-pathnames ".autolith/agents/" root))
                  (agent-path      (merge-pathnames "scout.md" agent-directory))
                  (project-configuration
                    (configuration--clone configuration :working-directory root)))
             (ensure-directories-exist agent-path)
             (with-open-file (stream agent-path
                                     :direction :output
                                     :if-exists :supersede)
               (format stream "---~%name: scout~%description: Project scout~%---~%Project instructions."))
             (let ((definition
                     (task-find-agent-definition
                      (task-discover-agents project-configuration)
                      "scout")))
               (test-assert (eq (task-agent-definition-source definition) :project)
                            "project agents override bundled definitions")
               (test-assert (string= (task-agent-definition-system-prompt definition)
                                     "Project instructions.")
                            "agent discovery retains the Markdown body")))
           (let* ((immutable
                    (configuration--clone configuration :immutable-p t))
                  (definition
                    (task-agent-definition-create
                     :name "inheritance"
                     :description "Exercise configuration inheritance."
                     :system-prompt ""
                     :tools :all
                     :models '("@parent")
                     :source ':test))
                  (child-configuration
                    (task-configuration-for-definition immutable definition)))
             (test-assert
              (and (configuration-immutable-p child-configuration)
                   (equal (configuration-config-root child-configuration)
                          (configuration-config-root immutable))
                   (equal (configuration-data-root child-configuration)
                          (configuration-data-root immutable))
                   (equal (configuration-state-root child-configuration)
                          (configuration-state-root immutable))
                   (equal (configuration-cache-root child-configuration)
                          (configuration-cache-root immutable))
                   (equal (configuration-provider-endpoint child-configuration)
                          (configuration-provider-endpoint immutable)))
              "task model selection preserves every parent runtime boundary"))
           (let* ((definition
                    (task-agent-definition-create
                     :name "structured"
                     :description "Yield structured data."
                     :system-prompt ""
                     :output (json-object "type" "object"
                                          "required" (json-array "answer")
                                          "properties"
                                          (json-object "answer"
                                                       (json-object "type" "string")))
                     :source :test))
                  (completion (make-instance 'task-completion))
                  (orchestrator (task-orchestrator-create))
                  (parent (agent-create
                           :configuration configuration
                           :provider (make-instance 'model-provider)
                           :conversation (conversation-create configuration)
                           :tool-registry (make-instance 'tool-registry)
                           :worker nil))
                  (job (make-instance 'task-job
                                      :orchestrator orchestrator
                                      :identity (list :id "yield-test" :index 1)
                                      :definition definition
                                      :item (list :task "Yield")
                                      :parent-agent parent
                                      :detached-p nil))
                  (child (make-instance 'task-child-agent
                                        :configuration configuration
                                        :provider (make-instance 'model-provider)
                                        :conversation (conversation-create configuration)
                                        :tool-registry (make-instance 'tool-registry)
                                        :worker nil
                                        :definition definition
                                        :identity (task-job-identity job)
                                        :depth 1
                                        :completion completion
                                        :orchestrator orchestrator
                                        :job job))
                  (context (make-instance 'tool-context
                                          :configuration configuration
                                          :worker nil
                                          :conversation nil
                                          :agent child))
                  (tool (make-instance 'task-yield-tool
                                       :namespace "yield"
                                       :name "submit"
                                       :description ""
                                       :parameters (json-object))))
             (test-assert
              (handler-case
                  (progn
                    (tool-execute tool context
                                  (json-object "status" "success"
                                               "data" (json-object "wrong" "shape")))
                    nil)
                (task-yield-error ()
                  t))
              "yield validation rejects data outside the output contract")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-task-child-shared-agent-loop () null)
(defun test-task-child-shared-agent-loop ()
  "Test child yield uses the ordinary provider and tool execution path."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (definition
           (task-agent-definition-create
            :name "runtime"
            :description "Exercise the shared child runtime."
            :system-prompt "Yield after checking authorization."
            :source ':test))
         (orchestrator (task-orchestrator-create))
         (parent
           (agent-create
            :configuration configuration
            :provider (make-instance 'model-provider)
            :conversation (conversation-create configuration)
            :tool-registry (make-instance 'tool-registry)
            :worker nil))
         (job
           (make-instance 'task-job
                          :orchestrator orchestrator
                          :identity (list :id "runtime-child" :index 1)
                          :definition definition
                          :item (list :task "Exercise the shared loop.")
                          :parent-agent parent
                          :detached-p nil))
         (completion (make-instance 'task-completion))
         (conversation
           (conversation-create configuration :identifier "task-shared-loop"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list
             (agent-test-result
              "child-yield"
              (list
               (agent-test-call :call-id "authorize"
                                :namespace "test"
                                :name "authorize")
               (agent-test-call
                :call-id "yield"
                :namespace "yield"
                :name "submit"
                :arguments
                "{\"status\":\"success\",\"text\":\"done\"}")
               (agent-test-call :call-id "effect"
                                :namespace "test"
                                :name "effect"))))))
         (child
           (make-instance 'task-child-agent
                          :configuration configuration
                          :provider provider
                          :conversation conversation
                          :tool-registry
                          (task-tests--child-registry definition orchestrator)
                          :worker nil
                          :definition definition
                          :identity (task-job-identity job)
                          :depth 1
                          :completion completion
                          :orchestrator orchestrator
                          :job job))
         (observer
           (callback-agent-observer-create
            :command-authorization-callback
            (lambda (command directory)
              (declare (ignore command directory))
              ':sandboxed))))
    (unwind-protect
         (let ((*task-test-command-decision* nil)
               (*task-test-effect-count* 0))
           (agent-run-user-turn child "Run the shared loop." :observer observer)
           (test-assert (task-completion-called-p completion)
                        "yield.submit completes a child through the shared loop")
           (test-assert (eq *task-test-command-decision* ':sandboxed)
                        "child tools receive the ordinary command authorization path")
           (test-assert (zerop *task-test-effect-count*)
                        "calls after terminal yield are not executed")
           (let* ((records (conversation--read-records
                            (conversation-pathname conversation)))
                  (results (remove-if-not
                            (lambda (record)
                              (eq (first record) :tool-result))
                            records)))
             (test-assert
              (equal (mapcar (lambda (record)
                               (getf (rest record) :status))
                             results)
                     '(:ok :ok :error))
              "yield retains its result and rejects every trailing call")
             (test-assert
              (and (every (lambda (record)
                            (typep (getf (rest record) :cpu-microseconds)
                                   '(integer 0)))
                          (subseq results 0 2))
                   (null (getf (rest (third results)) :cpu-microseconds)))
              "executed child calls retain timings while rejected calls omit them")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
