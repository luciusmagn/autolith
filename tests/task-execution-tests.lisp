(in-package #:autolith)

;;;; -- Task Child Execution Tests --

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

(-> test-task-abort-control-condition () null)
(defun test-task-abort-control-condition ()
  "Test that registry dispatch preserves the internal cancellation unwind."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (conversation  (conversation-create configuration))
         (registry      (make-instance 'tool-registry))
         (tool
           (make-instance
            'task-test-abort-tool
            :namespace "test"
            :name "abort"
            :description "Signal a task cancellation."
            :parameters (tool-object-schema (json-object) nil)))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation conversation
                          :registry registry)))
    (unwind-protect
         (progn
           (tool-registry-register registry tool)
           (test-assert
            (handler-case
                (progn
                  (tool-registry-execute-call
                   registry
                   (json-object "namespace" "test"
                                "name" "abort"
                                "arguments" "{}")
                   context)
                  nil)
              (task-aborted (condition)
                (and (eq (task-aborted-reason condition) :test-cancel)
                     (string= (task-aborted-message condition)
                              "Task test was cancelled.")))
              (condition ()
                nil))
            "tool registry dispatch propagates task-aborted as control flow"))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-orchestration () null)
(defun test-task-orchestration ()
  "Test task registry setup, request validation, agent discovery, and yields."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((registry (make-default-tool-registry))
                  (initial-count (length (tool-registry-tools registry))))
             (task-augment-tool-registry registry)
             (test-assert
              (= (length (tool-registry-tools registry))
                 (+ initial-count 6))
              "task augmentation adds two task and four job tools")
             (dolist (name '("run" "agents"))
               (test-assert (tool-registry-find registry "task" name)
                            (format nil
                                    "task augmentation registers task.~A"
                                    name)))
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
           (let* ((registry (make-default-tool-registry))
                  (local-definition
                    (task-agent-definition-create
                     :name "local-grant"
                     :description "Use one available local tool."
                     :instructions "Read one file."
                     :tools '("fs.read")
                     :source :test))
                  (hosted-definition
                    (task-agent-definition-create
                     :name "hosted-grant"
                     :description "Use hosted provider search."
                     :instructions "Search one authoritative source."
                     :tools '("web_search")
                     :source :test))
                  (missing-definition
                    (task-agent-definition-create
                     :name "missing-grant"
                     :description "Request one unavailable local tool."
                     :instructions "Exercise fail-closed grant validation."
                     :tools '("missing.operation")
                     :source :test)))
             (test-assert
              (handler-case
                  (progn
                    (task-agent-definition-validate-tools-available
                     local-definition registry)
                    t)
                (task-agent-definition-error ()
                  nil))
              "available child-safe local grants validate against the registry")
             (test-assert
              (handler-case
                  (progn
                    (task-agent-definition-validate-tools-available
                     hosted-definition registry)
                    t)
                (task-agent-definition-error ()
                  nil))
              "web_search remains a recognized hosted provider grant")
             (test-assert
              (handler-case
                  (progn
                    (task-agent-definition-validate-tools-available
                     missing-definition registry)
                    nil)
                (task-agent-definition-error (condition)
                  (eq (task-agent-definition-error-field condition) :tools)))
              "unavailable local tool grants fail closed with typed metadata"))
           (let* ((parent-registry (make-instance 'tool-registry))
                  (definition
                    (task-agent-definition-create
                     :name "extension-boundary"
                     :description "Exercise extension capability defaults."
                     :instructions "Use only explicitly child-safe extensions."
                     :tools :all
                     :source :test))
                  (orchestrator (task-orchestrator-create)))
             (tool-registry-register
              parent-registry
              (make-instance 'task-test-default-deny-tool
                             :namespace "extension"
                             :name "denied"
                             :description "Remain unavailable to children."
                             :parameters
                             (tool-object-schema (json-object) nil)))
             (tool-registry-register
              parent-registry
              (make-instance 'task-test-child-safe-tool
                             :namespace "extension"
                             :name "allowed"
                             :description "Opt into child availability."
                             :parameters
                             (tool-object-schema (json-object) nil)))
             (let ((child-registry
                     (task-child-tool-registry
                      parent-registry definition orchestrator 1)))
               (test-assert
                (null
                 (tool-registry-find child-registry "extension" "denied"))
                "ordinary extension tools default closed for child agents")
               (test-assert
                (tool-registry-find child-registry "extension" "allowed")
                "a class-specific child-safe method opts an extension in")))
           (let ((item (first (task-normalize-arguments
                               (json-object "task" "Inspect the parser."
                                            "agent" "SCOUT"
                                            "async" t)))))
             (test-assert (string= (getf item :agent) "scout")
                          "task normalization canonicalizes agent names")
             (test-assert (getf item :async)
                          "task normalization preserves detached execution"))
           (let ((item (first (task-normalize-arguments
                               (json-object "task" "Stay synchronous."
                                            "async" false)))))
             (test-assert (null (getf item :async))
                          "JSON false remains false for task async policy"))
           (let* ((registry
                    (task-augment-tool-registry
                     (make-default-tool-registry)))
                  (conversation
                    (conversation-create
                     configuration :identifier "task-null-dispatch"))
                  (parent
                    (agent-create
                     :configuration configuration
                     :provider (make-instance 'model-provider)
                     :conversation conversation
                     :tool-registry registry
                     :worker nil))
                  (context
                    (make-instance 'tool-context
                                   :configuration configuration
                                   :worker nil
                                   :conversation conversation
                                   :registry registry
                                   :agent parent))
                  (tool
                    (tool-registry-find registry "task" "run"))
                  (orchestrator (task-run-tool-orchestrator tool)))
             (unwind-protect
                  (let ((result
                          (tool-registry-execute-call
                           registry
                           (json-object
                            "namespace" "task"
                            "name" "run"
                            "arguments"
                            "{\"task\":\"Reject null async.\",\"async\":null}")
                           context)))
                    (test-assert
                     (and (not (tool-result-success-p result))
                          (null
                           (task-orchestrator-list-jobs orchestrator)))
                     "registry task.run decoding rejects JSON null before job admission"))
               (tool-registry-close-runtime-state registry)))
           (test-assert
            (handler-case
                (progn
                  (task-normalize-arguments
                   (json-object "task" "Reject removed fields."
                                "isolated" false))
                  nil)
              (task-error () t))
            "task normalization rejects the removed isolated field")
           (test-assert
            (handler-case
                (progn
                  (task-normalize-arguments
                   (json-object "task" "Reject bad booleans."
                                "async" "false"))
                  nil)
              (task-error () t))
            "task normalization rejects non-boolean async values")
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
           (dolist
               (case
                (list
                 (list
                  (json-object
                   "name" "forbidden-top-level-name"
                   "context" "Shared batch context."
                   "tasks" (json-array (json-object "task" "First")))
                  "batch task normalization rejects a top-level name")
                 (list
                  (json-object
                   "agent" "scout"
                   "context" "Shared batch context."
                   "tasks" (json-array (json-object "task" "First")))
                  "batch task normalization rejects a top-level agent")
                 (list
                 (json-object
                   "context" "Shared batch context."
                   "tasks"
                   (json-array
                    (json-object "task" "First" "legacy" t)))
                  "batch items reject unknown fields")
                 (list
                  (json-object
                   "context" "Shared batch context."
                   "tasks" "this string is not a task array")
                  "batch tasks reject strings despite their vector representation")))
             (test-assert
              (handler-case
                  (progn
                    (task-normalize-arguments (first case))
                    nil)
                (task-error ()
                  t))
              (second case)))
           (let* ((agent-directory (merge-pathnames ".autolith/agents/" root))
                  (agent-path      (merge-pathnames "scout.sexp" agent-directory))
                  (project-configuration
                    (configuration--clone configuration :working-directory root)))
             (task-tests--write-native-form
              agent-path
              (task-tests--role-form
               "scout" "Project scout" "Project instructions."))
             (let ((definition
                     (task-find-agent-definition
                      (task-discover-agents project-configuration)
                      "scout")))
               (test-assert (eq (task-agent-definition-source definition) :project)
                            "project agents override bundled definitions")
               (test-assert (string= (task-agent-definition-instructions definition)
                                     "Project instructions.")
                            "agent discovery retains native role instructions")))
           (let* ((immutable
                    (configuration--clone configuration :immutable-p t))
                  (definition
                    (task-agent-definition-create
                     :name "inheritance"
                     :description "Exercise configuration inheritance."
                     :instructions "Preserve inherited runtime configuration."
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
                     :instructions "Yield data matching the native output contract."
                     :output '(:type :object
                               :properties (("answer" (:type :string)))
                               :required ("answer"))
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
                                      :execution-identifier (make-identifier)
                                      :definition definition
                                      :item (list :task "Yield")
                                      :parent-agent parent
                                      :root-conversation-identifier
                                      (conversation-identifier
                                       (agent-conversation parent))
                                      :owner-identifiers nil
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
            :instructions "Yield after checking authorization."
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
                          :execution-identifier (make-identifier)
                          :definition definition
                          :item (list :task "Exercise the shared loop.")
                          :parent-agent parent
                          :root-conversation-identifier
                          (conversation-identifier
                           (agent-conversation parent))
                          :owner-identifiers nil
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
