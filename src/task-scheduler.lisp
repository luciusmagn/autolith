(in-package #:autolith)

;;;; -- Task Scheduling and Publication --

(defun task-job--set-progress-state (job state)
  "Set JOB's normalized progress STATE."
  (let ((progress (task-job-progress job)))
    (with-lock-held ((task-progress-lock progress))
      (setf (task-progress-status progress) state
            (task-progress-updated-at progress) (get-internal-real-time))
      (when (eq state :running)
        (setf (task-progress-started-at progress) (get-internal-real-time)))))
  nil)

(-> task--retained-prefix (string integer) string)
(defun task--retained-prefix (text limit)
  "Return at most LIMIT leading characters from TEXT."
  (subseq text 0 (min limit (length text))))

(-> task-job--compact-result
    (list &key (:artifact-available-p boolean))
    list)
(defun task-job--compact-result (result &key artifact-available-p)
  "Return a bounded terminal summary of RESULT and its artifact availability."
  (let ((retained
          (loop for field in
                  '(:id :name :agent :agent-source :assignment :status
                    :output :error :yielded-p
                    :structured-output-present-p :structured-output :label
                    :request-count :usage :duration-ms :model
                    :conversation-file :detached :output-path
                    :agent-definition)
                append (list field (getf result field))))
        (storage (if artifact-available-p :artifact :omitted)))
    (flet ((compact-string
              (field limit &key storage-field characters-field)
             (let ((value (getf retained field)))
               (when (and (stringp value) (> (length value) limit))
                 (setf (getf retained field)
                       (task--retained-prefix value limit)
                       (getf retained storage-field) storage
                       (getf retained characters-field) (length value))))))
      (compact-string :assignment *task-retained-assignment-limit*
                      :storage-field :assignment-storage
                      :characters-field :assignment-characters)
      (compact-string :output *task-retained-output-limit*
                      :storage-field :output-storage
                      :characters-field :output-characters)
      (compact-string :error *task-retained-output-limit*
                      :storage-field :error-storage
                      :characters-field :error-characters)
      (compact-string :label *task-result-label-maximum-characters*
                      :storage-field :label-storage
                      :characters-field :label-characters))
    (when (getf retained :structured-output-present-p)
      (let* ((value (getf retained :structured-output))
             (serialized (task--write-readable-sexp value)))
        (when (> (length serialized)
                 *task-retained-structured-output-limit*)
          (setf (getf retained :structured-output) nil
                (getf retained :structured-output-storage) storage
                (getf retained :structured-output-characters)
                (length serialized)))))
    (let* ((usage (getf retained :usage))
           (serialized (and usage (task--write-readable-sexp usage))))
      (when (and serialized
                 (> (length serialized) *task-retained-usage-limit*))
        (setf (getf retained :usage) nil
              (getf retained :usage-storage) storage
              (getf retained :usage-characters) (length serialized))))
    retained))

(-> task-job--compact-progress (task-job keyword) null)
(defun task-job--compact-progress (job state)
  "Make JOB's progress terminal and release its large transient fields."
  (let ((progress (task-job-progress job)))
    (with-lock-held ((task-progress-lock progress))
      (let* ((output (task-progress-output-tail progress))
             (start
               (max 0
                    (- (length output)
                       *task-retained-progress-output-limit*))))
        (setf (task-progress-status progress) state
              (task-progress-current-tool progress) nil
              (task-progress-output-tail progress) (subseq output start)
              (task-progress-usage progress)
              (task--compact-native-value
               (task-progress-usage progress)
               *task-retained-usage-limit*)
              (task-progress-updated-at progress) (get-internal-real-time)))))
  nil)

(-> task-job--compact-item (task-job) list)
(defun task-job--compact-item (job)
  "Return the bounded assignment metadata retained for terminal JOB."
  (let ((item (task-job-item job)))
    (list :name (getf item :name)
          :agent (getf item :agent)
          :task (task--retained-prefix
                 (or (getf item :task) "")
                 *task-retained-assignment-limit*)
          :async (getf item :async))))

(-> task--compact-native-value (t integer) t)
(defun task--compact-native-value (value limit)
  "Return native VALUE or a descriptor when its readable form exceeds LIMIT."
  (let ((characters (length (task--write-readable-sexp value))))
    (if (<= characters limit)
        value
        (list :omitted :characters characters))))

(-> task--agent-definition-summary (task-agent-definition) list)
(defun task--agent-definition-summary (definition)
  "Return compact non-instruction metadata for DEFINITION."
  (let ((pathname (task-agent-definition-pathname definition))
        (output (task-agent-definition-output definition)))
    (list :name (task-agent-definition-name definition)
          :source (task-agent-definition-source definition)
          :pathname (and pathname (namestring pathname))
          :tools
          (task--compact-native-value
           (task-agent-definition-tools definition) 1000)
          :spawns
          (task--compact-native-value
           (task-agent-definition-spawns definition) 1000)
          :models
          (task--compact-native-value
           (task-agent-definition-models definition) 1000)
          :reasoning-effort
          (task-agent-definition-reasoning-effort definition)
          :output-contract-p (and output t)
          :blocking-p
          (and (task-agent-definition-blocking-p definition) t))))

(-> task-orchestrator--retain-terminal-locked
    (task-orchestrator task-job)
    null)
(defun task-orchestrator--retain-terminal-locked (orchestrator job)
  "Account for terminal JOB while its lifecycle lock is held."
  (unless (task-job-retained-p job)
    (setf (task-job-retained-p job) t)
    (with-lock-held ((task-orchestrator-lock orchestrator))
      (when (plusp (task-orchestrator-live-count orchestrator))
        (decf (task-orchestrator-live-count orchestrator)))
      (let ((identifier (getf (task-job-identity job) :id)))
        (setf (task-orchestrator-terminal-identifiers orchestrator)
              (nconc (task-orchestrator-terminal-identifiers orchestrator)
                     (list identifier)))
        (loop while (> (length
                        (task-orchestrator-terminal-identifiers orchestrator))
                       *task-terminal-retention-limit*)
              for expired =
                (pop (task-orchestrator-terminal-identifiers orchestrator))
              do (remhash expired (task-orchestrator-jobs orchestrator))
                 (remhash expired (task-orchestrator-names orchestrator))))
      (task--condition-broadcast
       (task-orchestrator-condition-variable orchestrator))))
  nil)

(-> task-orchestrator--retain-terminal (task-orchestrator task-job) null)
(defun task-orchestrator--retain-terminal (orchestrator job)
  "Account for terminal JOB and evict the oldest excess summary."
  (with-lock-held ((task-job-lock job))
    (task-orchestrator--retain-terminal-locked orchestrator job))
  nil)

(-> task-job--lifecycle-event (task-job keyword list) list)
(defun task-job--lifecycle-event (job state result)
  "Return JOB's portable terminal lifecycle event."
  (list :id (getf (task-job-identity job) :id)
        :agent (task-job-agent-name job)
        :agent-source (task-job-agent-source job)
        :status state
        :session-file (getf result :conversation-file)
        :parent-tool-call-id (task-job-parent-call-id job)
        :index (getf (task-job-identity job) :index)
        :detached (task-job-detached-p job)))

(-> task-job--publish-terminal
    (task-job keyword list &key (:report (option string)))
    boolean)
(defun task-job--publish-terminal (job requested-state result &key report)
  "Claim and publish exactly one coherent terminal RESULT for JOB."
  (let ((*task-terminal-publication-job* job)
        (publish-p nil)
        (state requested-state)
        (final-result nil)
        (definition-summary nil)
        (event nil))
    (with-lock-held ((task-job-lock job))
      (unless (or (task-job--terminal-state-p (task-job-state job))
                  (task-job-publication-claimed-p job))
        (when (task-job-cancellation-reason job)
          (setf state :aborted))
        (setf (task-job-publication-claimed-p job) t
              publish-p t)))
    (when publish-p
      (let ((*task-terminal-publication-job* job))
        (handler-case
            (progn
              (setf final-result
                    (if (and (eq state :aborted)
                             (not (eq (getf result :status) :aborted)))
                        (task--failed-result
                         job
                         :aborted
                         (format nil "Task ~A was ~A."
                                 (getf (task-job-identity job) :id)
                                 (task-job-cancellation-reason job)))
                        (copy-list result))
                    (getf final-result :status)
                    (case state
                      (:completed :success)
                      (:aborted :aborted)
                      (otherwise :failed)))
              (setf definition-summary
                    (task--agent-definition-summary (task-job-definition job))
                    (getf final-result :agent-definition)
                    definition-summary)
              (handler-case
                  (setf final-result
                        (append
                         final-result
                         (list
                          :output-path
                          (namestring
                           (task--write-result-artifact job final-result)))))
                (error (condition)
                  (setf state :failed
                        (getf final-result :status) :failed
                        (getf final-result :error)
                        (format nil "Could not persist task artifact: ~A"
                                condition)
                        report
                        (or report
                            (bounded-string
                             (princ-to-string condition)
                             :limit *task-retained-output-limit*)))))
              (setf final-result
                    (task-job--compact-result
                     final-result
                     :artifact-available-p
                     (and (getf final-result :output-path) t))
                    report
                    (and report
                         (bounded-string
                          report :limit *task-retained-output-limit*)))
              (with-lock-held ((task-job-lock job))
                (task-job--compact-progress job state)
                (setf (task-job-state job) state
                      (task-job-publication-claimed-p job) nil
                      (task-job-result job) final-result
                      (task-job-condition-report job) report
                      (task-job-ended-at job) (get-internal-real-time)
                      (task-job-item job) (task-job--compact-item job)
                      (task-job-parent-agent job) nil
                      (task-job-command-authorization-function job) nil
                      (task-job-thread job) nil
                      (task-job-run-token job) nil
                      (task-job-deadline job) nil
                      event (task-job--lifecycle-event job state final-result)
                      (task-job-definition-summary job) definition-summary
                      (task-job-definition job) nil)
                (task-orchestrator--retain-terminal-locked
                 (task-job-orchestrator job) job)
                (task--condition-broadcast (task-job-condition-variable job)))
              (task-orchestrator-emit
               (task-job-orchestrator job) :task-subagent-lifecycle event))
          (serious-condition (condition)
            (task-job--force-terminal-failure job condition condition)))))
    publish-p))

(-> task-job--force-terminal-failure (task-job condition condition) null)
(defun task-job--force-terminal-failure
    (job execution-condition publication-condition)
  "Force JOB terminal when normal terminal publication itself fails."
  (let* ((cancellation-reason
           (with-lock-held ((task-job-lock job))
             (task-job-cancellation-reason job)))
         (state (if cancellation-reason :aborted :failed))
         (status (if cancellation-reason :aborted :failed))
         (report
           (bounded-string
            (format nil "Task failure: ~A; publication failure: ~A"
                    execution-condition publication-condition)
            :limit *task-retained-output-limit*))
         (definition-summary
           (or (task-job-definition-summary job)
               (task--agent-definition-summary (task-job-definition job))))
         (result
           (list :id (getf (task-job-identity job) :id)
                 :name (getf (task-job-identity job) :display-name)
                 :agent (task-job-agent-name job)
                 :status status
                 :output "(no retained output)"
                 :error report
                 :yielded-p nil
                 :structured-output-present-p nil
                 :agent-definition definition-summary
                 :detached (task-job-detached-p job)))
         (event nil))
    (with-lock-held ((task-job-lock job))
      (unless (task-job--terminal-state-p (task-job-state job))
        (task-job--compact-progress job state)
        (setf (task-job-state job) state
              (task-job-publication-claimed-p job) nil
              (task-job-result job) result
              (task-job-condition-report job) report
              (task-job-ended-at job) (get-internal-real-time)
              (task-job-item job) (task-job--compact-item job)
              (task-job-parent-agent job) nil
              (task-job-command-authorization-function job) nil
              (task-job-thread job) nil
              (task-job-run-token job) nil
              (task-job-deadline job) nil
              event (task-job--lifecycle-event job state result)
              (task-job-definition-summary job) definition-summary
              (task-job-definition job) nil)
        (task-orchestrator--retain-terminal-locked
         (task-job-orchestrator job) job)))
    (with-lock-held ((task-job-lock job))
      (task--condition-broadcast (task-job-condition-variable job)))
    (when event
      (task-orchestrator-emit
       (task-job-orchestrator job) :task-subagent-lifecycle event))
    nil))

(-> task-job--execute (task-job) null)
(defun task-job--execute (job)
  "Run JOB on the current reusable worker and publish one terminal result."
  (let* ((orchestrator (task-job-orchestrator job))
         (token (make-identifier))
         (started-p nil)
         (runtime-milliseconds
          (task-orchestrator-maximum-runtime-milliseconds orchestrator)))
    (with-lock-held ((task-job-lock job))
      (when (and (eq (task-job-state job) :queued)
                 (null (task-job-cancellation-reason job)))
        (let ((now (get-internal-real-time)))
          (setf (task-job-state job) :running
                (task-job-thread job) (current-thread)
                (task-job-run-token job) token
                (task-job-started-at job) now
                (task-job-deadline job)
                (and (plusp runtime-milliseconds)
                     (+ now
                        (round (* runtime-milliseconds
                                  internal-time-units-per-second)
                               1000)))
                started-p t)
          (task-job--set-progress-state job :running))))
    (when started-p
      (task-orchestrator-emit
       orchestrator
       :task-subagent-lifecycle
       (list :id (getf (task-job-identity job) :id)
             :agent (task-agent-definition-name (task-job-definition job))
             :agent-source
             (task-agent-definition-source (task-job-definition job))
             :status :started
             :parent-tool-call-id (task-job-parent-call-id job)
             :index (getf (task-job-identity job) :index)
             :detached (task-job-detached-p job)))
      (let ((*task-current-job* job)
            (*task-current-run-token* token))
        (handler-case
            (let* ((result (task-run-child job))
                   (status (getf result :status))
                   (state (cond
                            ((eq status :success) :completed)
                            ((eq status :aborted) :aborted)
                            (t :failed))))
              (task-job--publish-terminal job state result))
          (task-aborted (condition)
            (task-job--publish-terminal
             job
             :aborted
             (task--failed-result job :aborted (princ-to-string condition))
             :report (bounded-string (princ-to-string condition))))
          (error (condition)
            (task-job--publish-terminal
             job
             :failed
             (task--failed-result job :failed (princ-to-string condition))
             :report (bounded-string (princ-to-string condition))))))))
  nil)

(-> task-parent-root-conversation-identifier (agent) non-empty-string)
(defun task-parent-root-conversation-identifier (parent)
  "Return the primary conversation identifier owning PARENT's task tree."
  (if (typep parent 'task-child-agent)
      (task-job-root-conversation-identifier (task-child-agent-job parent))
      (conversation-identifier (agent-conversation parent))))

(-> task-parent-owner-identifiers (agent) list)
(defun task-parent-owner-identifiers (parent)
  "Return the task identifiers authorized to inspect PARENT's descendants."
  (if (typep parent 'task-child-agent)
      (let ((job (task-child-agent-job parent)))
        (append (task-job-owner-identifiers job)
                (list (getf (task-job-identity job) :id))))
      nil))

(-> task-orchestrator--live-job-count (task-orchestrator) (integer 0))
(defun task-orchestrator--live-job-count (orchestrator)
  "Return queued, running, and finalizing jobs while the lock is held."
  (task-orchestrator-live-count orchestrator))

(defun task-orchestrator-start-jobs
    (orchestrator parent-agent entries parent-call-id
     command-authorization-function)
  "Atomically admit ENTRIES and return jobs plus nested synchronous inline jobs."
  (when (and (typep parent-agent 'task-child-agent)
             (not *task-admission-parent-locked-p*))
    (let ((parent-job (task-child-agent-job parent-agent)))
      (with-lock-held ((task-job-lock parent-job))
        (when (or (task-job-cancellation-reason parent-job)
                  (task-job--terminal-state-p (task-job-state parent-job)))
          (error 'task-aborted
                 :message
                 (format nil "Task ~A was cancelled before child admission."
                         (getf (task-job-identity parent-job) :id))
                 :reason
                 (or (task-job-cancellation-reason parent-job) :shutdown)))
        (let ((*task-admission-parent-locked-p* t))
          (return-from task-orchestrator-start-jobs
            (task-orchestrator-start-jobs
             orchestrator parent-agent entries parent-call-id
             command-authorization-function))))))
  (let ((jobs nil)
        (inline nil)
        (queued nil)
        (reserved-identifiers nil)
        (count (length entries))
        (root-conversation-identifier
          (task-parent-root-conversation-identifier parent-agent))
        (owner-identifiers (task-parent-owner-identifiers parent-agent)))
    (when (> count *task-maximum-batch-size*)
      (error 'task-error
             :message
             (format nil "A task batch may contain at most ~D children."
                     *task-maximum-batch-size*)
             :tool-name "task.run"))
    (with-lock-held ((task-orchestrator-lock orchestrator))
      (when (or (task-orchestrator-shutdown-p orchestrator)
                (not (eq (task-orchestrator-lifecycle-state orchestrator)
                         :open)))
        (error 'task-error
               :message "The task runtime is shutting down."
               :tool-name "task.run"))
      (when (> (+ (task-orchestrator--live-job-count orchestrator) count)
               *task-maximum-live-jobs*)
        (error 'task-error
               :message
               (format nil "The task runtime admits at most ~D live jobs."
                       *task-maximum-live-jobs*)
               :tool-name "task.run"))
      (handler-case
          (dolist (entry entries)
            (let* ((definition (getf entry :definition))
                   (item (getf entry :item))
                   (detached-p (getf entry :detached))
                   (identity
                     (task-orchestrator--create-identity
                      orchestrator
                      (getf item :name)
                      (task-agent-definition-name definition))))
              (push (getf identity :id) reserved-identifiers)
              (let ((job
                      (make-instance
                       'task-job
                       :orchestrator orchestrator
                       :identity identity
                       :execution-identifier (make-identifier)
                       :definition definition
                       :item item
                       :parent-agent parent-agent
                       :root-conversation-identifier
                       root-conversation-identifier
                       :owner-identifiers owner-identifiers
                       :parent-call-id parent-call-id
                       :detached-p detached-p
                       :command-authorization-function
                       command-authorization-function)))
                (push job jobs)
                (if (and (typep parent-agent 'task-child-agent)
                         (not detached-p))
                    (push job inline)
                    (push job queued)))))
        (error (condition)
          (dolist (identifier reserved-identifiers)
            (remhash identifier (task-orchestrator-names orchestrator)))
          (error condition)))
      (setf jobs (nreverse jobs)
            inline (nreverse inline)
            queued (nreverse queued)
            (task-orchestrator-queue orchestrator)
            (nconc (task-orchestrator-queue orchestrator) queued))
      (dolist (job jobs)
        (setf (gethash (getf (task-job-identity job) :id)
                       (task-orchestrator-jobs orchestrator))
              job))
      (incf (task-orchestrator-live-count orchestrator) count)
      (task--condition-broadcast
       (task-orchestrator-condition-variable orchestrator)))
    (values jobs inline)))

(defun task-orchestrator-start-job
    (orchestrator
     &key parent-agent definition item detached-p parent-call-id
       command-authorization-function)
  "Admit one JOB through the atomic scheduler admission path."
  (multiple-value-bind (jobs inline)
      (task-orchestrator-start-jobs
       orchestrator
       parent-agent
       (list (list :definition definition
                   :item item
                   :detached detached-p))
       parent-call-id
       command-authorization-function)
    (dolist (job inline)
      (task-job--execute job))
    (first jobs)))
