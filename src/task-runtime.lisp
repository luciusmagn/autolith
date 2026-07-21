(in-package #:autolith)

;;;; -- Task Runtime --

(-> task--environment-integer
    (string integer &key (:minimum (option integer)) (:maximum (option integer)))
    integer)
(defun task--environment-integer (name fallback &key minimum maximum)
  "Return bounded integer environment NAME or FALLBACK."
  (let ((value (uiop/os:getenv name)))
    (if (non-empty-string-p value)
        (handler-case
            (let ((parsed (parse-integer value :junk-allowed nil)))
              (if (and (integerp parsed)
                       (or (null minimum) (>= parsed minimum)))
                  (if maximum (min parsed maximum) parsed)
                  fallback))
          (error nil fallback))
        fallback)))

(-> task-orchestrator-create () task-orchestrator)
(defun task-orchestrator-create ()
  "Create an orchestrator from the current task environment settings."
  (make-instance 'task-orchestrator :maximum-concurrency
                 (task--environment-integer "AUTOLITH_TASK_MAX_CONCURRENCY"
                                            +task-default-maximum-concurrency+
                                            :minimum 1
                                            :maximum
                                            +task-maximum-concurrency+)
                 :maximum-depth
                 (task--environment-integer "AUTOLITH_TASK_MAX_DEPTH"
                                            +task-default-maximum-depth+
                                            :minimum 1)
                 :maximum-runtime-milliseconds
                 (task--environment-integer
                  "AUTOLITH_TASK_MAX_RUNTIME_MS"
                  +task-default-maximum-runtime-milliseconds+
                  :minimum 0)))

(-> task-orchestrator--reap-dead-threads-locked
    (task-orchestrator)
    null)
(defun task-orchestrator--reap-dead-threads-locked (orchestrator)
  "Forget dead runtime threads and complete an ownerless shutdown."
  (let ((owner (task-orchestrator-close-owner orchestrator)))
    (when (and owner (not (thread-alive-p owner)))
      (setf (task-orchestrator-close-owner orchestrator) nil)))
  (setf (task-orchestrator-worker-threads orchestrator)
        (remove-if-not #'thread-alive-p
                       (task-orchestrator-worker-threads orchestrator)))
  (let ((monitor (task-orchestrator-monitor-thread orchestrator)))
    (when (and monitor (not (thread-alive-p monitor)))
      (setf (task-orchestrator-monitor-thread orchestrator) nil)))
  (when (and (eq (task-orchestrator-lifecycle-state orchestrator) :closing)
             (null (task-orchestrator-close-owner orchestrator))
             (null (task-orchestrator-worker-threads orchestrator))
             (null (task-orchestrator-monitor-thread orchestrator)))
    (setf (task-orchestrator-lifecycle-state orchestrator) :closed
          (task-orchestrator-active-count orchestrator) 0)
    (task--condition-broadcast
     (task-orchestrator-condition-variable orchestrator)))
  nil)

(-> task-orchestrator-refresh (task-orchestrator) task-orchestrator)
(defun task-orchestrator-refresh (orchestrator)
  "Apply current limits to ORCHESTRATOR and ensure its reusable workers."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (task-orchestrator--reap-dead-threads-locked orchestrator)
    (when (eq (task-orchestrator-lifecycle-state orchestrator) :closing)
      (error 'task-error
             :message "The task runtime is still shutting down."
             :tool-name "task.run"))
    (when (eq (task-orchestrator-lifecycle-state orchestrator) :closed)
      (setf (task-orchestrator-lifecycle-state orchestrator) :open))
    (setf (task-orchestrator-maximum-concurrency orchestrator)
          (task--environment-integer "AUTOLITH_TASK_MAX_CONCURRENCY"
                                     +task-default-maximum-concurrency+
                                     :minimum 1
                                     :maximum +task-maximum-concurrency+)
          (task-orchestrator-maximum-depth orchestrator)
          (task--environment-integer "AUTOLITH_TASK_MAX_DEPTH"
                                     +task-default-maximum-depth+ :minimum 1)
          (task-orchestrator-maximum-runtime-milliseconds orchestrator)
          (task--environment-integer
           "AUTOLITH_TASK_MAX_RUNTIME_MS"
           +task-default-maximum-runtime-milliseconds+
           :minimum 0)
          (task-orchestrator-shutdown-p orchestrator) nil)
    (task--condition-broadcast
     (task-orchestrator-condition-variable orchestrator)))
  (task-orchestrator--ensure-workers orchestrator)
  (task-orchestrator--ensure-monitor orchestrator)
  orchestrator)

(defun task-orchestrator-add-listener (orchestrator listener)
  "Register LISTENER for portable task events and return it."
  (check-type listener function)
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (pushnew listener (task-orchestrator-listeners orchestrator) :test #'eq))
  listener)

(defun task-orchestrator-remove-listener (orchestrator listener)
  "Remove LISTENER from ORCHESTRATOR."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (setf (task-orchestrator-listeners orchestrator)
          (remove listener (task-orchestrator-listeners orchestrator) :test
                  #'eq)))
  nil)

(defun task-orchestrator-emit (orchestrator channel payload)
  "Deliver portable CHANNEL and PAYLOAD to a snapshot of listeners."
  (let ((listeners
         (with-lock-held ((task-orchestrator-lock orchestrator))
           (copy-list (task-orchestrator-listeners orchestrator)))))
    (dolist (listener listeners)
      (handler-case
          (funcall listener channel payload)
        (serious-condition ()
          nil))))
  nil)

(defun task--identifier-fragment (value)
  "Return VALUE normalized for child identifiers and artifact names."
  (let* ((unbounded (string-downcase (task--trim (or value ""))))
         (text (subseq unbounded
                       0
                       (min (length unbounded)
                            +task-identifier-maximum-characters+)))
         (mapped
          (map 'string
               (lambda (character)
                 (if (or (alphanumericp character)
                         (member character '(#\HYPHEN-MINUS #\LOW_LINE) :test
                                 #'char=))
                     character
                     #\HYPHEN-MINUS))
               text))
         (trimmed (string-trim '(#\HYPHEN-MINUS) mapped)))
    (and (non-empty-string-p trimmed) trimmed)))

(-> task-orchestrator--create-identity
    (task-orchestrator (option string) string)
    list)
(defun task-orchestrator--create-identity
    (orchestrator requested-name agent-type)
  "Reserve a child identity while ORCHESTRATOR's lock is held."
  (incf (task-orchestrator-next-index orchestrator))
  (let* ((index (task-orchestrator-next-index orchestrator))
         (adjectives
          #("amber" "brisk" "calm" "clear" "keen" "quiet" "rapid" "steady"
            "vivid" "wise"))
         (nouns
          #("badger" "falcon" "heron" "lynx" "otter" "raven" "sparrow" "tern"
            "wolf" "wren"))
         (generated
          (format nil "~A-~A"
                  (aref adjectives (mod (1- index) (length adjectives)))
                  (aref nouns
                        (mod (floor (1- index) (length adjectives))
                             (length nouns)))))
         (base (or (task--identifier-fragment requested-name) generated))
         (suffix-text (format nil "-~D" index))
         (base-limit
           (max 0
                (- +task-identifier-maximum-characters+
                   (length suffix-text))))
         (candidate
           (concatenate 'string
                        (subseq base 0 (min (length base) base-limit))
                        suffix-text)))
    (setf (gethash candidate (task-orchestrator-names orchestrator)) t)
    (list :id candidate
          :display-name
          (if requested-name
              (subseq requested-name
                      0
                      (min (length requested-name)
                           +task-identifier-maximum-characters+))
              candidate)
          :agent-type agent-type
          :index index)))

(-> task-orchestrator-create-identity
    (task-orchestrator (option string) string)
    list)
(defun task-orchestrator-create-identity
    (orchestrator requested-name agent-type)
  "Reserve and return a stable unique child identity plist."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (task-orchestrator--create-identity orchestrator requested-name agent-type)))

(-> task-orchestrator--worker-loop (task-orchestrator) null)
(defun task-orchestrator--worker-loop (orchestrator)
  "Run queued jobs on one reusable worker until ORCHESTRATOR closes."
  (loop
    (let ((job nil))
      (with-lock-held ((task-orchestrator-lock orchestrator))
        (loop
          (when (task-orchestrator-shutdown-p orchestrator)
            (return-from task-orchestrator--worker-loop nil))
          (when (and (task-orchestrator-queue orchestrator)
                     (< (task-orchestrator-active-count orchestrator)
                        (task-orchestrator-maximum-concurrency orchestrator)))
            (setf job (pop (task-orchestrator-queue orchestrator)))
            (incf (task-orchestrator-active-count orchestrator))
            (return))
          (condition-wait
           (task-orchestrator-condition-variable orchestrator)
           (task-orchestrator-lock orchestrator))))
      (unwind-protect
           (handler-case
               (task-job--execute job)
             (serious-condition (condition)
               (handler-case
                   (unless (task-job-terminal-p job)
                     (unless
                         (task-job--publish-terminal
                          job
                          :failed
                          (task--failed-result job :failed
                                               (princ-to-string condition))
                          :report
                          (bounded-string (princ-to-string condition)))
                       (unless (task-job-terminal-p job)
                         (task-job--force-terminal-failure
                          job condition condition))))
                 (serious-condition (publication-condition)
                   (task-job--force-terminal-failure
                    job condition publication-condition)))))
        (with-lock-held ((task-orchestrator-lock orchestrator))
          (decf (task-orchestrator-active-count orchestrator))
          (task--condition-broadcast
           (task-orchestrator-condition-variable orchestrator)))))))

(-> task-orchestrator--ensure-workers (task-orchestrator) null)
(defun task-orchestrator--ensure-workers (orchestrator)
  "Ensure ORCHESTRATOR has enough reusable scheduler workers."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (setf (task-orchestrator-worker-threads orchestrator)
          (remove-if-not #'thread-alive-p
                         (task-orchestrator-worker-threads orchestrator)))
    (when (eq (task-orchestrator-lifecycle-state orchestrator) :open)
      (loop repeat (max 0
                        (- (task-orchestrator-maximum-concurrency orchestrator)
                           (length
                            (task-orchestrator-worker-threads orchestrator))))
            for index from
              (length (task-orchestrator-worker-threads orchestrator))
            for thread =
              (make-thread
               (lambda () (task-orchestrator--worker-loop orchestrator))
               :name (format nil "Autolith task worker ~D" (1+ index)))
            do (push thread (task-orchestrator-worker-threads orchestrator)))
      (task--condition-broadcast
       (task-orchestrator-condition-variable orchestrator))))
  nil)

(-> task-orchestrator--monitor-loop (task-orchestrator) null)
(defun task-orchestrator--monitor-loop (orchestrator)
  "Cancel running jobs whose runtime deadlines have elapsed."
  (loop
    (let ((expired nil)
          (jobs nil))
      (with-lock-held ((task-orchestrator-lock orchestrator))
        (when (task-orchestrator-shutdown-p orchestrator)
          (return-from task-orchestrator--monitor-loop nil))
        (setf jobs
              (loop for job being the hash-values of
                      (task-orchestrator-jobs orchestrator)
                    collect job)))
      (let ((now (get-internal-real-time)))
        (dolist (job jobs)
          (with-lock-held ((task-job-lock job))
            (when (and (eq (task-job-state job) :running)
                       (task-job-deadline job)
                       (>= now (task-job-deadline job)))
              (push job expired)))))
      (dolist (job expired)
        (task-job-cancel job :timeout))
      (with-lock-held ((task-orchestrator-lock orchestrator))
        (unless (task-orchestrator-shutdown-p orchestrator)
          (condition-wait
           (task-orchestrator-condition-variable orchestrator)
           (task-orchestrator-lock orchestrator)
           :timeout 0.1))))))

(-> task-orchestrator--ensure-monitor (task-orchestrator) null)
(defun task-orchestrator--ensure-monitor (orchestrator)
  "Ensure ORCHESTRATOR has one deadline monitor when runtime caps are enabled."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (let ((monitor (task-orchestrator-monitor-thread orchestrator)))
      (when (and (plusp
                  (task-orchestrator-maximum-runtime-milliseconds orchestrator))
                 (eq (task-orchestrator-lifecycle-state orchestrator) :open)
                 (not (and monitor (thread-alive-p monitor))))
        (setf (task-orchestrator-monitor-thread orchestrator)
              (make-thread
               (lambda () (task-orchestrator--monitor-loop orchestrator))
               :name "Autolith task deadline monitor")))))
  nil)

(-> task-orchestrator-close (task-orchestrator) boolean)
(defun task-orchestrator-close (orchestrator)
  "Cancel all jobs, stop reusable threads, and report complete shutdown."
  (let ((owner-p nil)
        (jobs nil)
        (threads nil)
        (deadline (+ (get-internal-real-time)
                     (* +task-shutdown-timeout-seconds+
                        internal-time-units-per-second))))
    (with-lock-held ((task-orchestrator-lock orchestrator))
      (task-orchestrator--reap-dead-threads-locked orchestrator)
      (case (task-orchestrator-lifecycle-state orchestrator)
        (:closed
         (return-from task-orchestrator-close t))
        (:closing
         (let ((owner (task-orchestrator-close-owner orchestrator)))
           (unless (and owner
                        (not (eq owner (current-thread)))
                        (thread-alive-p owner))
             (setf owner-p t
                   (task-orchestrator-close-owner orchestrator)
                   (current-thread)
                   jobs
                   (loop for job being the hash-values of
                           (task-orchestrator-jobs orchestrator)
                         collect job)
                   threads
                   (remove nil
                           (cons
                            (task-orchestrator-monitor-thread orchestrator)
                            (copy-list
                             (task-orchestrator-worker-threads
                              orchestrator))))))))
        (otherwise
         (setf owner-p t
               (task-orchestrator-close-owner orchestrator) (current-thread)
               (task-orchestrator-lifecycle-state orchestrator) :closing
               (task-orchestrator-shutdown-p orchestrator) t
               (task-orchestrator-queue orchestrator) nil
               jobs
               (loop for job being the hash-values of
                       (task-orchestrator-jobs orchestrator)
                     collect job)
               threads
               (remove nil
                       (cons (task-orchestrator-monitor-thread orchestrator)
                             (copy-list
                              (task-orchestrator-worker-threads
                               orchestrator)))))
         (task--condition-broadcast
          (task-orchestrator-condition-variable orchestrator)))))
    (unless owner-p
      (with-lock-held ((task-orchestrator-lock orchestrator))
        (loop while (and (eq (task-orchestrator-lifecycle-state orchestrator)
                             :closing)
                         (task-orchestrator-close-owner orchestrator)
                         (< (get-internal-real-time) deadline))
              for remaining =
                (/ (max 0 (- deadline (get-internal-real-time)))
                   internal-time-units-per-second)
              do (condition-wait
                  (task-orchestrator-condition-variable orchestrator)
                  (task-orchestrator-lock orchestrator)
                  :timeout remaining))
        (return-from task-orchestrator-close
          (eq (task-orchestrator-lifecycle-state orchestrator) :closed))))
    (dolist (job jobs)
      (task-job-cancel job :shutdown))
    (loop
      for live = (remove-if-not
                  (lambda (thread)
                    (and (not (eq thread (current-thread)))
                         (thread-alive-p thread)))
                  threads)
      until (or (null live)
                (>= (get-internal-real-time) deadline))
      do (sleep 0.01))
    (dolist (thread threads)
      (when (and (not (eq thread (current-thread)))
                 (not (thread-alive-p thread)))
        (join-thread thread)))
    (let ((live
            (remove-if-not
             (lambda (thread)
               (and (not (eq thread (current-thread)))
                    (thread-alive-p thread)))
             threads)))
      (with-lock-held ((task-orchestrator-lock orchestrator))
        (setf (task-orchestrator-worker-threads orchestrator)
              (intersection live
                            (task-orchestrator-worker-threads orchestrator)
                            :test #'eq)
              (task-orchestrator-monitor-thread orchestrator)
              (and (member (task-orchestrator-monitor-thread orchestrator)
                           live
                           :test #'eq)
                   (task-orchestrator-monitor-thread orchestrator))
              (task-orchestrator-lifecycle-state orchestrator)
              (if live :closing :closed)
              (task-orchestrator-close-owner orchestrator) nil
              (task-orchestrator-active-count orchestrator)
              (if live (task-orchestrator-active-count orchestrator) 0))
        (task--condition-broadcast
         (task-orchestrator-condition-variable orchestrator)))
      (null live))))

(-> task-orchestrator-detach (task-orchestrator) null)
(defun task-orchestrator-detach (orchestrator)
  "Remove closed runtime state before an image save or registry replacement."
  (with-lock-held ((task-orchestrator-lock orchestrator))
    (unless (eq (task-orchestrator-lifecycle-state orchestrator) :closed)
      (error 'task-error
             :message "Task runtime must close before it can detach."
             :tool-name "task.run"))
    (when (or (some #'thread-alive-p
                    (task-orchestrator-worker-threads orchestrator))
              (let ((monitor (task-orchestrator-monitor-thread orchestrator)))
                (and monitor (thread-alive-p monitor))))
      (error 'task-error
             :message "Task runtime cannot detach while its threads are alive."
             :tool-name "task.run"))
    (setf (task-orchestrator-worker-threads orchestrator) nil
          (task-orchestrator-monitor-thread orchestrator) nil
          (task-orchestrator-queue orchestrator) nil
          (task-orchestrator-close-owner orchestrator) nil
          (task-orchestrator-active-count orchestrator) 0
          (task-orchestrator-live-count orchestrator) 0
          (task-orchestrator-terminal-identifiers orchestrator) nil
          (task-orchestrator-listeners orchestrator) nil)
    (clrhash (task-orchestrator-jobs orchestrator))
    (clrhash (task-orchestrator-names orchestrator)))
  nil)

(defmethod tool-runtime-identity ((tool task-orchestrator-tool))
  "Return the scheduler shared by task and job tools."
  (task-orchestrator-tool-orchestrator tool))

(defmethod tool-runtime-close ((tool task-orchestrator-tool))
  "Stop TOOL's shared jobs and reusable scheduler threads."
  (unless (task-orchestrator-close
           (task-orchestrator-tool-orchestrator tool))
    (error 'task-error
           :message "Task workers did not stop before the shutdown deadline."
           :tool-name (tool-canonical-name tool)))
  nil)

(defmethod tool-runtime-detach ((tool task-orchestrator-tool))
  "Remove TOOL's closed shared scheduler graph before image saving."
  (task-orchestrator-detach (task-orchestrator-tool-orchestrator tool)))

(defun task--milliseconds-between (start end)
  "Return elapsed milliseconds between internal real times START and END."
  (round (* 1000 (- end start)) internal-time-units-per-second))

(defun task-progress-append-output (progress text)
  "Append streamed TEXT while retaining only a bounded tail."
  (with-lock-held ((task-progress-lock progress))
    (let* ((combined
            (concatenate 'string (task-progress-output-tail progress) text))
           (start (max 0 (- (length combined) +task-progress-output-limit+))))
      (setf (task-progress-output-tail progress) (subseq combined start)
            (task-progress-updated-at progress) (get-internal-real-time))))
  nil)

(defun task-progress-note-status (job status details)
  "Update JOB's normalized progress from one child observer STATUS event."
  (let ((progress (task-job-progress job))
        (event nil))
    (with-lock-held ((task-progress-lock progress))
      (case status
        (:provider-request-started
         (setf (task-progress-request-count progress)
               (or (getf details :request-number)
                   (1+ (task-progress-request-count progress)))))
        (:provider-request-completed
         (setf (task-progress-usage progress) (getf details :usage)))
        (:tool-call-started
         (setf (task-progress-current-tool progress) (getf details :tool)))
        (:tool-call-completed
         (let ((tool (getf details :tool)))
           (when tool
             (push tool (task-progress-recent-tools progress))
             (setf (task-progress-recent-tools progress)
                   (subseq (task-progress-recent-tools progress) 0
                           (min 8
                                (length
                                 (task-progress-recent-tools progress)))))))
         (setf (task-progress-current-tool progress) nil)))
      (setf (task-progress-updated-at progress) (get-internal-real-time)
            event
            (list :id (getf (task-job-identity job) :id)
                  :status (task-progress-status progress)
                  :current-tool (task-progress-current-tool progress)
                  :request-count (task-progress-request-count progress))))
    (task-orchestrator-emit (task-job-orchestrator job) :task-subagent-progress
                            event)
    (let ((reason
            (with-lock-held ((task-job-lock job))
              (and (not (task-job--terminal-state-p (task-job-state job)))
                   (task-job-cancellation-reason job)))))
      (when reason
        (error 'task-aborted
               :message
               (format nil "Task ~A was ~A."
                       (getf (task-job-identity job) :id)
                       reason)
               :reason reason))))
  nil)

(-> task-job--terminal-state-p (keyword) boolean)
(defun task-job--terminal-state-p (state)
  "Return true when STATE is a published terminal task state."
  (not (null (member state '(:completed :failed :aborted) :test #'eq))))

(-> task-job-agent-name (task-job) non-empty-string)
(defun task-job-agent-name (job)
  "Return JOB's live or retained child role name."
  (let ((definition (task-job-definition job)))
    (if definition
        (task-agent-definition-name definition)
        (getf (task-job-definition-summary job) :name))))

(-> task-job-agent-source (task-job) keyword)
(defun task-job-agent-source (job)
  "Return JOB's live or retained child role source."
  (let ((definition (task-job-definition job)))
    (if definition
        (task-agent-definition-source definition)
        (getf (task-job-definition-summary job) :source))))

(-> task-progress--snapshot
    (task-job &key (:parent t) (:result t) (:ended-at t))
    list)
(defun task-progress--snapshot (job &key parent result ended-at)
  "Return JOB progress using lifecycle values captured under the job lock."
  (let ((progress (task-job-progress job)))
    (with-lock-held ((task-progress-lock progress))
      (list :id (getf (task-job-identity job) :id)
            :agent (task-job-agent-name job)
            :status (task-progress-status progress)
            :current-tool (task-progress-current-tool progress)
            :recent-tools
            (reverse (copy-list (task-progress-recent-tools progress)))
            :recent-output (task-progress-output-tail progress)
            :request-count (task-progress-request-count progress)
            :usage (copy-tree (task-progress-usage progress))
            :duration-ms
            (and (task-progress-started-at progress)
                 (task--milliseconds-between
                  (task-progress-started-at progress)
                  (or ended-at (get-internal-real-time))))
            :model
            (or (getf result :model)
                (and parent
                     (configuration-model
                      (task-configuration-for-definition
                       (agent-configuration parent)
                       (task-job-definition job)))))))))

(-> task-progress-snapshot (task-job) list)
(defun task-progress-snapshot (job)
  "Return a coherent portable snapshot of JOB's current progress."
  (with-lock-held ((task-job-lock job))
    (task-progress--snapshot job
                             :parent (task-job-parent-agent job)
                             :result (task-job-result job)
                             :ended-at (task-job-ended-at job))))

(-> task-job-terminal-p (task-job) boolean)
(defun task-job-terminal-p (job)
  "Return true when JOB cannot make another state transition."
  (with-lock-held ((task-job-lock job))
    (task-job--terminal-state-p (task-job-state job))))

(-> task-job--snapshot-locked (task-job) list)
(defun task-job--snapshot-locked (job)
  "Return JOB's snapshot while its lifecycle lock is held."
  (let ((result (copy-tree (task-job-result job))))
    (list :job-id (getf (task-job-identity job) :id)
          :execution-id (task-job-execution-identifier job)
          :type :task
          :state (task-job-state job)
          :detached (task-job-detached-p job)
          :agent (task-job-agent-name job)
          :assignment
          (bounded-string (getf (task-job-item job) :task)
                          :limit +task-retained-assignment-limit+)
          :progress
          (task-progress--snapshot job
                                   :parent (task-job-parent-agent job)
                                   :result result
                                   :ended-at (task-job-ended-at job))
          :result result
          :cancellation-reason (task-job-cancellation-reason job)
          :condition-report (task-job-condition-report job))))

(-> task-job-snapshot (task-job) list)
(defun task-job-snapshot (job)
  "Return JOB's coherent portable lifecycle, progress, and result snapshot."
  (with-lock-held ((task-job-lock job))
    (task-job--snapshot-locked job)))

(defun task-orchestrator-find-job (orchestrator identifier)
  "Return IDENTIFIER's job or signal a typed task error."
  (let ((job
         (with-lock-held ((task-orchestrator-lock orchestrator))
           (gethash identifier (task-orchestrator-jobs orchestrator)))))
    (or job
        (error 'task-error :message
               (format nil "No task job named ~A exists." identifier)
               :tool-name "job.get" :task-id identifier))))

(defun task-orchestrator-list-jobs (orchestrator)
  "Return all jobs sorted by child index."
  (let ((jobs nil))
    (with-lock-held ((task-orchestrator-lock orchestrator))
      (maphash
       (lambda (identifier job) (declare (ignore identifier)) (push job jobs))
       (task-orchestrator-jobs orchestrator)))
    (sort jobs #'< :key (lambda (job) (getf (task-job-identity job) :index)))))

(-> task-job-visible-to-agent-p (task-job agent) boolean)
(defun task-job-visible-to-agent-p (job viewer)
  "Return true when VIEWER owns JOB through conversation or task ancestry."
  (not
   (null
    (if (typep viewer 'task-child-agent)
        (member (getf (task-job-identity
                       (task-child-agent-job viewer))
                      :id)
                (task-job-owner-identifiers job)
                :test #'string=)
        (string=
         (task-job-root-conversation-identifier job)
         (conversation-identifier (agent-conversation viewer)))))))

(-> task-orchestrator-list-visible-jobs
    (task-orchestrator agent)
    list)
(defun task-orchestrator-list-visible-jobs (orchestrator viewer)
  "Return jobs VIEWER may inspect, ordered by child index."
  (remove-if-not
   (lambda (job) (task-job-visible-to-agent-p job viewer))
   (task-orchestrator-list-jobs orchestrator)))

(-> task-orchestrator-find-visible-job
    (task-orchestrator string agent string)
    task-job)
(defun task-orchestrator-find-visible-job
    (orchestrator identifier viewer tool-name)
  "Return VIEWER's visible IDENTIFIER or signal a non-disclosing task error."
  (let ((job
         (with-lock-held ((task-orchestrator-lock orchestrator))
           (gethash identifier (task-orchestrator-jobs orchestrator)))))
    (if (and job (task-job-visible-to-agent-p job viewer))
        job
        (error 'task-error
               :message (format nil "No visible task job named ~A exists."
                                identifier)
               :tool-name tool-name
               :task-id identifier))))

(-> task-job--request-cancellation (task-job keyword) boolean)
(defun task-job--request-cancellation (job reason)
  "Request first-writer cancellation REASON for JOB without walking descendants."
  (let ((thread nil)
        (run-token nil)
        (queued-p nil)
        (cancel-p nil)
        (orchestrator (task-job-orchestrator job)))
    (with-lock-held ((task-job-lock job))
      (unless (or (task-job--terminal-state-p (task-job-state job))
                  (task-job-publication-claimed-p job)
                  (task-job-cancellation-reason job))
        (setf (task-job-cancellation-reason job) reason
              thread (task-job-thread job)
              run-token (task-job-run-token job)
              queued-p (eq (task-job-state job) :queued)
              cancel-p t)))
    (when cancel-p
      (with-lock-held ((task-orchestrator-lock orchestrator))
        (setf (task-orchestrator-queue orchestrator)
              (remove job (task-orchestrator-queue orchestrator) :test #'eq))
        (task--condition-broadcast
         (task-orchestrator-condition-variable orchestrator)))
      (when queued-p
        (task-job--publish-terminal
         job
         :aborted
         (task--failed-result
          job
          :aborted
          (format nil "Task ~A was ~A before it started."
                  (getf (task-job-identity job) :id)
                  reason)))))
    (when (and cancel-p thread run-token (thread-alive-p thread))
      (interrupt-thread thread
                        (lambda ()
                          (when (and (eq *task-current-job* job)
                                     (not (eq *task-terminal-publication-job*
                                              job))
                                     (stringp *task-current-run-token*)
                                     (string=
                                      *task-current-run-token*
                                      run-token))
                            (error 'task-aborted
                                   :message
                                   (format nil "Task ~A was ~A."
                                           (getf (task-job-identity job) :id)
                                           reason)
                                   :reason reason)))))
    cancel-p))

(-> task-job-cancel (task-job keyword) (values boolean list))
(defun task-job-cancel (job reason)
  "Cancel JOB and every retained live descendant, returning accepted identities."
  (let* ((orchestrator (task-job-orchestrator job))
         (identifier (getf (task-job-identity job) :id))
         (accepted-p (task-job--request-cancellation job reason))
         (accepted-descendants nil))
    (loop
      with accepted-this-pass = nil
      do
         (setf accepted-this-pass nil)
         (let ((descendants
                 (with-lock-held ((task-orchestrator-lock orchestrator))
                   (sort
                    (loop for candidate being the hash-values of
                            (task-orchestrator-jobs orchestrator)
                          when (member identifier
                                       (task-job-owner-identifiers candidate)
                                       :test #'string=)
                            collect candidate)
                    #'<
                    :key (lambda (candidate)
                           (getf (task-job-identity candidate) :index))))))
           (dolist (descendant descendants)
             (when (task-job--request-cancellation descendant reason)
               (let ((descendant-identifier
                       (getf (task-job-identity descendant) :id)))
                 (pushnew descendant-identifier accepted-descendants
                          :test #'string=)
                 (setf accepted-this-pass t)))))
      while accepted-this-pass)
    (values accepted-p
            (sort accepted-descendants #'string<))))

(-> task-job-help-join (task-job) boolean)
(defun task-job-help-join (job)
  "Run queued JOB inline when a child waiter would otherwise occupy a worker."
  (let ((claimed-p nil)
        (orchestrator (task-job-orchestrator job)))
    (with-lock-held ((task-job-lock job))
      (when (and (eq (task-job-state job) :queued)
                 (null (task-job-cancellation-reason job))
                 (not (task-job-publication-claimed-p job)))
        (with-lock-held ((task-orchestrator-lock orchestrator))
          (when (member job (task-orchestrator-queue orchestrator) :test #'eq)
            (setf (task-orchestrator-queue orchestrator)
                  (remove job
                          (task-orchestrator-queue orchestrator)
                          :test #'eq)
                  claimed-p t)
            (task--condition-broadcast
             (task-orchestrator-condition-variable orchestrator))))))
    (when claimed-p
      (task-job--execute job))
    claimed-p))

(-> task-job-await
    (task-job (option (real 0)))
    (values list boolean))
(defun task-job-await (job timeout-seconds)
  "Wait up to TIMEOUT-SECONDS and return a snapshot plus terminal flag."
  (let ((deadline
         (and timeout-seconds
              (+ (get-internal-real-time)
                 (* timeout-seconds internal-time-units-per-second)))))
    (with-lock-held ((task-job-lock job))
      (loop until (task-job--terminal-state-p (task-job-state job))
            for now = (get-internal-real-time)
            for remaining =
              (and deadline
                   (/ (max 0 (- deadline now))
                      internal-time-units-per-second))
            when (and deadline (<= remaining 0))
              return nil
            do (condition-wait
                (task-job-condition-variable job)
                (task-job-lock job)
                :timeout remaining))))
  (let ((snapshot (task-job-snapshot job)))
    (values snapshot
            (task-job--terminal-state-p (getf snapshot :state)))))
