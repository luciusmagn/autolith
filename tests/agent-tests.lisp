(in-package #:frob)

;;;; -- Scripted Agent Boundary --

(defclass scripted-provider (model-provider)
  ((results
    :initarg :results
    :accessor scripted-provider-results
    :type list
    :documentation "The provider results returned in request order.")
   (input-counts
    :initform nil
    :accessor scripted-provider-input-counts
    :type list
    :documentation "Conversation input lengths observed before each request.")
   (turn-states
    :initform nil
    :accessor scripted-provider-turn-states
    :type list
    :documentation "Request-local turn states observed before each request."))
  (:documentation "A deterministic provider for exercising repeated agent rounds."))

(defmethod provider-stream-turn
    ((provider scripted-provider)
     (conversation conversation)
     (tool-namespaces vector)
     (event-callback function))
  "Return PROVIDER's next scripted result after recording request state."
  (declare (ignore tool-namespaces))
  (push (length (conversation-input-items conversation))
        (scripted-provider-input-counts provider))
  (push (conversation-turn-state conversation)
        (scripted-provider-turn-states provider))
  (let ((result (pop (scripted-provider-results provider))))
    (unless result
      (error "The scripted provider has no remaining result."))
    (when (typep result 'serious-condition)
      (error result))
    (funcall event-callback
             (make-instance 'assistant-delta-event :text "delta"))
    result))

(defclass agent-test-echo-tool (tool)
  ()
  (:documentation "Return one required string to the scripted agent provider."))

(defmethod tool-execute ((tool agent-test-echo-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return the required test value without external effects."
  (declare (ignore tool context))
  (tool-success
   (format nil "echo: ~A"
           (tool-argument arguments "value" :required t))))

(-> agent-test-registry () tool-registry)
(defun agent-test-registry ()
  "Return a registry containing the deterministic echo tool."
  (let ((registry (make-instance 'tool-registry)))
    (tool-registry-register
     registry
     (make-instance
      'agent-test-echo-tool
      :namespace "test"
      :name "echo"
      :description "Echo a test string."
      :parameters
      (tool-object-schema
       (json-object
        "value" (tool-string-property "The value to echo."))
       '("value"))))
    registry))

(-> agent-test-call
    (&key
     (:call-id (option string))
     (:namespace string)
     (:name string)
     (:arguments string))
    json-object)
(defun agent-test-call
    (&key call-id (namespace "test") (name "echo") (arguments "{}"))
  "Return a scripted function call with optional CALL-ID."
  (let ((call (json-object
               "type" "function_call"
               "namespace" namespace
               "name" name
               "arguments" arguments)))
    (when call-id
      (setf (gethash "call_id" call) call-id))
    call))

(-> agent-test-message (string) json-object)
(defun agent-test-message (text)
  "Return one scripted assistant message containing TEXT."
  (json-object
   "type" "message"
   "role" "assistant"
   "content" (json-array
              (json-object "type" "output_text" "text" text))))

(-> agent-test-result
    (string list
     &key
     (:turn-state (option string))
     (:turn-completion turn-completion))
    provider-result)
(defun agent-test-result
    (response-id output-items &key turn-state (turn-completion :unspecified))
  "Return a scripted provider result containing OUTPUT-ITEMS."
  (make-instance 'provider-result
                 :response-id response-id
                 :output-items output-items
                 :tool-calls (remove-if-not #'function-call-item-p output-items)
                 :usage (json-object "input_tokens" 1 "output_tokens" 1)
                 :turn-state turn-state
                 :turn-completion turn-completion))

(-> test-agent-tool-loop () null)
(defun test-agent-tool-loop ()
  "Test authoritative replay, correlated tool output, callbacks, and turn-state scope."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "agent-loop"))
         (call (json-object
                "type" "function_call"
                "call_id" "call-1"
                "namespace" "test"
                "name" "echo"
                "arguments" "{\"value\":\"hello\"}"))
         (message (json-object
                   "type" "message"
                   "role" "assistant"
                   "content" (json-array
                              (json-object
                               "type" "output_text"
                               "text" "complete"))))
         (provider
           (make-instance
            'scripted-provider
            :results (list (agent-test-result "response-1"
                                              (list call)
                                              :turn-state "turn-state-1")
                           (agent-test-result "response-2" (list message)))))
         (registry (agent-test-registry))
         (deltas nil)
         (statuses nil))
    (unwind-protect
         (progn
           (let* ((agent (agent-create
                          :configuration configuration
                          :provider provider
                          :conversation conversation
                          :tool-registry registry
                          :worker ':unused
                          :maximum-tool-rounds 2))
                  (observer
                    (callback-agent-observer-create
                     :text-callback (lambda (text)
                                      (push text deltas))
                     :status-callback (lambda (status details)
                                        (declare (ignore details))
                                        (push status statuses))))
                  (result (agent-run-user-turn agent "run the echo" :observer observer)))
             (test-assert (string= (provider-result-response-id result) "response-2")
                          "the agent returns the final tool-free provider result")
             (test-assert (equal (nreverse (scripted-provider-input-counts provider))
                                 '(1 3))
                          "the second request replays the call and its correlated output")
             (test-assert (equal (nreverse (scripted-provider-turn-states provider))
                                 '(nil "turn-state-1"))
                          "provider turn state is replayed only inside the active turn")
             (test-assert (null (conversation-turn-state conversation))
                          "the agent clears request-local turn state after completion")
             (test-assert (= (length (conversation-input-items conversation)) 4)
                          "conversation history contains user, call, output, and answer")
             (test-assert (equal (nreverse deltas) '("delta" "delta"))
                          "the observer receives deltas from every provider request")
             (test-assert (member :tool-call-completed statuses)
                          "the observer receives correlated tool lifecycle status")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-agent-explicit-continuation () null)
(defun test-agent-explicit-continuation ()
  "Test a tool-free explicit continuation receives another bounded request."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration :identifier "agent-continue"))
         (provider
           (make-instance
            'scripted-provider
            :results
            (list (agent-test-result "response-1"
                                     (list (agent-test-message "working"))
                                     :turn-state "continuation-state"
                                     :turn-completion :continue)
                  (agent-test-result "response-2"
                                     (list (agent-test-message "done"))
                                     :turn-completion :end)))))
    (unwind-protect
         (let* ((agent (agent-create
                        :configuration configuration
                        :provider provider
                        :conversation conversation
                        :tool-registry (agent-test-registry)
                        :worker ':unused
                        :maximum-provider-requests 3))
                (result (agent-run-user-turn agent "continue explicitly")))
           (test-assert (string= (provider-result-response-id result) "response-2")
                        "the agent follows an explicit provider continuation")
           (test-assert
            (equal (nreverse (scripted-provider-input-counts provider)) '(1 2))
            "the continuation request replays the first completed message")
           (test-assert
            (equal (nreverse (scripted-provider-turn-states provider))
                   '(nil "continuation-state"))
            "the continuation request receives request-local routing state")
           (test-assert (null (conversation-turn-state conversation))
                        "explicit continuation state is cleared after the user turn"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-agent-invalid-call-history () null)
(defun test-agent-invalid-call-history ()
  "Test uncorrelatable and duplicate calls cannot poison durable history."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((conversation
                    (conversation-create configuration :identifier "missing-call-id"))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result
                       "missing-id"
                       (list (agent-test-call :arguments "{\"value\":\"x\"}"))))))
                  (agent
                    (agent-create :configuration configuration
                                  :provider provider
                                  :conversation conversation
                                  :tool-registry (agent-test-registry)
                                  :worker ':unused)))
             (test-assert
              (handler-case
                  (progn
                    (agent-run-user-turn agent "reject missing id")
                    nil)
                (agent-loop-error ()
                  t))
              "a call without correlation identity is rejected")
             (test-assert (= (length (conversation-input-items conversation)) 1)
                          "an uncorrelatable provider item is never persisted"))
           (let* ((conversation
                    (conversation-create configuration :identifier "duplicate-call-id"))
                  (first-call
                    (agent-test-call :call-id "duplicate"
                                     :arguments "{\"value\":\"first\"}"))
                  (duplicate-call
                    (agent-test-call :call-id "duplicate"
                                     :arguments "{\"value\":\"second\"}"))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list (agent-test-result "first" (list first-call)
                                              :turn-state "duplicate-state")
                           (agent-test-result "duplicate" (list duplicate-call)))))
                  (agent
                    (agent-create :configuration configuration
                                  :provider provider
                                  :conversation conversation
                                  :tool-registry (agent-test-registry)
                                  :worker ':unused)))
             (test-assert
              (handler-case
                  (progn
                    (agent-run-user-turn agent "reject duplicate id")
                    nil)
                (agent-loop-error ()
                  t))
              "a repeated call identity is rejected before persistence")
             (test-assert (= (length (conversation-input-items conversation)) 3)
                          "only the first call and its correlated output remain")
             (test-assert (null (conversation-turn-state conversation))
                          "turn state clears after a duplicate-call invariant failure")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-agent-bounds-and-tool-failures () null)
(defun test-agent-bounds-and-tool-failures ()
  "Test total tool correlation and independent provider and tool round limits."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((conversation
                    (conversation-create configuration :identifier "mixed-tools"))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result
                       "tool-batch"
                       (list
                        (agent-test-call :call-id "good"
                                         :arguments "{\"value\":\"ok\"}")
                        (agent-test-call :call-id "bad"
                                         :namespace "missing"
                                         :name "tool")))
                      (agent-test-result "tool-final"
                                         (list (agent-test-message "finished"))))))
                  (agent
                    (agent-create :configuration configuration
                                  :provider provider
                                  :conversation conversation
                                  :tool-registry (agent-test-registry)
                                  :worker ':unused)))
             (agent-run-user-turn agent "run mixed tools")
             (test-assert (= (length (conversation-input-items conversation)) 6)
                          "multiple calls each receive one correlated output")
             (let* ((records
                      (conversation--read-records
                       (conversation-pathname conversation)))
                    (tool-results
                      (remove-if-not
                       (lambda (record)
                         (and (listp record) (eq (first record) :tool-result)))
                       records)))
               (test-assert
                (equal (mapcar (lambda (record) (getf (rest record) :status))
                               tool-results)
                       '(:ok :error))
                "successful and failed calls remain explicitly distinguished")))
           (let* ((conversation
                    (conversation-create configuration :identifier "tool-bound"))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result
                       "tool-one"
                       (list (agent-test-call :call-id "one"
                                              :arguments "{\"value\":\"one\"}")))
                      (agent-test-result
                       "tool-two"
                       (list (agent-test-call :call-id "two"
                                              :arguments "{\"value\":\"two\"}"))))))
                  (agent
                    (agent-create :configuration configuration
                                  :provider provider
                                  :conversation conversation
                                  :tool-registry (agent-test-registry)
                                  :worker ':unused
                                  :maximum-tool-rounds 1)))
             (test-assert
              (handler-case
                  (progn
                    (agent-run-user-turn agent "reach tool bound")
                    nil)
                (agent-tool-round-limit-exceeded ()
                  t))
              "the agent stops beyond its independent tool-round bound")
             (test-assert (= (length (conversation-input-items conversation)) 5)
                          "the rejected over-limit call still receives a failure output"))
           (let* ((conversation
                    (conversation-create configuration :identifier "provider-bound"))
                  (provider
                    (make-instance
                     'scripted-provider
                     :results
                     (list
                      (agent-test-result "continue-one"
                                         (list (agent-test-message "one"))
                                         :turn-completion :continue)
                      (agent-test-result "continue-two"
                                         (list (agent-test-message "two"))
                                         :turn-completion :continue))))
                  (agent
                    (agent-create :configuration configuration
                                  :provider provider
                                  :conversation conversation
                                  :tool-registry (agent-test-registry)
                                  :worker ':unused
                                  :maximum-provider-requests 2)))
             (test-assert
              (handler-case
                  (progn
                    (agent-run-user-turn agent "reach provider bound")
                    nil)
                (agent-provider-request-limit-exceeded ()
                  t))
              "explicit continuations stop at the provider-request bound")
             (test-assert (= (length (conversation-input-items conversation)) 3)
                          "bounded provider results remain durable and replayable")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> run-agent-tests () boolean)
(defun run-agent-tests ()
  "Run focused agent-loop tests and return true on success."
  (test-agent-tool-loop)
  (test-agent-explicit-continuation)
  (test-agent-invalid-call-history)
  (test-agent-bounds-and-tool-failures)
  t)
