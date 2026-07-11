(in-package #:frob)

;;;; -- Provider Events --

(defclass provider-event ()
  ()
  (:documentation "A semantic event emitted while consuming a provider stream."))

(defclass assistant-delta-event (provider-event)
  ((text
    :initarg :text
    :reader assistant-delta-event-text
    :type string
    :documentation "The newly received assistant text."))
  (:documentation "An incremental assistant text update."))

(defclass reasoning-delta-event (provider-event)
  ((text
    :initarg :text
    :reader reasoning-delta-event-text
    :type string
    :documentation "The newly received visible reasoning summary text."))
  (:documentation "An incremental visible reasoning summary update."))

(defclass provider-item-event (provider-event)
  ((item
    :initarg :item
    :reader provider-item-event-item
    :type json-object
    :documentation "The authoritative completed Responses item."))
  (:documentation "A completed provider output item ready for persistence."))

(defclass provider-completed-event (provider-event)
  ((response-id
    :initarg :response-id
    :reader provider-completed-event-response-id
    :type (option string)
    :documentation "The provider response identifier, if supplied.")
   (usage
    :initarg :usage
    :reader provider-completed-event-usage
    :type t
    :documentation "Portable provider usage metadata, if supplied."))
  (:documentation "The successful terminal event for one provider request."))

(defclass provider-result ()
  ((response-id
    :initarg :response-id
    :reader provider-result-response-id
    :type (option string)
    :documentation "The provider response identifier, if supplied.")
   (output-items
    :initarg :output-items
    :reader provider-result-output-items
    :type list
    :documentation "Authoritative completed response items in wire order.")
   (tool-calls
    :initarg :tool-calls
    :reader provider-result-tool-calls
    :type list
    :documentation "The function-call subset of OUTPUT-ITEMS.")
   (usage
    :initarg :usage
    :reader provider-result-usage
    :type t
    :documentation "Provider usage metadata, if supplied.")
   (turn-state
    :initarg :turn-state
    :reader provider-result-turn-state
    :type (option string)
    :documentation "The routing token to replay within the current user turn."))
  (:documentation "The complete semantic result of one streamed provider request."))


;;;; -- Provider Protocol --

(defclass model-provider ()
  ()
  (:documentation "The abstract interface between an agent and a model service."))

(defclass codex-subscription-provider (model-provider)
  ((configuration
    :initarg :configuration
    :reader provider-configuration
    :type configuration
    :documentation "Immutable model and path configuration.")
   (credential-manager
    :initarg :credential-manager
    :reader provider-credential-manager
    :type credential-manager
    :documentation "Credential paths and refresh policy without retained tokens.")
   (session-id
    :initarg :session-id
    :reader provider-session-id
    :type non-empty-string
    :documentation "The stable provider session identifier."))
  (:documentation "A direct ChatGPT subscription client for the Codex Responses service."))

(-> provider-create (configuration) codex-subscription-provider)
(defun provider-create (configuration)
  "Create the default direct subscription provider for CONFIGURATION."
  (make-instance 'codex-subscription-provider
                 :configuration configuration
                 :credential-manager (credential-manager-create configuration)
                 :session-id (make-identifier)))

(-> provider-stream-turn
    (model-provider conversation vector function)
    provider-result)
(defgeneric provider-stream-turn (provider conversation tool-namespaces event-callback)
  (:documentation
   "Stream one model response for CONVERSATION using TOOL-NAMESPACES and EVENT-CALLBACK."))

(-> provider-open-response-stream
    (model-provider json-object oauth-credentials conversation)
    (values stream integer t))
(defgeneric provider-open-response-stream (provider request credentials conversation)
  (:documentation "Open an authenticated provider stream and return body, status, and headers."))


;;;; -- Responses Lite Encoding --

(-> responses-lite-developer-message (string) json-object)
(defun responses-lite-developer-message (instructions)
  "Return the Responses Lite developer message containing INSTRUCTIONS."
  (json-object
   "type" "message"
   "role" "developer"
   "content" (json-array
              (json-object
               "type" "input_text"
               "text" instructions))))

(-> responses-lite-additional-tools (vector) json-object)
(defun responses-lite-additional-tools (tool-namespaces)
  "Return the Responses Lite developer item exposing TOOL-NAMESPACES."
  (json-object
   "type" "additional_tools"
   "role" "developer"
   "tools" tool-namespaces))

(-> provider-request-object
    (codex-subscription-provider conversation vector)
    json-object)
(defun provider-request-object (provider conversation tool-namespaces)
  "Build the complete stateless Sol Responses Lite request for CONVERSATION."
  (let* ((configuration (provider-configuration provider))
         (prefix (list
                  (responses-lite-additional-tools tool-namespaces)
                  (responses-lite-developer-message
                   (system-prompt configuration))))
         (input (coerce (append prefix (conversation-input-items conversation))
                        'vector)))
    (json-object
     "model" (configuration-model configuration)
     "input" input
     "tool_choice" "auto"
     "parallel_tool_calls" false
     "reasoning" (json-object
                  "effort" (configuration-wire-effort configuration)
                  "context" "all_turns")
     "store" false
     "stream" t
     "include" (json-array "reasoning.encrypted_content")
     "prompt_cache_key" (conversation-identifier conversation)
     "text" (json-object "verbosity" "low"))))

(-> provider-user-agent () string)
(defun provider-user-agent ()
  "Return an honest, stable user agent for direct Frob provider requests."
  (format nil "frob/~A (~A ~A; ~A)"
          +frob-version+
          (software-type)
          (software-version)
          (machine-type)))

(defmethod provider-open-response-stream
    ((provider codex-subscription-provider)
     (request hash-table)
     (credentials oauth-credentials)
     (conversation conversation))
  "Open a direct authenticated SSE request to the ChatGPT Codex endpoint."
  (let* ((configuration (provider-configuration provider))
         (thread-id (conversation-identifier conversation))
         (request-id (make-identifier))
         (headers
           (append
            (list
             (cons "Authorization"
                   (format nil "Bearer ~A"
                           (oauth-credentials-access-token credentials)))
             (cons "ChatGPT-Account-ID"
                   (oauth-credentials-account-id credentials))
             (cons "Content-Type" "application/json")
             (cons "Accept" "text/event-stream")
             (cons "x-openai-internal-codex-responses-lite" "true")
             (cons "originator" "frob")
             (cons "User-Agent" (provider-user-agent))
             (cons "session-id" (provider-session-id provider))
             (cons "thread-id" thread-id)
             (cons "x-client-request-id" request-id))
            (when (conversation-turn-state conversation)
              (list (cons "x-codex-turn-state"
                          (conversation-turn-state conversation)))))))
    (dexador:post
     (configuration-provider-endpoint configuration)
     :headers headers
     :content (json-encode request)
     :want-stream t
     :force-string t
     :keep-alive nil
     :connect-timeout 30
     :read-timeout 300)))


;;;; -- SSE Decoding --

(defparameter +sse-end-of-stream+ (gensym "SSE-END-")
  "A private marker returned after a clean SSE end of stream.")

(-> sse-data-line (string) (option string))
(defun sse-data-line (line)
  "Return the payload of an SSE data LINE, or NIL for another field."
  (when (and (>= (length line) 5)
             (string= line "data:" :end1 5 :end2 5))
    (let ((start (if (and (> (length line) 5)
                          (char= (char line 5) #\Space))
                     6
                     5)))
      (subseq line start))))

(-> sse-read-line (stream) t)
(defun sse-read-line (stream)
  "Read a line from STREAM using only the portable character-stream protocol."
  (let ((characters nil))
    (loop for character = (read-char stream nil +sse-end-of-stream+)
          do (cond
               ((eq character +sse-end-of-stream+)
                (return (if characters
                            (coerce (nreverse characters) 'string)
                            +sse-end-of-stream+)))
               ((char= character #\Newline)
                (return (coerce (nreverse characters) 'string)))
               (t
                (push character characters))))))

(-> read-sse-data (stream) t)
(defun read-sse-data (stream)
  "Read one SSE event's joined data field from STREAM."
  (let ((data-lines nil))
    (loop
      (let ((raw-line (sse-read-line stream)))
        (when (eq raw-line +sse-end-of-stream+)
          (return (if data-lines
                      (format nil "~{~A~^~%~}" (nreverse data-lines))
                      +sse-end-of-stream+)))
        (let ((line (string-right-trim '(#\Return) raw-line)))
          (when (zerop (length line))
            (when data-lines
              (return (format nil "~{~A~^~%~}" (nreverse data-lines)))))
          (let ((data (sse-data-line line)))
            (when data
              (push data data-lines))))))))

(-> response-header (t string) (option string))
(defun response-header (headers name)
  "Return case-insensitive header NAME from Dexador HEADERS."
  (labels ((matching-name-p (candidate)
             (string-equal (string candidate) name)))
    (cond
      ((hash-table-p headers)
       (loop for key being the hash-keys of headers
               using (hash-value value)
             when (matching-name-p key)
               return value))
      ((listp headers)
       (let ((pair (find name headers :key #'first :test #'string-equal)))
         (when pair
           (if (consp (rest pair))
               (second pair)
               (rest pair)))))
      (t
       nil))))

(-> normalize-response-item (json-object) json-object)
(defun normalize-response-item (item)
  "Remove transient server item identifiers from replayable provider ITEM."
  (remhash "id" item)
  item)

(-> function-call-item-p (json-object) boolean)
(defun function-call-item-p (item)
  "Return true when ITEM is a Responses function call."
  (string= (or (json-get item "type") "") "function_call"))

(-> provider--consume-stream (stream t function) provider-result)
(defun provider--consume-stream (stream headers event-callback)
  "Consume STREAM into a provider result while invoking EVENT-CALLBACK."
  (let ((output-items nil)
        (response-id nil)
        (usage nil)
        (completed-p nil))
    (loop until completed-p
          for data = (read-sse-data stream)
          do (when (eq data +sse-end-of-stream+)
               (error 'response-stream-error
                      :message "The provider stream closed before a terminal event."
                      :status nil
                      :request-id nil
                      :response nil))
             (unless (string= data "[DONE]")
               (let* ((event (json-decode data))
                      (type (and (json-object-p event)
                                 (json-get event "type"))))
                 (cond
                   ((string= (or type "") "response.created")
                    (let ((response (json-get event "response")))
                      (when (json-object-p response)
                        (setf response-id (json-get response "id")))))
                   ((string= (or type "") "response.output_text.delta")
                    (funcall event-callback
                             (make-instance 'assistant-delta-event
                                            :text (or (json-get event "delta") ""))))
                   ((and type
                         (member type
                                 '("response.reasoning_summary_text.delta"
                                   "response.reasoning_text.delta")
                                 :test #'string=))
                    (funcall event-callback
                             (make-instance 'reasoning-delta-event
                                            :text (or (json-get event "delta") ""))))
                   ((string= (or type "") "response.output_item.done")
                    (let ((item (json-get event "item")))
                      (when (json-object-p item)
                        (normalize-response-item item)
                        (push item output-items)
                        (funcall event-callback
                                 (make-instance 'provider-item-event :item item)))))
                   ((string= (or type "") "response.completed")
                    (let ((response (json-get event "response")))
                      (when (json-object-p response)
                        (setf response-id (or (json-get response "id") response-id)
                              usage (json-get response "usage")))
                      (setf completed-p t)
                      (funcall event-callback
                               (make-instance 'provider-completed-event
                                              :response-id response-id
                                              :usage usage))))
                   ((and type
                         (member type
                                 '("response.failed" "response.incomplete" "error")
                                 :test #'string=))
                    (error 'provider-error
                           :message (format nil "The provider ended with ~A." type)
                           :status nil
                           :request-id response-id
                           :response (bounded-string data :limit 2000)))))))
    (let* ((ordered-items (nreverse output-items))
           (tool-calls (remove-if-not #'function-call-item-p ordered-items)))
      (make-instance 'provider-result
                     :response-id response-id
                     :output-items ordered-items
                     :tool-calls tool-calls
                     :usage usage
                     :turn-state (response-header headers "x-codex-turn-state")))))

(-> provider--attempt-turn
    (codex-subscription-provider conversation vector function boolean)
    provider-result)
(defun provider--attempt-turn
    (provider conversation tool-namespaces event-callback force-refresh)
  "Perform one provider attempt, optionally forcing credential refresh."
  (with-credentials (credentials (provider-credential-manager provider)
                                 :force-refresh force-refresh)
    (multiple-value-bind (stream status headers)
        (provider-open-response-stream
         provider
         (provider-request-object provider conversation tool-namespaces)
         credentials
         conversation)
      (unless (= status 200)
        (when (open-stream-p stream)
          (close stream))
        (error 'provider-error
               :message (format nil "The provider returned HTTP ~D." status)
               :status status
               :request-id nil
               :response nil))
      (unwind-protect
           (provider--consume-stream stream headers event-callback)
        (when (open-stream-p stream)
          (close stream))))))

(defmethod provider-stream-turn
    ((provider codex-subscription-provider)
     (conversation conversation)
     (tool-namespaces vector)
     (event-callback function))
  "Stream one direct Sol turn, retrying a single unauthorized response after refresh."
  (handler-case
      (provider--attempt-turn
       provider conversation tool-namespaces event-callback nil)
    (dexador.error:http-request-unauthorized ()
      (provider--attempt-turn
       provider conversation tool-namespaces event-callback t))
    (http-request-failed (condition)
      (error 'provider-error
             :message (format nil "The provider returned HTTP ~D."
                              (response-status condition))
             :status (response-status condition)
             :request-id (response-header
                          (response-headers condition)
                          "x-request-id")
             :response (bounded-string (response-body condition) :limit 2000)))))
