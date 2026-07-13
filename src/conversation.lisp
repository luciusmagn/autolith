(in-package #:autolith)

;;;; -- Conversation Object --

(defclass conversation ()
  ((identifier
    :initarg :identifier
    :reader conversation-identifier
    :type non-empty-string
    :documentation "The stable conversation identifier.")
   (pathname
    :initarg :pathname
    :reader conversation-pathname
    :type pathname
    :documentation "The append-only S-expression file.")
   (created-at
    :initarg :created-at
    :reader conversation-created-at
    :type timestamp
    :documentation "The creation time as Common Lisp universal time.")
   (origin-directory
    :initarg :origin-directory
    :initform nil
    :reader conversation-origin-directory
    :type (option string)
    :documentation "The workspace directory in which this conversation began.")
   (next-sequence
    :initarg :next-sequence
    :accessor conversation-next-sequence
    :type integer
    :documentation "The sequence number assigned to the next appended event.")
   (input-items
    :initarg :input-items
    :accessor conversation-input-items
    :type list
    :documentation "Provider wire items in chronological order.")
   (turn-state
    :initform nil
    :accessor conversation-turn-state
    :type (option string)
    :documentation "The transient provider routing token for one user turn.")
   (last-total-tokens
    :initform 0
    :accessor conversation-last-total-tokens
    :type (integer 0)
    :documentation "The total token usage reported by the newest provider step."))
  (:documentation "An append-only conversation and its provider projection."))

(-> conversation--write-form (pathname list &key (:append boolean)) null)
(defun conversation--write-form (pathname form &key append)
  "Write portable FORM to PATHNAME, appending when APPEND is true."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists (if append :append :supersede)
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (let ((*print-circle* t)
          (*print-readably* t)
          (*print-pretty* t))
      (prin1 form stream)
      (terpri stream)
      (finish-output stream)))
  nil)

(-> conversation-create (configuration &key (:identifier (option string))) conversation)
(defun conversation-create (configuration &key identifier)
  "Create and durably initialize a new conversation under CONFIGURATION."
  (let* ((created-at (get-universal-time))
         (conversation-id (or identifier (make-identifier)))
         (root (configuration-conversation-root configuration))
         (origin-directory (namestring
                            (configuration-working-directory configuration)))
         (pathname (merge-pathnames
                    (make-pathname :name conversation-id :type "sexp")
                    root))
         (conversation
           (make-instance 'conversation
                          :identifier conversation-id
                          :pathname pathname
                          :created-at created-at
                          :origin-directory origin-directory
                          :next-sequence 1
                          :input-items nil)))
    (when (probe-file pathname)
      (error 'conversation-error
             :message (format nil "Conversation ~A already exists." conversation-id)
             :pathname pathname
             :sequence nil))
    (conversation--write-form
     pathname
     (list :conversation
           :version 1
           :id conversation-id
           :created-at created-at
           :directory origin-directory))
    conversation))

(-> conversation-append-record (conversation list) list)
(defgeneric conversation-append-record (conversation record)
  (:documentation "Append portable RECORD to CONVERSATION and return the sequenced form."))

(defmethod conversation-append-record ((conversation conversation) (record list))
  "Assign sequence and time metadata, then append RECORD to CONVERSATION."
  (unless (keywordp (first record))
    (error 'conversation-invariant-error
           :message "A conversation record must begin with a keyword."
           :pathname (conversation-pathname conversation)
           :sequence (conversation-next-sequence conversation)))
  (let* ((sequence (conversation-next-sequence conversation))
         (sequenced (list* (first record)
                           :seq sequence
                           :time (get-universal-time)
                           (rest record))))
    (handler-case
        (conversation--write-form
         (conversation-pathname conversation)
         sequenced
         :append t)
      (error (condition)
        (error 'conversation-invariant-error
               :message (format nil "Could not append conversation record: ~A" condition)
               :pathname (conversation-pathname conversation)
               :sequence sequence)))
    (incf (conversation-next-sequence conversation))
    sequenced))

(-> conversation--append-input-item (conversation json-object) json-object)
(defun conversation--append-input-item (conversation item)
  "Append provider ITEM to CONVERSATION's in-memory chronological projection."
  (setf (conversation-input-items conversation)
        (nconc (conversation-input-items conversation) (list item)))
  item)

(-> user-message-item (string) json-object)
(defun user-message-item (content)
  "Return a Responses API user message containing CONTENT."
  (json-object
   "type" "message"
   "role" "user"
   "content" (json-array
              (json-object
               "type" "input_text"
               "text" content))))

(-> conversation-append-user-message (conversation string) json-object)
(defun conversation-append-user-message (conversation content)
  "Persist user CONTENT before adding its provider item to CONVERSATION."
  (let ((item (user-message-item content)))
    (conversation-append-record
     conversation
     (list :message
           :role :user
           :content content
           :wire-json (json-encode item)))
    (setf (conversation-turn-state conversation) nil)
    (conversation--append-input-item conversation item)))

(-> conversation-append-provider-item (conversation json-object) json-object)
(defun conversation-append-provider-item (conversation item)
  "Persist one authoritative completed provider ITEM in CONVERSATION."
  (conversation-append-record
   conversation
   (list :provider-item
         :wire-json (json-encode item)))
  (conversation--append-input-item conversation item))

(-> function-call-output-item (string string) json-object)
(defun function-call-output-item (call-id output)
  "Return a Responses API function-call output correlated by CALL-ID."
  (json-object
   "type" "function_call_output"
   "call_id" call-id
   "output" output))

(-> conversation-append-tool-result (conversation string string string boolean) json-object)
(defun conversation-append-tool-result (conversation call-id tool-name output success-p)
  "Persist and append one tool OUTPUT associated with CALL-ID and TOOL-NAME."
  (let ((item (function-call-output-item call-id output)))
    (conversation-append-record
     conversation
     (list :tool-result
           :call-id call-id
           :tool tool-name
           :status (if success-p :ok :error)
           :output output
           :wire-json (json-encode item)))
    (conversation--append-input-item conversation item)))

(-> conversation--usage-total (t) (option integer))
(defun conversation--usage-total (usage)
  "Return the total token count carried by portable or wire USAGE data."
  (cond
    ((json-object-p usage)
     (let ((total (json-get usage "total_tokens")))
       (and (integerp total) total)))
    ((listp usage)
     (let ((total (second (assoc "total_tokens" usage :test #'equal))))
       (and (integerp total) total)))
    (t
     nil)))

(-> conversation-append-provider-metadata (conversation list) list)
(defun conversation-append-provider-metadata (conversation metadata)
  "Persist portable provider METADATA that is not part of request history."
  (let ((total (conversation--usage-total (getf metadata :usage))))
    (when total
      (setf (conversation-last-total-tokens conversation) total)))
  (conversation-append-record
   conversation
   (list :provider :metadata metadata)))

(define-constant +conversation-summary-prefix+
  "A previous segment of this conversation was compacted. The summary below replaces that segment; use it to continue seamlessly without repeating completed work."
  :test #'string=
  :documentation "The bridge text introducing a compaction summary to the model.")

(-> conversation-summary-item (string) json-object)
(defun conversation-summary-item (content)
  "Return the replayable wire item carrying a compaction summary CONTENT."
  (json-object
   "type" "message"
   "role" "user"
   "content" (json-array
              (json-object
               "type" "input_text"
               "text" (format nil "~A~2%~A"
                              +conversation-summary-prefix+
                              content)))))

(-> conversation-append-summary (conversation string) list)
(defun conversation-append-summary (conversation content)
  "Persist a compaction summary and replace CONVERSATION's projection with it.

The durable record covers every record before it, so replay reproduces the
same compacted projection. The provider turn-state token is dropped because
it described the uncompacted context."
  (let ((record (conversation-append-record
                 conversation
                 (list :summary
                       :through-seq (1- (conversation-next-sequence
                                         conversation))
                       :content content))))
    (setf (conversation-input-items conversation)
          (list (conversation-summary-item content))
          (conversation-turn-state conversation) nil
          (conversation-last-total-tokens conversation) 0)
    record))


;;;; -- Conversation Loading --

(-> conversation--read-records (pathname) list)
(defun conversation--read-records (pathname)
  "Read complete top-level forms from PATHNAME, ignoring an incomplete final form."
  (with-open-file (stream pathname :direction :input :external-format :utf-8)
    (let ((*read-eval* nil)
          (end-marker (cons nil nil))
          (records nil))
      (handler-case
          (loop for record = (read stream nil end-marker)
                until (eq record end-marker)
                do (push record records))
        (end-of-file ()
          nil)
        (reader-error (condition)
          (error 'conversation-invariant-error
                 :message (format nil "Malformed conversation record: ~A" condition)
                 :pathname pathname
                 :sequence nil)))
      (nreverse records))))

(-> conversation--apply-record (conversation list) null)
(defun conversation--apply-record (conversation record)
  "Project one persisted RECORD into CONVERSATION's in-memory state."
  (unless (and (listp record) (keywordp (first record)))
    (error 'conversation-invariant-error
           :message "A persisted conversation record is not a keyword list."
           :pathname (conversation-pathname conversation)
           :sequence nil))
  (let ((sequence (getf (rest record) :seq))
        (wire-json (getf (rest record) :wire-json)))
    (when (integerp sequence)
      (setf (conversation-next-sequence conversation)
            (max (conversation-next-sequence conversation) (1+ sequence))))
    (when (and (member (first record)
                       '(:message :provider-item :tool-result))
               (stringp wire-json))
      (let ((item (json-decode wire-json)))
        (unless (json-object-p item)
          (error 'conversation-invariant-error
                 :message "A persisted provider item is not a JSON object."
                 :pathname (conversation-pathname conversation)
                 :sequence sequence))
        (conversation--append-input-item conversation item)))
    (when (eq (first record) :summary)
      (let ((content (getf (rest record) :content)))
        (when (stringp content)
          (setf (conversation-input-items conversation)
                (list (conversation-summary-item content))
                (conversation-last-total-tokens conversation) 0))))
    (when (eq (first record) :provider)
      (let ((total (conversation--usage-total
                    (getf (getf (rest record) :metadata) :usage))))
        (when total
          (setf (conversation-last-total-tokens conversation) total)))))
  nil)

(-> conversation-peek-header (pathname) (option list))
(defun conversation-peek-header (pathname)
  "Return PATHNAME's leading conversation header form, or NIL when unreadable.

Only the first top-level form is read, so peeking stays cheap for large
conversation files."
  (handler-case
      (with-open-file (stream pathname :direction :input :external-format :utf-8)
        (let* ((*read-eval* nil)
               (end-marker (cons nil nil))
               (form (read stream nil end-marker)))
          (if (and (listp form)
                   (eq (first form) :conversation))
              form
              nil)))
    (error ()
      nil)))

(-> conversation-load (pathname) conversation)
(defun conversation-load (pathname)
  "Load a conversation from PATHNAME and rebuild its provider input projection."
  (let* ((records (conversation--read-records pathname))
         (header (first records)))
    (unless (and (listp header)
                 (eq (first header) :conversation)
                 (= (or (getf (rest header) :version) 0) 1)
                 (non-empty-string-p (getf (rest header) :id)))
      (error 'conversation-invariant-error
             :message "The conversation header is missing or unsupported."
             :pathname pathname
             :sequence nil))
    (let* ((directory (getf (rest header) :directory))
           (conversation
             (make-instance 'conversation
                            :identifier (getf (rest header) :id)
                            :pathname pathname
                            :created-at (getf (rest header) :created-at)
                            :origin-directory (and (stringp directory)
                                                   directory)
                            :next-sequence 1
                            :input-items nil)))
      (dolist (record (rest records))
        (conversation--apply-record conversation record))
      conversation)))

(-> conversation-pathname-for-id (configuration string) pathname)
(defun conversation-pathname-for-id (configuration identifier)
  "Return CONFIGURATION's conversation pathname for IDENTIFIER."
  (merge-pathnames (make-pathname :name identifier :type "sexp")
                   (configuration-conversation-root configuration)))

(-> conversation-load-by-id (configuration string) conversation)
(defun conversation-load-by-id (configuration identifier)
  "Load IDENTIFIER from CONFIGURATION's conversation directory."
  (let ((pathname (conversation-pathname-for-id configuration identifier)))
    (unless (probe-file pathname)
      (error 'conversation-error
             :message (format nil "Conversation ~A does not exist." identifier)
             :pathname pathname
             :sequence nil))
    (conversation-load pathname)))

(-> conversation-list (configuration) list)
(defun conversation-list (configuration)
  "Return known conversation pathnames, newest first."
  (let ((root (configuration-conversation-root configuration)))
    (if (probe-file root)
        (sort (uiop:directory-files root "*.sexp")
              #'>
              :key (lambda (pathname)
                     (or (file-write-date pathname) 0)))
        nil)))
