(in-package #:frob)

;;;; -- Subsystem Tests --

(-> test-conversation-compaction () null)
(defun test-conversation-compaction ()
  "Test summary records, projection replacement, and usage tracking."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let ((conversation (conversation-create configuration
                                                  :identifier "compact")))
           (conversation-append-user-message conversation "first question")
           (conversation-append-provider-metadata
            conversation
            (list :request-number 1
                  :response-id "one"
                  :usage '(("total_tokens" 4321))))
           (test-assert (= (conversation-last-total-tokens conversation) 4321)
                        "usage totals track the newest provider step")
           (conversation-append-summary conversation
                                        "summary of the earlier work")
           (test-assert (= (length (conversation-input-items conversation)) 1)
                        "compaction replaces the projection with one bridge")
           (test-assert (zerop (conversation-last-total-tokens conversation))
                        "compaction resets the tracked usage")
           (test-assert (null (conversation-turn-state conversation))
                        "compaction drops the provider turn state")
           (conversation-append-user-message conversation "later question")
           (let* ((reloaded (conversation-load-by-id configuration "compact"))
                  (items (conversation-input-items reloaded))
                  (bridge-text (json-get
                                (aref (json-get (first items) "content") 0)
                                "text")))
             (test-assert (= (length items) 2)
                          "replay reproduces the compacted projection")
             (test-assert (search "summary of the earlier work" bridge-text)
                          "the bridge item carries the summary")
             (test-assert (search "compacted" bridge-text)
                          "the bridge item explains its provenance")
             (test-assert (zerop (conversation-last-total-tokens reloaded))
                          "replay resets usage tracked before the summary")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-origin-directory () null)
(defun test-conversation-origin-directory ()
  "Test origin directory persistence, peeking, and legacy header tolerance."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "origin"))
                (expected (namestring
                           (configuration-working-directory configuration))))
           (test-assert (string= (conversation-origin-directory conversation)
                                 expected)
                        "a new conversation records its origin directory")
           (test-assert (string= (conversation-origin-directory
                                  (conversation-load-by-id configuration
                                                           "origin"))
                                 expected)
                        "a reloaded conversation preserves its origin directory")
           (test-assert (string= (getf (rest (conversation-peek-header
                                              (conversation-pathname
                                               conversation)))
                                       :directory)
                                 expected)
                        "peeking reads the origin directory cheaply")
           (let ((legacy (conversation-pathname-for-id configuration "legacy")))
             (conversation--write-form
              legacy
              (list :conversation :version 1 :id "legacy" :created-at 1))
             (test-assert (null (conversation-origin-directory
                                 (conversation-load-by-id configuration
                                                          "legacy")))
                          "legacy conversations without an origin still load")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-persistence () null)
(defun test-conversation-persistence ()
  "Test append-only conversation projection and incomplete-tail recovery."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration :identifier "test-turn"))
                (assistant-item
                  (json-object
                   "type" "message"
                   "role" "assistant"
                   "content" (json-array
                              (json-object "type" "output_text" "text" "hello")))))
           (conversation-append-user-message conversation "hi")
           (conversation-append-provider-item conversation assistant-item)
           (conversation-append-tool-result
            conversation "call-1" "lisp.eval" "42" t)
           (with-open-file (stream (conversation-pathname conversation)
                                   :direction :output
                                   :if-exists :append
                                   :external-format :utf-8)
             (write-string "(:incomplete" stream))
           (let ((loaded (conversation-load-by-id configuration "test-turn")))
             (test-assert (= (length (conversation-input-items loaded)) 3)
                          "conversation reload projects complete wire items")
             (test-assert (= (conversation-next-sequence loaded) 4)
                          "conversation reload restores its next sequence")
             (test-assert
              (string= (json-get (first (conversation-input-items loaded)) "role")
                       "user")
              "conversation reload preserves the first user message")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
