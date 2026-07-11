(in-package #:frob)

;;;; -- Subsystem Tests --

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
