(in-package #:frob)

;;;; -- Presentation Test Support --

(-> application-tests--ui-application (&key (:columns integer)) application)
(defun application-tests--ui-application (&key (columns 40))
  "Return a minimal application presenting into a recording terminal."
  (make-instance 'application
                 :ui (terminal-ui-create
                      :terminal (make-instance 'recording-terminal
                                               :columns columns))))


;;;; -- Focused Presentation Tests --

(-> test-transcript-entries () null)
(defun test-transcript-entries ()
  "Test styled transcript entry construction, wrapping, and output bounds."
  (let ((application (application-tests--ui-application :columns 40)))
    (let ((entry (conversation-record-entry
                  application
                  '(:message :seq 1 :time 0 :role :user :content "hello there"))))
      (test-assert (equal (first entry) (terminal-span :user "❯ you"))
                   "user records present a styled you header")
      (test-assert (search "  hello there"
                           (terminal-span-text (first (last entry))))
                   "user bodies are indented beneath their header"))
    (let ((entry (conversation-record-entry
                  application
                  (list :message :seq 1 :time 0 :role :user
                        :content (make-string 50 :initial-element #\a)))))
      (test-assert (= (count #\Newline
                             (terminal-span-text (first (last entry))))
                      1)
                   "long bodies wrap at the terminal width"))
    (let ((entry (response-item-entry
                  application
                  (json-decode
                   "{\"type\":\"message\",\"role\":\"assistant\",
                     \"content\":[{\"type\":\"output_text\",\"text\":\"hi\"}]}"))))
      (test-assert (equal (first entry) (terminal-span :brand "● frob"))
                   "assistant items present a styled frob header"))
    (let ((entry (response-item-entry
                  application
                  (json-decode
                   "{\"type\":\"function_call\",\"namespace\":\"self\",
                     \"name\":\"eval\",
                     \"arguments\":\"{\\\"form\\\":\\\"(+ 1 2)\\\"}\"}"))))
      (test-assert (equal (first entry) (terminal-span :tool "▸ self.eval"))
                   "tool requests present a styled tool header")
      (test-assert (eq (terminal-span-style (second entry)) ':dim)
                   "tool arguments render as a dim detail")
      (test-assert (= (length entry) 2)
                   "tool requests stay on one header row"))
    (let ((entry (conversation-record-entry
                  application
                  '(:tool-result :seq 2 :time 0 :call-id 1 :tool "self.eval"
                    :status :ok :output "42"))))
      (test-assert (equal (first entry) (terminal-span :success "✓ self.eval"))
                   "successful tool results present a success header"))
    (let ((entry (conversation-record-entry
                  application
                  '(:tool-result :seq 3 :time 0 :call-id 2 :tool "self.eval"
                    :status :error :output "boom"))))
      (test-assert (equal (first entry)
                          (terminal-span :failure "✗ self.eval failed"))
                   "failed tool results present a failure header")))
  (let* ((output (format nil "~{line ~D~^~%~}"
                         (loop for index from 1 to 20
                               collect index)))
         (bounded (application--bounded-tool-output output)))
    (test-assert (search "… +8 more lines" bounded)
                 "long tool output is bounded with a truncation note"))
  (test-assert (null (application--bounded-tool-output ""))
               "empty tool output produces no transcript body")
  (let ((help (application-help)))
    (test-assert (search "/rollback ID" help)
                 "help lists commands with their argument hints")
    (test-assert (search "leave Frob" help)
                 "help lists command descriptions"))
  nil)

(-> run-application-tests () boolean)
(defun run-application-tests ()
  "Run focused application presentation tests and return true on success."
  (test-transcript-entries)
  t)
