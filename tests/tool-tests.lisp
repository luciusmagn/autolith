(in-package #:frob)

;;;; -- Subsystem Tests --

(-> test-tool-registry () null)
(defun test-tool-registry ()
  "Test namespaced tool schema construction and total dispatch failure handling."
  (let* ((registry (make-default-tool-registry))
         (schemas (tool-registry-provider-schemas registry))
         (configuration (test-configuration))
         (root (test-configuration-root configuration)))
    (unwind-protect
         (let* ((conversation (conversation-create configuration
                                                   :identifier "tool-registry"))
                (context (make-instance 'tool-context
                                        :configuration configuration
                                        :worker nil
                                        :conversation conversation))
                (unknown-call (json-object
                               "namespace" "missing"
                               "name" "operation"
                               "arguments" "{}"))
                (result (tool-registry-execute-call
                         registry unknown-call context)))
           (test-assert (= (length (tool-registry-tools registry)) 17)
                        "the default registry exposes the complete initial tool set")
           (test-assert (= (length schemas) 2)
                        "the provider schemas contain two namespaces")
           (test-assert (string= (json-get (aref schemas 0) "name") "lisp")
                        "the disposable Lisp namespace is first")
           (test-assert (= (length (json-get (aref schemas 0) "tools")) 6)
                        "the Lisp namespace exposes six worker operations")
           (test-assert (string= (json-get (aref schemas 1) "name") "self")
                        "the active-image namespace is second")
           (test-assert (= (length (json-get (aref schemas 1) "tools")) 11)
                        "the self namespace exposes eleven active-image operations")
           (test-assert (tool-registry-find registry "self" "source")
                        "tracked source inspection has a dedicated self tool")
           (test-assert (not (tool-result-success-p result))
                        "unknown provider calls produce a correlated tool failure"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
