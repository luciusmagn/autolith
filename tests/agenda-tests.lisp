(in-package #:autolith)

;;;; -- Workspace Agenda Tests --

(-> test-agenda-persistence-and-transport () null)
(defun test-agenda-persistence-and-transport ()
  "Test agenda mutation, reload, copy, and moved-repository rekeying."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (source (merge-pathnames "source/" root))
         (copy-target (merge-pathnames "copy-target/" root))
         (move-target (merge-pathnames "move-target/" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames "marker" source))
           (ensure-directories-exist (merge-pathnames "marker" copy-target))
           (ensure-directories-exist (merge-pathnames "marker" move-target))
           (let* ((source-configuration
                    (configuration-with-working-directory configuration source))
                  (state (agenda-load source-configuration))
                  (item
                    (agenda-add :configuration source-configuration
                                :state state
                                :text "ship the release"
                                :status ':todo
                                :now 10))
                  (note
                    (agenda-add :configuration source-configuration
                                :state state
                                :text "keep compatibility"
                                :status ':note
                                :now 20)))
             (agenda-update source-configuration
                            state
                            (agenda-item-identifier item)
                            :status ':doing
                            :now 30)
             (let* ((loaded (agenda-load source-configuration))
                    (source-name
                      (workspace-agenda-directory
                       (agenda-current source-configuration loaded))))
               (test-assert
                (equal (mapcar #'agenda-item-status
                               (workspace-agenda-items
                                (agenda-current source-configuration loaded)))
                       '(:doing :note))
                "agenda items retain order and updated status across reload")
               (agenda-transport :configuration source-configuration
                                 :state loaded
                                 :source-directory source-name
                                 :target-directory copy-target)
               (test-assert
                (and (agenda-find loaded source-name)
                     (= (length
                         (workspace-agenda-items
                          (agenda-find
                           loaded
                           (agenda-directory-name
                            source-configuration copy-target
                            :require-existing-p t))))
                        2))
                "copying an agenda preserves its source and all target items")
               (uiop:delete-directory-tree source
                                           :validate t
                                           :if-does-not-exist :ignore)
               (agenda-transport :configuration source-configuration
                                 :state loaded
                                 :source-directory source-name
                                 :target-directory move-target
                                 :move-p t)
               (test-assert
                (and (null (agenda-find loaded source-name))
                     (= (length (agenda-state-records loaded)) 2))
                "moving a missing-path agenda rekeys it without losing copies")
               (let ((copy-configuration
                       (configuration-with-working-directory
                        source-configuration copy-target)))
                 (test-assert
                  (agenda-remove copy-configuration
                                 loaded
                                 (agenda-item-identifier note))
                  "agenda removal targets the current workspace only")))
             (test-assert (= (logand (sb-posix:stat-mode
                                      (sb-posix:stat
                                       (namestring
                                        (configuration-agenda-path
                                         source-configuration))))
                                     #o777)
                             #o600)
                          "agenda state is private to the current user")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-agenda-malformed-state () null)
(defun test-agenda-malformed-state ()
  "Test malformed agenda state cannot evaluate reader forms."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (pathname (configuration-agenda-path configuration))
         (*agenda-reader-evaluated-p* nil))
    (declare (special *agenda-reader-evaluated-p*))
    (unwind-protect
         (progn
           (ensure-directories-exist pathname)
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-string
              "#.(setf autolith::*agenda-reader-evaluated-p* t)"
              stream))
           (handler-bind ((agenda-load-warning #'muffle-warning))
             (test-assert (null (agenda-state-records
                                 (agenda-load configuration)))
                          "malformed agenda state loads as empty"))
           (test-assert (null *agenda-reader-evaluated-p*)
                        "agenda state disables reader evaluation"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-agenda-tools-and-prompt () null)
(defun test-agenda-tools-and-prompt ()
  "Test agenda tool dispatch, transport inspection, and prompt recall."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (target (merge-pathnames "transport-target/" root))
         (registry (make-default-tool-registry)))
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames "marker" target))
           (let ((conversation (conversation-create configuration
                                                    :identifier "agenda-tools")))
             (labels ((run (name &rest arguments)
                        "Execute agenda NAME with alternating ARGUMENTS."
                        (tool-registry-execute-call
                         registry
                         (json-object
                          "namespace" "agenda"
                          "name" name
                          "arguments" (json-encode
                                       (apply #'json-object arguments)))
                         (make-instance 'tool-context
                                        :configuration configuration
                                        :worker nil
                                        :conversation conversation))))
               (test-assert
                (tool-result-success-p
                 (run "add" "text" "finish agenda integration"
                            "status" "doing"))
                "agenda.add creates a current-workspace item")
               (let* ((state (agenda-load configuration))
                      (item (first (workspace-agenda-items
                                    (agenda-current configuration state))))
                      (identifier (agenda-item-identifier item))
                      (prompt (system-prompt configuration)))
                 (test-assert
                  (and (search identifier prompt)
                       (search "finish agenda integration" prompt)
                       (search "[doing]" prompt))
                  "the system prompt carries the complete current agenda")
                 (test-assert
                  (tool-result-success-p
                   (run "update" "id" identifier "status" "blocked"))
                  "agenda.update changes an item by stable id")
                 (test-assert
                  (search "[blocked]"
                          (tool-result-content (run "list")))
                  "agenda.list returns complete updated item data")
                 (test-assert
                  (tool-result-success-p
                   (run "transport"
                        "operation" "copy"
                        "source-directory"
                        (namestring
                         (configuration-working-directory configuration))
                        "target-directory" (namestring target)))
                  "agenda.transport copies an agenda to another workspace")
                 (test-assert
                  (search "finish agenda integration"
                          (tool-result-content
                           (run "transport"
                                "operation" "view"
                                "source-directory" (namestring target))))
                  "agenda.transport views another workspace's complete agenda")
                 (test-assert
                  (and (search (namestring
                                (configuration-working-directory configuration))
                               (tool-result-content
                                (run "transport" "operation" "workspaces")))
                       (search (namestring target)
                               (tool-result-content
                                (run "transport" "operation" "workspaces"))))
                  "agenda.transport enumerates every stored workspace key")
                 (test-assert
                  (tool-result-success-p (run "remove" "id" identifier))
                  "agenda.remove deletes a current-workspace item")
                 (test-assert
                  (not (tool-result-success-p
                        (run "transport" "operation" "rename")))
                  "agenda.transport rejects unsupported operations")))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
