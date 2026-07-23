(in-package #:autolith)

;;;; -- Workspace Agenda Tests --

(-> test-agenda-command () null)
(defun test-agenda-command ()
  "Test /agenda presents current entries, statuses, identifiers, and links."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (state (agenda-load configuration))
         (memory
           (memory-remember configuration
                            :title "Command-linked memory"
                            :content "Visible through its stable identifier."
                            :tags '("agenda")))
         (item
           (agenda-add :configuration configuration
                       :state state
                       :text "inspect the current agenda"
                       :status ':doing
                       :memory-identifiers (list (memory-identifier memory))))
         (terminal (make-instance 'recording-terminal :columns 80))
         (ui (terminal-ui-create :terminal terminal))
         (application (make-instance 'application
                                     :configuration configuration
                                     :ui ui)))
    (unwind-protect
         (progn
           (terminal-ui-start ui)
           (test-assert (eq (application-command application "/agenda")
                            ':continue)
                        "/agenda remains inside the interactive application")
           (let ((output (recording-terminal-output terminal)))
             (test-assert
              (and (search "agenda" output)
                   (search "[doing]" output)
                   (search "inspect the current agenda" output)
                   (search (agenda-item-identifier item) output)
                   (search (memory-identifier memory) output))
              "/agenda shows complete current-workspace entry information"))
           (test-assert (search "/agenda" (application-help))
                        "interactive help includes /agenda"))
      (ignore-errors (terminal-ui-stop ui))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

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
                  (memory
                    (memory-remember
                     source-configuration
                     :title "Release procedure"
                     :content "Publish the verified release artifacts."
                     :tags '("release")))
                  (item
                    (agenda-add :configuration source-configuration
                                :state state
                                :text "ship the release"
                                :status ':todo
                                :memory-identifiers
                                (list (memory-identifier memory))
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
               (let ((loaded-item
                       (first
                        (workspace-agenda-items
                         (agenda-current source-configuration loaded)))))
                 (test-assert
                  (equal (agenda-item-memory-identifiers loaded-item)
                         (list (memory-identifier memory)))
                  "agenda items retain stable memory links across reload")
                 (memory-remember
                  source-configuration
                  :identifier (memory-identifier memory)
                  :title "Release procedure"
                  :content "Publish only signed verified release artifacts."
                  :tags '("release"))
                 (test-assert
                  (string= (memory-content
                            (memory-find
                             source-configuration
                             (first
                              (agenda-item-memory-identifiers loaded-item))))
                           "Publish only signed verified release artifacts.")
                  "replacing a memory preserves agenda links through its stable id"))
               (agenda-transport :configuration source-configuration
                                 :state loaded
                                 :source-directory source-name
                                 :target-directory copy-target)
               (let ((copied
                       (agenda-find
                        loaded
                        (agenda-directory-name
                         source-configuration copy-target
                         :require-existing-p t))))
                 (test-assert
                  (and (agenda-find loaded source-name)
                       (= (length (workspace-agenda-items copied)) 2)
                       (equal (agenda-item-memory-identifiers
                               (first (workspace-agenda-items copied)))
                              (list (memory-identifier memory))))
                  "copying an agenda preserves its source, items, and memory links"))
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

(-> test-agenda-version-one-migration () null)
(defun test-agenda-version-one-migration ()
  "Test that legacy agendas load and upgrade on their next successful write."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (directory
           (namestring (configuration-working-directory configuration)))
         (pathname (configuration-agenda-path configuration))
         (identifier "legacy-agenda-item"))
    (unwind-protect
         (progn
           (snapshot-write
            pathname
            (list :agendas
                  :version 1
                  :records
                  (list
                   (list :agenda
                         :directory directory
                         :items
                         (list
                          (list :item
                                :id identifier
                                :text "migrate this agenda"
                                :status ':todo
                                :created-at 10
                                :updated-at 10))))))
           (let* ((state (agenda-load configuration))
                  (item
                    (first
                     (workspace-agenda-items
                      (agenda-current configuration state)))))
             (test-assert (null (agenda-item-memory-identifiers item))
                          "version-one items load with no memory links")
             (agenda-update configuration state identifier
                            :status ':doing
                            :now 20))
           (multiple-value-bind (form sole-form-p)
               (snapshot-read pathname)
             (test-assert
              (and sole-form-p
                   (= (third form) *agenda-version*)
                   (readable-state-property-present-p
                    (rest
                     (first
                      (getf (rest (first (fifth form))) :items)))
                    :memory-ids))
              "the next agenda write upgrades legacy state to version two")))
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
         (registry (make-default-tool-registry))
         (memory
           (memory-remember configuration
                            :title "Agenda integration"
                            :content "Keep the agenda integration durable."
                            :tags '("agenda"))))
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
                            "status" "doing"
                            "memory-ids"
                            (json-array (memory-identifier memory))))
                "agenda.add creates a current-workspace item")
               (let* ((state (agenda-load configuration))
                      (item (first (workspace-agenda-items
                                    (agenda-current configuration state))))
                      (identifier (agenda-item-identifier item))
                      (prompt (system-prompt configuration)))
                 (test-assert
                  (and (search identifier prompt)
                       (search "finish agenda integration" prompt)
                       (search "[doing]" prompt)
                       (search (memory-identifier memory) prompt))
                  "the system prompt carries the complete agenda and memory links")
                 (test-assert
                  (tool-result-success-p
                   (run "update" "id" identifier "status" "blocked"))
                  "agenda.update changes an item by stable id")
                 (test-assert
                 (search "[blocked]"
                          (tool-result-content (run "list")))
                  "agenda.list returns complete updated item data")
                 (test-assert
                  (search (memory-identifier memory)
                          (tool-result-content (run "list")))
                  "agenda.update preserves attached memories when omitted")
                 (test-assert
                  (not (tool-result-success-p
                        (run "update"
                             "id" identifier
                             "memory-ids" (json-array "missing-memory"))))
                  "agenda.update rejects unknown memory identifiers")
                 (test-assert
                  (tool-result-success-p
                   (run "update" "id" identifier
                                 "memory-ids" (json-array)))
                  "agenda.update accepts an empty array to detach memories")
                 (test-assert
                  (not (search (memory-identifier memory)
                               (tool-result-content (run "list"))))
                  "detaching removes the memory id from agenda output")
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
