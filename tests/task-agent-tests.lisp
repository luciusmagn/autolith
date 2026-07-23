(in-package #:autolith)

;;;; -- Native Role Contract Tests --

(-> test-task-agent-native-reader () null)
(defun test-task-agent-native-reader ()
  "Test exact, safe, diagnostic-rich parsing of native role files."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (directory     (merge-pathnames "agents/" root)))
    (unwind-protect
         (progn
           (let* ((pathname
                    (merge-pathnames "native.sexp" directory))
                  (definition
                    (task-parse-agent-file
                     (task-tests--write-native-form
                      pathname
                      (task-tests--role-form
                       "native" "Native role" "Use native Lisp data."
                       :tools '("fs.read")
                       :blocking-p t))
                     :project)))
             (test-assert
              (and (string= (task-agent-definition-name definition) "native")
                   (string= (task-agent-definition-instructions definition)
                            "Use native Lisp data.")
                   (equal (task-agent-definition-tools definition)
                          '("fs.read"))
                   (task-agent-definition-blocking-p definition)
                   (eq (task-agent-definition-source definition) :project)
                   (equal (task-agent-definition-pathname definition)
                          pathname))
              "one .sexp form creates a complete native role definition"))
           (let* ((pathname
                    (task-tests--write-text
                     (merge-pathnames "deterministic.sexp" directory)
                     "(:name \"deterministic\" :description \"Deterministic reader\" :instructions \"Ignore ambient reader state.\" :output (:type :number :enum (10 1.5)))"))
                  (definition
                    (let ((*read-base* 16)
                          (*read-suppress* t)
                          (*read-default-float-format* 'single-float)
                          (*package* (find-package '#:common-lisp-user)))
                      (task-parse-agent-file pathname :project)))
                  (enum
                    (getf (task-agent-definition-output definition) :enum)))
             (test-assert
              (and (= (first enum) 10)
                   (typep (second enum) 'double-float))
              "native role parsing ignores ambient package, base, suppression, and float bindings"))
           (let* ((pathname
                    (task-tests--write-text
                     (merge-pathnames "block-comment.sexp" directory)
                     "(:name \"block-comment\" #| outer #| nested |# comment |# :description \"Commented role\" :instructions \"Accept standard block comments.\")"))
                  (definition
                    (task-parse-agent-file pathname :project)))
             (test-assert
              (string= (task-agent-definition-name definition)
                       "block-comment")
              "native role parsing accepts nested standard block comments"))
           (let* ((pathname
                    (merge-pathnames "deeply-nested.sexp" directory))
                  (source
                    (concatenate
                     'string
                     (make-string 129 :initial-element #\()
                     "nil"
                     (make-string 129 :initial-element #\))))
                  (condition
                    (task-tests--agent-definition-error
                     (task-tests--write-text pathname source)
                     :project)))
             (test-assert
              (search "nesting"
                      (string-downcase
                       (princ-to-string
                        (task-agent-definition-error-cause condition))))
              "native role parsing rejects source nesting beyond its hard bound"))
           (setf *task-test-reader-evaluated-p* nil)
           (let* ((pathname
                    (merge-pathnames "reader-eval.sexp" directory))
                  (condition
                    (task-tests--agent-definition-error
                     (task-tests--write-text
                      pathname
                      "(:name \"reader-eval\" :description \"Unsafe\" :instructions #.(progn (setf *task-test-reader-evaluated-p* t) \"executed\"))")
                     :project)))
             (test-assert (not *task-test-reader-evaluated-p*)
                          "the native role reader binds *READ-EVAL* to NIL")
             (test-assert
              (typep (task-agent-definition-error-line condition)
                     '(integer 1))
              "reader failures retain a one-based source line"))
           (let* ((pathname
                    (task-tests--write-text
                     (merge-pathnames "fresh-readtable.sexp" directory)
                     "!"))
                  (condition
                    (let ((*readtable* (copy-readtable nil)))
                      (set-macro-character
                       #\!
                       (lambda (stream character)
                         (declare (ignore stream character))
                         (task-tests--role-form
                          "fresh-readtable" "Inherited macro"
                          "This must not be accepted."))
                       nil
                       *readtable*)
                      (task-tests--agent-definition-error
                       pathname :project))))
             (test-assert
              (search "Non-keyword symbol"
                      (princ-to-string
                      (task-agent-definition-error-cause condition)))
               "the native role reader starts from a fresh standard readtable"))
           (let ((bare-name "AUTOLITH-TASK-READER-BARE-LEAK-71D21A")
                 (qualified-name
                   "AUTOLITH-TASK-READER-QUALIFIED-LEAK-71D21A")
                 (keyword-name
                   "AUTOLITH-TASK-READER-KEYWORD-LEAK-71D21A"))
             (test-assert
              (and (null (find-symbol bare-name '#:autolith))
                   (null (find-symbol qualified-name '#:autolith))
                   (null (find-symbol keyword-name '#:keyword)))
              "reader-pollution sentinels begin absent from global packages")
             (task-tests--agent-definition-error
              (task-tests--write-text
               (merge-pathnames "bare-symbol.sexp" directory)
               "(:name \"bare-symbol\" :description \"Bare symbol\" :instructions \"Reject and forget it.\" :tools (autolith-task-reader-bare-leak-71d21a))")
              :project)
             (task-tests--agent-definition-error
              (task-tests--write-text
               (merge-pathnames "qualified-symbol.sexp" directory)
               "(:name \"qualified-symbol\" :description \"Qualified symbol\" :instructions \"Reject before interning it.\" :tools (autolith::autolith-task-reader-qualified-leak-71d21a))")
              :project)
             (task-tests--agent-definition-error
              (task-tests--write-text
               (merge-pathnames "unknown-keyword.sexp" directory)
               "(:name \"unknown-keyword\" :description \"Unknown keyword\" :instructions \"Reject before interning it.\" :autolith-task-reader-keyword-leak-71d21a t)")
              :project)
             (test-assert
              (and (null (find-symbol bare-name '#:autolith))
                   (null (find-symbol qualified-name '#:autolith))
                   (null (find-symbol keyword-name '#:keyword)))
              "malformed role symbols never pollute project or keyword packages"))
           (let* ((pathname
                    (merge-pathnames "utf8-bound.sexp" directory))
                  (condition
                    (task-tests--agent-definition-error
                     (task-tests--write-text
                      pathname
                      (make-string 65537 :initial-element #\é))
                     :project)))
             (test-assert
              (search "byte bound"
                      (princ-to-string
                       (task-agent-definition-error-cause condition)))
              "the native role file limit counts consumed UTF-8 bytes"))
           (dolist
               (case
                '(("extra"
                   "(:name \"extra\" :description \"Extra\" :instructions \"First\")\n(:second t)"
                   nil t)
                  ("incomplete"
                   "(:name \"incomplete\" :description \"Incomplete\" :instructions"
                   nil t)
                  ("shared"
                   "(:name \"shared\" :description #1=\"Shared\" :instructions #1#)"
                   nil nil)
                  ("circular"
                   "(:name \"circular\" :description \"Circular\" :instructions \"Reject cycles.\" :tools #1=(\"fs.read\" . #1#))"
                   nil nil)
                  ("dotted"
                   "(:name \"dotted\" :description \"Dotted\" :instructions \"Reject tails.\" . :tail)"
                   nil t)
                  ("unknown"
                   "(:name \"unknown\" :description \"Unknown\" :instructions \"Reject fields.\" :type :string)"
                   :type t)
                  ("duplicate"
                   "(:name \"duplicate\" :description \"First\" :instructions \"Reject duplicates.\" :description \"Second\")"
                   :description t)))
             (destructuring-bind
                 (name contents expected-field expected-line-p)
                 case
               (let* ((pathname
                        (merge-pathnames
                         (format nil "~A.sexp" name)
                         directory))
                      (condition
                        (task-tests--agent-definition-error
                         (task-tests--write-text pathname contents)
                         :project)))
                 (test-assert
                  (and (typep condition 'task-agent-definition-error)
                       (equal (task-agent-definition-error-pathname condition)
                              pathname)
                       (eq (task-agent-definition-error-source condition)
                           :project)
                       (string=
                        (task-agent-definition-error-definition-name condition)
                        name)
                       (task-agent-definition-error-cause condition)
                       (if expected-field
                           (eq (task-agent-definition-error-field condition)
                               expected-field)
                           t)
                       (if expected-line-p
                           (typep (task-agent-definition-error-line condition)
                                  '(integer 1))
                           t))
                   (format nil
                           "~A native role input returns complete typed diagnostic metadata"
                           name))))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)


(-> test-task-agent-discovery-precedence () null)
(defun test-task-agent-discovery-precedence ()
  "Test project, user, and bundled role precedence remains fail-closed."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (configuration
           (configuration--clone configuration :working-directory root))
         (project-directory (merge-pathnames ".autolith/agents/" root))
         (user-directory
           (merge-pathnames "agents/"
                            (configuration-config-root configuration))))
    (unwind-protect
         (progn
           (task-tests--write-native-form
            (merge-pathnames "scout.sexp" project-directory)
            (task-tests--role-form
             "scout" "Project scout" "Project instructions."))
           (task-tests--write-native-form
            (merge-pathnames "scout.sexp" user-directory)
            (task-tests--role-form
             "scout" "User scout" "User instructions."))
           (task-tests--write-native-form
            (merge-pathnames "reviewer.sexp" user-directory)
            (task-tests--role-form
             "reviewer" "User reviewer" "Review as configured by the user."))
           (task-tests--write-text
            (merge-pathnames "sonic.sexp" project-directory)
            "(:name \"sonic\" :description \"Missing instructions\")")
           (task-tests--write-native-form
            (merge-pathnames "sonic.sexp" user-directory)
            (task-tests--role-form
             "sonic" "User sonic" "This lower role must stay blocked."))
           (dolist (filename '("dupe.sexp" "DUPE.sexp"))
             (task-tests--write-native-form
              (merge-pathnames filename project-directory)
              (task-tests--role-form
               "dupe" "Duplicate role" "Reject normalized duplicates.")))
           (task-tests--write-native-form
            (merge-pathnames "dupe.sexp" user-directory)
            (task-tests--role-form
             "dupe" "Lower duplicate" "This lower role must stay blocked."))
           (multiple-value-bind (definitions diagnostics)
               (task-discover-agents configuration)
             (let ((scout
                     (task-find-agent-definition definitions "scout"))
                   (reviewer
                     (task-find-agent-definition definitions "reviewer"))
                   (librarian
                     (task-find-agent-definition definitions "librarian"))
                   (sonic-diagnostic
                     (task-find-agent-diagnostic diagnostics "sonic"))
                   (dupe-diagnostic
                     (task-find-agent-diagnostic diagnostics "dupe")))
               (test-assert
                (and scout
                     (eq (task-agent-definition-source scout) :project)
                     (string= (task-agent-definition-instructions scout)
                              "Project instructions."))
                "project .sexp roles override user and bundled roles")
               (test-assert
                (and reviewer
                     (eq (task-agent-definition-source reviewer) :user))
                "user .sexp roles override bundled roles")
               (test-assert
                (and librarian
                     (eq (task-agent-definition-source librarian) :bundled))
                "unclaimed roles retain their bundled definitions")
               (test-assert
                (and (null (task-find-agent-definition definitions "sonic"))
                     sonic-diagnostic
                     (eq (task-agent-definition-error-source sonic-diagnostic)
                         :project)
                     (eq (task-agent-definition-error-field sonic-diagnostic)
                         :instructions))
                "a malformed higher-precedence role blocks only its own name")
               (test-assert
                (and (task-find-agent-definition definitions "scout")
                     (task-find-agent-definition definitions "reviewer")
                     (task-find-agent-definition definitions "librarian"))
                "one blocked role does not suppress unrelated definitions")
               (test-assert
                (and (null (task-find-agent-definition definitions "dupe"))
                     dupe-diagnostic
                     (eq (task-agent-definition-error-source dupe-diagnostic)
                         :project)
                     (eq (task-agent-definition-error-field dupe-diagnostic)
                         :name)
                     (search "same normalized role name"
                             (princ-to-string
                              (task-agent-definition-error-cause
                               dupe-diagnostic))))
                "case-normalized duplicate filenames fail closed before parsing"))))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-agents-tool () null)
(defun test-task-agents-tool ()
  "Test native role discovery, policy filtering, diagnostics, and secrecy."
  (let* ((base-configuration (test-configuration))
         (root               (test-configuration-root base-configuration))
         (configuration
           (configuration--clone base-configuration :working-directory root))
         (project-directory (merge-pathnames ".autolith/agents/" root))
         (hidden-broken-path
           (merge-pathnames "hidden-broken.sexp" project-directory))
         (secret
           "AUTOLITH-TASK-AGENT-INSTRUCTION-SENTINEL-71D21A")
         (registry
           (task-augment-tool-registry (make-default-tool-registry))))
    (unwind-protect
         (progn
           (task-tests--write-native-form
            (merge-pathnames "allowed.sexp" project-directory)
            (task-tests--role-form
             "allowed" "An explicitly spawnable role." secret))
           (task-tests--write-native-form
            (merge-pathnames "denied.sexp" project-directory)
            (task-tests--role-form
             "denied" "A role outside the child policy."
             "This instruction must not matter."))
           (task-tests--write-text
            (merge-pathnames "blocked.sexp" project-directory)
            "(:name \"blocked\" :description \"Missing instructions\")")
           (task-tests--write-text
            hidden-broken-path
            "(:name \"hidden-broken\" :description \"Private malformed role\" :instructions 177771)")
           (let* ((primary
                    (task-tests--primary-agent
                     configuration "agents-primary" registry))
                  (tool (tool-registry-find registry "task" "agents")))
             (test-assert tool
                          "the default registry exposes task.agents")
             (let ((orchestrator (task-agents-tool-orchestrator tool)))
               (labels
                   ((invoke (selected-tool viewer offset limit)
                      "Execute task.agents and return its result and native form."
                      (let* ((context
                               (make-instance
                                'tool-context
                                :configuration (agent-configuration viewer)
                                :worker nil
                                :conversation (agent-conversation viewer)
                                :registry (agent-tool-registry viewer)
                                :agent viewer))
                             (result
                               (tool-execute
                                selected-tool context
                                (json-object "offset" offset "limit" limit)))
                             (form
                               (task-tests--read-exact-native-value
                                (tool-result-content result))))
                        (values result form)))

                    (entry (form name kind)
                      "Return the native entry named NAME with KIND from FORM."
                      (find-if
                       (lambda (record)
                         (and (eq (getf record :kind) kind)
                              (string= (getf record :name) name)))
                       (getf (rest form) :entries)))

                    (field-present-p (record field)
                      "Return true when FIELD occurs as a key in RECORD."
                      (loop for tail on record by #'cddr
                            thereis (eq (first tail) field)))

                    (run-report (viewer selected-registry agent-name)
                      "Invoke task.run as VIEWER and return its failure report."
                      (let* ((context
                               (make-instance
                                'tool-context
                                :configuration (agent-configuration viewer)
                                :worker nil
                                :conversation (agent-conversation viewer)
                                :registry selected-registry
                                :agent viewer))
                             (result
                               (tool-registry-execute-call
                                selected-registry
                                (json-object
                                 "namespace" "task"
                                 "name" "run"
                                 "arguments"
                                 (json-encode
                                  (json-object
                                   "agent" agent-name
                                   "task" "Exercise spawn-policy secrecy.")))
                                context)))
                        (test-assert
                         (not (tool-result-success-p result))
                         "a disallowed role request fails through registry dispatch")
                        (tool-result-content result))))
                 (multiple-value-bind (result form)
                     (invoke tool primary 0 *task-agent-page-maximum*)
                   (let ((allowed (entry form "allowed" :agent))
                         (denied (entry form "denied" :agent))
                         (blocked (entry form "blocked" :diagnostic)))
                     (test-assert
                      (and (equal form (tool-result-details result))
                           (eq (first form) :task-agents)
                           allowed denied blocked
                           (eq (getf allowed :source) :project)
                           (getf allowed :pathname)
                           (eq (getf blocked :field) :instructions))
                      "primary task.agents returns exact native role and diagnostic metadata")
                     (test-assert
                      (every
                       (lambda (field) (field-present-p allowed field))
                       '(:description :source :pathname :models
                         :reasoning-effort :tools :spawns
                         :output-contract-p :blocking-p))
                      "role discovery exposes stable policy and source fields")
                     (test-assert
                      (and (not (field-present-p allowed :instructions))
                           (null (search secret (tool-result-content result))))
                      "task.agents never exposes role instructions")))
                 (multiple-value-bind (result form)
                     (invoke tool primary 0 1)
                   (declare (ignore result))
                   (test-assert
                    (and (= (getf (rest form) :offset) 0)
                         (= (getf (rest form) :count) 1)
                         (> (getf (rest form) :total) 1)
                         (= (getf (rest form) :next-offset) 1)
                         (= (length (getf (rest form) :entries)) 1))
                    "task.agents paginates native discovery without clipping forms"))
                 (let* ((definition
                          (task-agent-definition-create
                           :name "spawn-parent"
                           :description "Permit two role names."
                           :instructions "Exercise child discovery policy."
                           :tools :all
                           :spawns '("allowed" "blocked")
                           :source :test))
                        (job
                          (task-tests--register-job
                           orchestrator primary definition
                           :name "spawn-parent"))
                        (child-registry
                          (task-child-tool-registry
                           registry definition orchestrator 1))
                        (child
                          (task-tests--child-viewer
                           configuration job :registry child-registry))
                        (child-tool
                          (tool-registry-find child-registry "task" "agents")))
                   (test-assert
                    child-tool
                    "a child allowed to delegate inherits task.agents")
                   (multiple-value-bind (result form)
                       (invoke child-tool child 0 *task-agent-page-maximum*)
                     (let ((entries (getf (rest form) :entries)))
                       (test-assert
                        (and
                         (equal
                          (sort (mapcar (lambda (entry)
                                          (getf entry :name))
                                        entries)
                                #'string<)
                          '("allowed" "blocked"))
                         (entry form "allowed" :agent)
                         (entry form "blocked" :diagnostic)
                         (null (entry form "denied" :agent))
                         (null (search secret (tool-result-content result))))
                        "child task.agents shows only spawnable roles and reserved-name diagnostics")))
                   (let ((unknown-report
                           (run-report child child-registry
                                       "unlisted-request"))
                         (malformed-report
                           (run-report child child-registry
                                       "hidden-broken"))
                         (expected
                           "task.run failed: The current agent may not spawn the requested role."))
                     (test-assert
                      (and (string= unknown-report expected)
                           (null (search "Available agents" unknown-report))
                           (null (search "allowed" unknown-report))
                           (null (search "denied" unknown-report)))
                      "disallowed unknown roles cannot enumerate discovered roles")
                     (test-assert
                      (and (string= malformed-report expected)
                           (null
                            (search (namestring hidden-broken-path)
                                    malformed-report))
                           (null (search "hidden-broken.sexp"
                                         malformed-report))
                           (null (search "177771" malformed-report))
                           (null (search "instructions" malformed-report
                                         :test #'char-equal)))
                      "disallowed malformed roles reveal neither pathname nor parse cause")))
                 (dolist
                     (case
                      (list
                       (list
                        "no-spawn"
                        (task-agent-definition-create
                         :name "no-spawn"
                         :description "Permit no descendants."
                         :instructions "Do not delegate."
                         :spawns nil
                         :source :test)
                        1)
                       (list
                        "max-depth"
                        (task-agent-definition-create
                         :name "max-depth"
                         :description "Reach the configured depth."
                         :instructions "Do not exceed the depth limit."
                         :spawns :all
                         :source :test)
                        (task-orchestrator-maximum-depth orchestrator))))
                   (destructuring-bind (name definition depth) case
                     (let* ((job
                              (task-tests--register-job
                               orchestrator primary definition :name name))
                            (child-registry
                              (task-child-tool-registry
                               registry definition orchestrator depth))
                            (child
                              (task-tests--child-viewer
                               configuration job
                               :depth depth
                               :registry child-registry)))
                       (test-assert
                        (null
                         (tool-registry-find
                          child-registry "task" "agents"))
                        (format nil
                                "~A child does not inherit task.agents"
                                name))
                       (multiple-value-bind (result form)
                           (invoke tool child 0 *task-agent-page-maximum*)
                         (declare (ignore result))
                         (test-assert
                          (and (zerop (getf (rest form) :total))
                               (null (getf (rest form) :entries)))
                          (format nil
                                  "~A child has no discoverable spawn targets"
                                  name))))))))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-tool-default-argument-types () null)
(defun test-task-tool-default-argument-types ()
  "Test that explicit JSON false and null never become omitted task defaults."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration))
         (registry
           (task-augment-tool-registry (make-default-tool-registry)))
         (primary
           (task-tests--primary-agent
            configuration "task-default-types" registry))
         (conversation (agent-conversation primary))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation conversation
                          :registry registry
                          :agent primary))
         (orchestrator
           (task-run-tool-orchestrator
            (tool-registry-find registry "task" "run")))
         (definition
           (task-agent-definition-create
            :name "default-types"
            :description "Exercise explicit invalid default values."
            :instructions "Remain terminal while job.wait validates."
            :source :test))
         (job
           (task-tests--register-job
            orchestrator primary definition :name "default-types"))
         (job-result
           (task-tests--terminal-result
            job :status :success :output "already terminal")))
    (unwind-protect
         (progn
           (task-job--publish-terminal job :completed job-result)
           (labels ((rejected-p (namespace name arguments)
                      "Return true when actual registry dispatch rejects ARGUMENTS."
                      (not
                       (tool-result-success-p
                        (tool-registry-execute-call
                         registry
                         (json-object "namespace" namespace
                                      "name" name
                                      "arguments" arguments)
                         context)))))
             (dolist
                 (case
                  '(("task" "agents" "{\"offset\":false}")
                    ("task" "agents" "{\"offset\":null}")
                    ("task" "agents" "{\"limit\":false}")
                    ("task" "agents" "{\"limit\":null}")
                    ("job" "list" "{\"offset\":false}")
                    ("job" "list" "{\"offset\":null}")
                    ("job" "list" "{\"limit\":false}")
                    ("job" "list" "{\"limit\":null}")))
               (destructuring-bind (namespace name arguments) case
                 (test-assert
                  (rejected-p namespace name arguments)
                  (format nil
                          "~A.~A rejects explicit non-integer default input ~A"
                          namespace name arguments))))
             (dolist (value '("false" "null"))
               (test-assert
                (rejected-p
                 "job" "wait"
                 (format nil
                         "{\"id\":~A,\"timeout-seconds\":~A}"
                         (json-encode (getf (task-job-identity job) :id))
                         value))
                (format nil
                        "job.wait rejects explicit timeout-seconds ~A"
                        value)))))
      (ignore-errors (tool-registry-close-runtime-state registry))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)

(-> test-task-native-output-contracts () null)
(defun test-task-native-output-contracts ()
  "Test recursive native output schemas and exact JSON boundary conversion."
  (let* ((schema
           (task-output-schema-normalize
            '(:type :object
              :properties
              (("enabled" (:type :boolean))
               ("nothing" (:type :null))
               ("items"
                (:type :array
                 :items
                 (:type :object
                  :properties
                  (("name" (:type :string))
                   ("score" (:type :number)))
                  :required ("name")
                  :additional-properties nil)
                 :min-items 1
                 :max-items 2)))
              :required ("enabled" "nothing" "items")
              :additional-properties nil)
            :source :programmatic
            :definition-name "recursive"))
         (provider-schema (task-output-schema->json schema))
         (candidate
           (task-json-decode
            "{\"enabled\":false,\"nothing\":null,\"items\":[{\"name\":\"one\",\"score\":1.5}]}")))
    (test-assert (task-output-schema-valid-p candidate schema)
                 "recursive provider JSON satisfies its native output DSL")
    (test-assert
     (and (string= (json-get provider-schema "type") "object")
          (eq (json-get provider-schema "additionalProperties") false)
          (vectorp (json-get provider-schema "required"))
          (string=
           (json-get
            (json-get
             (json-get (json-get provider-schema "properties") "items")
             "items")
            "type")
           "object"))
     "native recursive schemas convert to JSON only at the provider boundary")
    (test-assert
     (not
      (task-output-schema-valid-p
       (task-json-decode
        "{\"enabled\":null,\"nothing\":false,\"items\":[{\"name\":\"one\"}]}")
       schema))
     "recursive validation keeps JSON false distinct from JSON null")
    (test-assert
     (not
      (task-output-schema-valid-p
       (task-json-decode
        "{\"enabled\":false,\"nothing\":null,\"items\":[{\"name\":\"one\",\"extra\":1}]}")
       schema))
     "recursive validation enforces nested additional-property policy"))
  (let* ((enum-schema
           (task-output-schema-normalize
            '(:enum (nil :null t))
            :source :programmatic
            :definition-name "enum"))
         (provider-enum
           (json-get (task-output-schema->json enum-schema) "enum")))
    (test-assert
     (and (= (length provider-enum) 3)
          (eq (aref provider-enum 0) false)
          (eq (aref provider-enum 1) :null)
          (eq (aref provider-enum 2) t))
     "native NIL and :NULL become distinct JSON false and null enum values"))
  (dolist
      (case
       '(((:type :array) :items)
         ((:type :object
           :properties (("known" (:type :string)))
           :required ("missing"))
          :required)
         ((:type :object
           :properties
           (("same" (:type :string))
            ("same" (:type :integer))))
          :properties)
         ((:type :boolean :enum (nil :null)) :enum)))
    (destructuring-bind (invalid-schema expected-field) case
      (let ((condition
              (handler-case
                  (progn
                    (task-output-schema-normalize
                     invalid-schema
                     :source :programmatic
                     :definition-name "invalid-output")
                    nil)
                (task-agent-definition-error (error)
                  error))))
        (test-assert
         (and condition
              (eq (task-agent-definition-error-field condition)
                  expected-field)
              (eq (task-agent-definition-error-source condition)
                  :programmatic)
              (string=
               (task-agent-definition-error-definition-name condition)
               "invalid-output"))
         (format nil "invalid recursive output field ~S has a typed diagnostic"
                 expected-field)))))
  (let* ((provider-value
           (task-json-decode
            "{\"z\":null,\"a\":[false,true,{\"quote\":\"a\\\"b\\n\"}],\"n\":2.5}"))
         (native-value (task-json->sexp provider-value))
         (entries (rest native-value))
         (array (second (assoc "a" entries :test #'string=))))
    (test-assert
     (and (eq (first native-value) :object)
          (equal (mapcar #'first entries) '("a" "n" "z"))
          (eq (first array) :array)
          (null (second array))
          (eq (third array) t)
          (eq (second (assoc "z" entries :test #'string=)) :null))
     "provider JSON becomes sorted tagged readable s-expression data")
    (test-assert
     (equal native-value
            (task-json->sexp (task-sexp->json native-value)))
     "tagged objects, arrays, false, null, numbers, and escaped strings round trip")
    (test-assert
     (handler-case
         (progn
           (task-sexp->json
            '(:object ("duplicate" 1) ("duplicate" 2)))
           nil)
       (task-error ()
         t))
     "tagged native objects reject duplicate keys during reconstruction")
    (test-assert
     (handler-case
         (progn
           (task-sexp->json '("untagged" nil :null))
           nil)
       (task-error ()
         t))
     "untagged lists cannot cross the durable task result boundary")
    (test-assert
     (handler-case
         (progn
           (task-json-decode
            "{\"complete\":true} false"
            :tool-name "yield.submit")
           nil)
       (task-error (condition)
         (string= (tool-error-tool-name condition) "yield.submit")))
     "task JSON decoding rejects trailing values with canonical tool metadata"))
  nil)

(-> test-task-yield-contract () null)
(defun test-task-yield-contract ()
  "Test exact yield semantics through provider JSON argument decoding."
  (let* ((configuration (test-configuration))
         (root          (test-configuration-root configuration)))
    (unwind-protect
         (progn
           (let* ((definition
                    (task-agent-definition-create
                     :name "boolean-output"
                     :description "Return a boolean."
                     :instructions "Yield one explicit boolean."
                     :output '(:type :boolean)
                     :source :test))
                  (fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-false"))
                  (completion (getf fixture :completion))
                  (result
                    (task-tests--execute-yield
                     fixture
                     "{\"status\":\"success\",\"text\":\"false value\",\"data\":false}")))
             (test-assert
              (and (tool-result-success-p result)
                   (task-completion-called-p completion)
                   (eq (task-completion-status completion) :success)
                   (task-completion-data-present-p completion)
                   (eq (task-completion-data completion) false)
                   (null
                    (task-json->sexp
                     (task-completion-data completion))))
              "registry decoding preserves an explicitly supplied JSON false")
             (let ((durable
                     (task--assemble-child-result
                      (getf fixture :job)
                      (agent-test-result "yield-false-result" nil)
                      (getf fixture :child)
                      (getf fixture :conversation)
                      completion)))
               (test-assert
                (and (getf durable :structured-output-present-p)
                     (task--plist-key-present-p durable :structured-output)
                     (null (getf durable :structured-output)))
                "durable task results tag false as NIL with an explicit presence bit"))
             (test-assert
              (not
               (tool-result-success-p
                (task-tests--execute-yield
                 fixture
                 "{\"status\":\"success\",\"data\":true}")))
              "yield.submit rejects every call after the exact terminal yield"))
           (let* ((definition
                    (task-agent-definition-create
                     :name "null-output"
                     :description "Return null."
                     :instructions "Yield one explicit null."
                     :output '(:type :null)
                     :source :test))
                  (fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-null"))
                  (completion (getf fixture :completion))
                  (result
                    (task-tests--execute-yield
                     fixture
                     "{\"status\":\"success\",\"data\":null}")))
             (test-assert
              (and (tool-result-success-p result)
                   (task-completion-data-present-p completion)
                   (eq (task-completion-data completion) :null)
                   (eq (task-json->sexp
                        (task-completion-data completion))
                       :null))
              "registry decoding preserves JSON null separately from false"))
           (let* ((definition
                    (task-agent-definition-create
                     :name "optional-output"
                     :description "Return optional data."
                     :instructions "Yield a concise result."
                     :source :test))
                  (fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-absent"))
                  (completion (getf fixture :completion))
                  (result
                    (task-tests--execute-yield
                     fixture
                     "{\"status\":\"success\",\"text\":\"no structured data\"}")))
             (test-assert
              (and (tool-result-success-p result)
                   (not (task-completion-data-present-p completion))
                   (null (task-completion-data completion)))
              "absent yield data remains distinct from explicit false"))
           (dolist
               (case
                '(("required-missing"
                   (:type :boolean)
                   "{\"status\":\"success\"}")
                  ("success-error"
                   nil
                   "{\"status\":\"success\",\"error\":\"impossible\"}")
                  ("success-empty"
                   nil
                   "{\"status\":\"success\",\"text\":\" \\t\\n \"}")
                  ("unknown-field"
                   nil
                   "{\"status\":\"success\",\"text\":\"done\",\"legacy\":true}")
                  ("failed-with-data"
                   nil
                   "{\"status\":\"failed\",\"error\":\"blocked\",\"data\":false}")
                  ("failed-empty-error"
                   nil
                   "{\"status\":\"failed\",\"error\":\"\"}")
                  ("failed-blank-error"
                   nil
                   "{\"status\":\"failed\",\"error\":\" \\t \"}")
                  ("aborted-no-error"
                   nil
                   "{\"status\":\"aborted\"}")
                  ("status-case"
                   nil
                   "{\"status\":\"Success\"}")
                  ("non-string-text"
                   nil
                   "{\"status\":\"success\",\"text\":null}")))
             (destructuring-bind (name output arguments) case
               (let* ((definition
                        (task-agent-definition-create
                         :name name
                         :description "Exercise one invalid terminal yield."
                         :instructions "Follow the exact yield contract."
                         :output output
                         :source :test))
                      (fixture
                        (task-tests--yield-fixture
                         configuration definition name))
                      (result
                        (task-tests--execute-yield fixture arguments)))
                 (test-assert
                  (and (not (tool-result-success-p result))
                       (not
                        (task-completion-called-p
                         (getf fixture :completion))))
                  (format nil "yield contract rejects ~A without terminal mutation"
                          name)))))
           (let* ((definition
                    (task-agent-definition-create
                     :name "bounded-label"
                     :description "Exercise the terminal label bound."
                     :instructions "Yield one bounded label."
                     :source :test))
                  (oversized-fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-oversized-label"))
                  (oversized-result
                    (task-tests--execute-yield
                     oversized-fixture
                     (json-encode
                      (json-object
                       "status" "success"
                       "text" "done"
                       "label"
                       (make-string
                        (1+ *task-result-label-maximum-characters*)
                        :initial-element #\L))))))
             (test-assert
              (and (not (tool-result-success-p oversized-result))
                   (not
                    (task-completion-called-p
                     (getf oversized-fixture :completion))))
              "yield.submit rejects labels beyond the terminal retention bound")
             (let* ((fixture
                      (task-tests--yield-fixture
                       configuration definition "yield-bounded-label"))
                    (label
                      (make-string
                       *task-result-label-maximum-characters*
                       :initial-element #\L))
                    (result
                      (task-tests--execute-yield
                       fixture
                       (json-encode
                        (json-object "status" "success"
                                     "text" "done"
                                     "label" label))))
                    (durable
                      (task--assemble-child-result
                       (getf fixture :job)
                       (agent-test-result "bounded-label-result" nil)
                       (getf fixture :child)
                       (getf fixture :conversation)
                       (getf fixture :completion))))
               (test-assert
                (and (tool-result-success-p result)
                     (task-job--publish-terminal
                      (getf fixture :job) :completed durable)
                     (string=
                      (getf (task-job-result (getf fixture :job)) :label)
                      label))
                "a maximum-length yield label survives terminal compaction")))
           (let* ((definition
                    (task-agent-definition-create
                     :name "failed-result"
                     :description "Report a failure."
                     :instructions "Yield one explained failure."
                     :source :test))
                  (fixture
                    (task-tests--yield-fixture
                     configuration definition "yield-failed"))
                  (completion (getf fixture :completion))
                  (result
                    (task-tests--execute-yield
                     fixture
                     "{\"status\":\"failed\",\"error\":\"dependency unavailable\"}")))
             (test-assert
              (and (tool-result-success-p result)
                   (task-completion-called-p completion)
                   (eq (task-completion-status completion) :failed)
                   (string= (task-completion-error completion)
                            "dependency unavailable")
                   (not (task-completion-data-present-p completion)))
              "an explained failed yield is an accepted terminal result")))
      (uiop:delete-directory-tree root :validate t
                                       :if-does-not-exist :ignore)))
  nil)
