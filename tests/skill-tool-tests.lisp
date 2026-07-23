(in-package #:autolith)

;;;; -- Skill Selection Tool Tests --

(-> skill-tool-tests--write (pathname string string) pathname)
(defun skill-tool-tests--write (root relative-path content)
  "Write CONTENT beneath ROOT at RELATIVE-PATH and return its pathname."
  (let ((pathname (merge-pathnames relative-path root)))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede
                            :external-format :utf-8)
      (write-string content stream))
    pathname))

(-> skill-tool-tests--call
    (tool-registry tool-context string)
    tool-result)
(defun skill-tool-tests--call (registry context name)
  "Call skill.load through REGISTRY with exact NAME."
  (tool-registry-execute-call
   registry
   (json-object
    "namespace" "skill"
    "name" "load"
    "arguments" (json-encode (json-object "name" name)))
   context))

(-> skill-tool-tests--contribution
    (list string)
    (option context-contribution))
(defun skill-tool-tests--contribution (contributions identifier)
  "Return the contribution named IDENTIFIER from CONTRIBUTIONS."
  (find identifier
        contributions
        :key #'context-contribution-identifier
        :test #'string=))

(-> test-skill-load-tool () null)
(defun test-skill-load-tool ()
  "Test exact, ephemeral, child-safe Skill selection through skill.load."
  (let* ((base-configuration (test-configuration))
         (root (test-configuration-root base-configuration))
         (project (merge-pathnames "project/" root))
         (skill-root (merge-pathnames ".autolith/skills/" project))
         (secret-body
           "FOLLOW-THE-ALPHA-INSTRUCTION-BODY-ONLY-IN-REQUEST-CONTEXT")
         (configuration
           (progn
             (ensure-directories-exist
              (merge-pathnames ".git/marker" project))
             (configuration-with-working-directory
              base-configuration
              project)))
         (conversation
           (conversation-create configuration
                                :identifier "skill-load-tool"))
         (registry
           (skill-augment-tool-registry
            (make-instance 'tool-registry)))
         (tool (tool-registry-find registry "skill" "load"))
         (context
           (make-instance 'tool-context
                          :configuration configuration
                          :worker nil
                          :conversation conversation
                          :registry registry)))
    (unwind-protect
         (progn
           (skill-tool-tests--write
            skill-root
            "alpha/SKILL.sexp"
            (format nil
                    "(:autolith-skill :version 1 :name \"alpha\" :description \"Apply the alpha workflow.\" :instructions ~S)~%"
                    secret-body))
           (skill-tool-tests--write
            skill-root
            "oversized/SKILL.sexp"
            (format nil
                    "(:autolith-skill :version 1 :name \"oversized\" :description \"Exercise deferred instruction reading.\" :instructions ~S)~%"
                    (make-string 256 :initial-element #\x)))
           (test-assert tool
                        "skill registry augmentation installs skill.load")
           (test-assert (eq tool
                            (tool-registry-find
                             (skill-augment-tool-registry registry)
                             "skill"
                             "load"))
                        "skill registry augmentation is idempotent")
           (test-assert (tool-child-safe-p tool)
                        "skill.load is available across the child-agent boundary")
           (test-assert
            (and (eq (tool-conversation-persistence tool) ':next-response)
                 (tool-provider-round-trip-barrier-p tool))
            "skill.load declares request-local persistence and a provider barrier")
           (let ((schema (tool-provider-schema tool)))
             (test-assert
              (and (string= (json-get schema "name") "load")
                   (equal (coerce
                           (json-get (json-get schema "parameters")
                                     "required")
                           'list)
                          '("name"))
                   (eq (json-get (json-get schema "parameters")
                                 "additionalProperties")
                       false))
              "skill.load exposes one required exact-name argument"))
           (let ((outside-turn
                   (skill-tool-tests--call registry context "alpha")))
             (test-assert
              (and (not (tool-result-success-p outside-turn))
                   (search "only while an agent turn is active"
                           (tool-result-content outside-turn)))
              "skill.load rejects selection that cannot survive a logical turn"))
           (let ((discovery-called-p nil)
                 (result nil))
             (test-call-with-function-replacements
              (list
               (list
                'skill-catalog-for-configuration
                (lambda (ignored-configuration)
                  (declare (ignore ignored-configuration))
                  (setf discovery-called-p t)
                  (error "Invalid names must not reach discovery."))))
              (lambda ()
                (setf result
                      (skill-tool-tests--call
                       registry
                       context
                       (make-string
                        (1+ *skill-name-character-limit*)
                        :initial-element #\a)))))
             (test-assert
              (and (not discovery-called-p)
                   (not (tool-result-success-p result))
                   (search "at most"
                           (tool-result-content result)))
              "skill.load rejects oversized names before filesystem discovery"))
           (call-with-skill-logical-turn
            (user-message-input-create :text "Use the relevant workflow.")
            (lambda ()
              (let* ((before
                       (skill-request-contributions
                        configuration
                        conversation))
                     (result
                       (skill-tool-tests--call
                        registry
                        context
                        "alpha")))
                (test-assert
                 (null
                  (skill-tool-tests--contribution
                   before
                   "skill-selected-alpha"))
                 "an implicit skill is absent before skill.load selects it")
                (test-assert
                 (tool-result-success-p result)
                 "skill.load selects an exact discovered skill")
                (test-assert
                (equal *skill-logical-turn-selection-names* '("alpha"))
                 "skill.load accumulates selection in logical-turn state")
                (test-assert
                 (and (< (length (tool-result-content result)) 256)
                      (not (search secret-body
                                   (tool-result-content result)))
                      (null (tool-result-details result))
                      (null (tool-result-image-attachments result)))
                 "the request-local tool result contains only bounded confirmation")
                (let* ((after
                         (skill-request-contributions
                          configuration
                          conversation))
                       (selected
                         (skill-tool-tests--contribution
                          after
                          "skill-selected-alpha")))
                  (test-assert
                   (and selected
                        (search
                         secret-body
                         (context-contribution-instruction selected)))
                   "subsequent requests in the turn receive the complete body ephemerally"))
                (let ((duplicate
                        (skill-tool-tests--call
                         registry
                         context
                         "alpha")))
                  (test-assert
                   (and (tool-result-success-p duplicate)
                        (search "already selected"
                                (tool-result-content duplicate))
                        (equal *skill-logical-turn-selection-names*
                               '("alpha")))
                   "repeated selection is idempotent"))
                (let ((wrong-case
                        (skill-tool-tests--call
                         registry
                         context
                         "Alpha")))
                  (test-assert
                   (and (not (tool-result-success-p wrong-case))
                        (search "lowercase ASCII letters"
                                (tool-result-content wrong-case))
                        (equal *skill-logical-turn-selection-names*
                               '("alpha")))
                   "skill.load rejects names that differ only by case")))))
           (let ((*skill-instruction-character-limit* 128))
             (call-with-skill-logical-turn
              (user-message-input-create :text "Use the large workflow.")
              (lambda ()
                (let ((result
                        (skill-tool-tests--call
                         registry
                         context
                         "oversized")))
                  (test-assert
                   (tool-result-success-p result)
                   "skill.load selects from metadata without reading the body")
                  (let ((warning
                          (skill-tool-tests--contribution
                           (skill-request-contributions
                            configuration
                            conversation)
                           "skill-warning-oversized")))
                    (test-assert
                     (and warning
                          (search "could not be read"
                                  (context-contribution-instruction warning)))
                     "deferred body failure becomes request-local warning"))))))
           (let ((catalog-text
                   (skill--catalog-instruction
                    (skill-catalog-for-configuration configuration))))
             (test-assert
             (and (search "call `skill.load`" catalog-text)
                   (search "Do not read SKILL.sexp through `fs.read`"
                           catalog-text)
                   (search "subsequent provider requests" catalog-text))
              "catalog guidance routes implicit selection through skill.load")))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  nil)
