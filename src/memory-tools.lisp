(in-package #:autolith)

;;;; -- Memory Tool Classes --

(defclass memory-tool (tool)
  ()
  (:documentation "A tool reading or updating Autolith's persistent memories."))

(defclass memory-remember-tool (memory-tool)
  ()
  (:documentation "Create or replace one persistent memory."))

(defclass memory-list-tool (memory-tool)
  ()
  (:documentation "List persistent memory metadata."))

(defclass memory-search-tool (memory-tool)
  ()
  (:documentation "Search persistent memory content and metadata."))

(defclass memory-read-tool (memory-tool)
  ()
  (:documentation "Read one complete persistent memory."))

(defclass memory-forget-tool (memory-tool)
  ()
  (:documentation "Stop recalling one persistent memory."))


;;;; -- Tool Arguments --

(defparameter *memory-search-default-results* 20
  "The default number of memory search results returned to the model.")

(defparameter *memory-search-maximum-results* 50
  "The largest memory result page returned to the model.")

(-> memory-tool--visibility (json-object) memory-visibility)
(defun memory-tool--visibility (arguments)
  "Return the validated visibility requested in ARGUMENTS."
  (let ((scope (or (tool-argument arguments "scope") "relevant")))
    (unless (stringp scope)
      (error 'tool-error
             :message "Memory scope must be a string."
             :tool-name "memory"))
    (cond
      ((string-equal scope "relevant") :relevant)
      ((string-equal scope "global") :global)
      ((string-equal scope "workspace") :workspace)
      ((string-equal scope "all") :all)
      (t
       (error 'tool-error
              :message "Memory scope must be relevant, global, workspace, or all."
              :tool-name "memory")))))

(-> memory-tool--write-scope (json-object) (option memory-scope))
(defun memory-tool--write-scope (arguments)
  "Return the optional global or workspace scope requested in ARGUMENTS."
  (let ((scope (tool-argument arguments "scope")))
    (cond
      ((null scope) nil)
      ((and (stringp scope) (string-equal scope "global")) :global)
      ((and (stringp scope) (string-equal scope "workspace")) :workspace)
      (t
       (error 'tool-error
              :message "memory.remember scope must be global or workspace."
              :tool-name "memory.remember")))))

(-> memory-tool--tags (json-object) list)
(defun memory-tool--tags (arguments)
  "Return the optional string tag array from ARGUMENTS as a list."
  (let ((tags (tool-argument arguments "tags")))
    (cond
      ((null tags)
       nil)
      ((and (vectorp tags)
            (every #'stringp tags))
       (coerce tags 'list))
      (t
       (error 'tool-error
              :message "Memory tags must be an array of strings."
              :tool-name "memory.remember")))))

(-> memory-tool--maximum-results (json-object) integer)
(defun memory-tool--maximum-results (arguments)
  "Return the clamped result count requested in ARGUMENTS."
  (let ((requested (tool-argument arguments "max-results")))
    (cond
      ((null requested)
       *memory-search-default-results*)
      ((and (integerp requested) (plusp requested))
       (min requested *memory-search-maximum-results*))
      (t
       (error 'tool-error
              :message "max-results must be a positive integer."
              :tool-name "memory")))))


;;;; -- Presentation --

(-> memory-tool--scope-label (memory) string)
(defun memory-tool--scope-label (memory)
  "Return a concise scope label for MEMORY."
  (if (eq (memory-scope memory) :global)
      "global"
      (format nil "workspace ~A" (memory-workspace memory))))

(-> memory-tool--summary (memory) string)
(defun memory-tool--summary (memory)
  "Return one compact metadata line for MEMORY."
  (format nil "~A  [~A]  ~A  ~A~@[  tags: ~{~A~^, ~}~]"
          (memory-identifier memory)
          (memory-tool--scope-label memory)
          (memory--timestamp-string (memory-updated-at memory))
          (memory-title memory)
          (and (memory-tags memory) (memory-tags memory))))

(-> memory-tool--render-list (list) string)
(defun memory-tool--render-list (memories)
  "Return compact lines describing MEMORIES."
  (if memories
      (format nil "~{~A~^~%~}" (mapcar #'memory-tool--summary memories))
      "No matching memories."))

(-> memory-tool--render-memory (memory) string)
(defun memory-tool--render-memory (memory)
  "Return complete model-visible MEMORY content and metadata."
  (format nil
          "id: ~A~%scope: ~A~%created: ~A~%updated: ~A~%source conversation: ~A~%title: ~A~%tags: ~:[none~;~:*~{~A~^, ~}~]~2%~A"
          (memory-identifier memory)
          (memory-tool--scope-label memory)
          (memory--timestamp-string (memory-created-at memory))
          (memory--timestamp-string (memory-updated-at memory))
          (if (memory-source-conversation memory)
              (conversation-identifier-display
               (memory-source-conversation memory))
              "unknown")
          (memory-title memory)
          (memory-tags memory)
          (memory-content memory)))


;;;; -- Tool Executions --

(defmethod tool-execute ((tool memory-remember-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Create or replace one durable memory from complete supplied content."
  (let ((title (tool-argument arguments "title" :required t))
        (content (tool-argument arguments "content" :required t))
        (identifier (tool-argument arguments "id")))
    (unless (and (stringp title)
                 (stringp content)
                 (or (null identifier) (stringp identifier)))
      (error 'tool-error
             :message "memory.remember requires string title, content, and optional id."
             :tool-name "memory.remember"))
    (let ((memory
            (memory-remember
             (tool-context-configuration context)
             :identifier identifier
             :title title
             :content content
             :scope (memory-tool--write-scope arguments)
             :tags (memory-tool--tags arguments)
             :source-conversation
             (conversation-identifier (tool-context-conversation context)))))
      (tool-success
       (format nil "~:[Created~;Updated~] memory ~A."
               (and identifier t)
               (memory-identifier memory))))))

(defmethod tool-execute ((tool memory-list-tool)
                         (context tool-context)
                         (arguments hash-table))
  "List memories from the requested scope."
  (let* ((configuration (tool-context-configuration context))
         (memories (memory-list
                    configuration
                    :visibility (memory-tool--visibility arguments)))
         (limit (memory-tool--maximum-results arguments)))
    (tool-success
     (memory-tool--render-list
      (subseq memories 0 (min limit (length memories)))))))

(defmethod tool-execute ((tool memory-search-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Search titles, content, tags, and workspace names by lexical relevance."
  (let ((query (tool-argument arguments "query" :required t)))
    (unless (stringp query)
      (error 'tool-error
             :message "memory.search requires a string query."
             :tool-name "memory.search"))
    (let* ((memories
             (memory-search
              (tool-context-configuration context)
              query
              :visibility (memory-tool--visibility arguments)))
           (limit (memory-tool--maximum-results arguments)))
      (tool-success
       (memory-tool--render-list
        (subseq memories 0 (min limit (length memories))))))))

(defmethod tool-execute ((tool memory-read-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Read one complete active memory by identifier."
  (let ((identifier (tool-argument arguments "id" :required t)))
    (unless (non-empty-string-p identifier)
      (error 'tool-error
             :message "memory.read requires a non-empty string id."
             :tool-name "memory.read"))
    (let ((memory (memory-find (tool-context-configuration context) identifier)))
      (if memory
          (tool-success (memory-tool--render-memory memory))
          (tool-failure (format nil "Memory ~A does not exist." identifier))))))

(defmethod tool-execute ((tool memory-forget-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Stop recalling one memory by appending a durable tombstone."
  (let ((identifier (tool-argument arguments "id" :required t)))
    (unless (non-empty-string-p identifier)
      (error 'tool-error
             :message "memory.forget requires a non-empty string id."
             :tool-name "memory.forget"))
    (memory-forget (tool-context-configuration context) identifier)
    (tool-success (format nil "Forgot memory ~A." identifier))))
