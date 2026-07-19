(in-package #:autolith)

;;;; -- Agenda Tool Classes --

(defclass agenda-tool (tool)
  ()
  (:documentation "A tool reading or updating persistent workspace agendas."))

(defclass agenda-list-tool (agenda-tool)
  ()
  (:documentation "A tool reading the complete current workspace agenda."))

(defclass agenda-add-tool (agenda-tool)
  ()
  (:documentation "A tool adding one current workspace agenda item."))

(defclass agenda-update-tool (agenda-tool)
  ()
  (:documentation "A tool replacing fields of one current agenda item."))

(defclass agenda-remove-tool (agenda-tool)
  ()
  (:documentation "A tool removing one current agenda item."))

(defclass agenda-transport-tool (agenda-tool)
  ()
  (:documentation "A tool inspecting, copying, or moving workspace agendas."))


;;;; -- Tool Arguments --

(-> agenda-tool--status (json-object &key (:required boolean))
    (option agenda-status))
(defun agenda-tool--status (arguments &key required)
  "Return the validated agenda status in ARGUMENTS."
  (multiple-value-bind (value present-p)
      (gethash "status" arguments)
    (when (and required (not present-p))
      (error 'tool-error
             :message "An agenda status is required."
             :tool-name "agenda"))
    (cond
      ((not present-p)
       nil)
      ((not (stringp value))
       (error 'tool-error
              :message "Agenda status must be a string."
              :tool-name "agenda"))
      ((string-equal value "todo") ':todo)
      ((string-equal value "doing") ':doing)
      ((string-equal value "blocked") ':blocked)
      ((string-equal value "done") ':done)
      ((string-equal value "note") ':note)
      (t
       (error 'tool-error
              :message "Agenda status must be todo, doing, blocked, done, or note."
              :tool-name "agenda")))))

(-> agenda-tool--string-argument
    (json-object string string &key (:required boolean))
    (option string))
(defun agenda-tool--string-argument
    (arguments name tool-name &key required)
  "Return the optional string NAME in ARGUMENTS for TOOL-NAME."
  (multiple-value-bind (value present-p)
      (gethash name arguments)
    (when (and required (not present-p))
      (error 'tool-error
             :message (format nil "~A requires ~A." tool-name name)
             :tool-name tool-name))
    (when (and present-p (not (non-empty-string-p value)))
      (error 'tool-error
             :message (format nil "~A must be a non-empty string." name)
             :tool-name tool-name))
    value))

(-> agenda-tool--memory-identifiers
    (json-object string)
    (values list boolean))
(defun agenda-tool--memory-identifiers (arguments tool-name)
  "Return TOOL-NAME's memory-ids array and whether it was supplied."
  (multiple-value-bind (value present-p)
      (gethash "memory-ids" arguments)
    (cond
      ((not present-p)
       (values nil nil))
      ((and (vectorp value)
            (every #'non-empty-string-p value))
       (values (coerce value 'list) t))
      (t
       (error 'tool-error
              :message "memory-ids must be an array of non-empty strings."
              :tool-name tool-name)))))


;;;; -- Presentation --

(-> agenda-tool--render-item (agenda-item) string)
(defun agenda-tool--render-item (item)
  "Return complete model-visible data for agenda ITEM."
  (format nil "~A  [~(~A~)]  ~A~@[  memories: ~{~A~^, ~}~]"
          (agenda-item-identifier item)
          (agenda-item-status item)
          (agenda-item-text item)
          (agenda-item-memory-identifiers item)))

(-> agenda-tool--render-record ((option workspace-agenda)) string)
(defun agenda-tool--render-record (record)
  "Return the complete model-visible agenda RECORD."
  (if (and record (workspace-agenda-items record))
      (format nil "workspace: ~A~%~{~A~^~%~}"
              (workspace-agenda-directory record)
              (mapcar #'agenda-tool--render-item
                      (workspace-agenda-items record)))
      "The workspace agenda is empty."))

(-> agenda-tool--render-workspaces (agenda-state) string)
(defun agenda-tool--render-workspaces (state)
  "Return every known agenda directory and item count."
  (if (agenda-state-records state)
      (format nil "~{~A~^~%~}"
              (mapcar
               (lambda (record)
                 (format nil "~D item~:P  ~A"
                         (length (workspace-agenda-items record))
                         (workspace-agenda-directory record)))
               (agenda-state-records state)))
      "No workspace agendas are stored."))


;;;; -- Tool Executions --

(defmethod tool-execute ((tool agenda-list-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Read the complete agenda for the current workspace."
  (declare (ignore tool arguments))
  (let* ((configuration (tool-context-configuration context))
         (state (agenda-load configuration)))
    (tool-success
     (agenda-tool--render-record (agenda-current configuration state)))))

(defmethod tool-execute ((tool agenda-add-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Add one item to the current workspace agenda."
  (declare (ignore tool))
  (let* ((configuration (tool-context-configuration context))
         (state (agenda-load configuration))
         (text (agenda-tool--string-argument
                arguments "text" "agenda.add" :required t))
         (status (or (agenda-tool--status arguments) ':todo)))
    (multiple-value-bind (memory-identifiers memory-identifiers-supplied-p)
        (agenda-tool--memory-identifiers arguments "agenda.add")
      (declare (ignore memory-identifiers-supplied-p))
      (let ((item (agenda-add :configuration configuration
                              :state state
                              :text text
                              :status status
                              :memory-identifiers memory-identifiers)))
        (tool-success
         (format nil "Added ~A." (agenda-tool--render-item item)))))))

(defmethod tool-execute ((tool agenda-update-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Update text or status for one current workspace agenda item."
  (declare (ignore tool))
  (let* ((configuration (tool-context-configuration context))
         (state (agenda-load configuration))
         (identifier (agenda-tool--string-argument
                      arguments "id" "agenda.update" :required t))
         (text (agenda-tool--string-argument
                arguments "text" "agenda.update"))
         (status (agenda-tool--status arguments)))
    (multiple-value-bind (memory-identifiers memory-identifiers-supplied-p)
        (agenda-tool--memory-identifiers arguments "agenda.update")
      (unless (or text status memory-identifiers-supplied-p)
        (error 'tool-error
               :message "agenda.update requires text, status, or memory-ids."
               :tool-name "agenda.update"))
      (let ((item
              (apply #'agenda-update
                     configuration state identifier
                     (append (and text (list :text text))
                             (and status (list :status status))
                             (and memory-identifiers-supplied-p
                                  (list :memory-identifiers
                                        memory-identifiers))))))
        (tool-success
         (format nil "Updated ~A." (agenda-tool--render-item item)))))))

(defmethod tool-execute ((tool agenda-remove-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Remove one current workspace agenda item."
  (declare (ignore tool))
  (let* ((configuration (tool-context-configuration context))
         (state (agenda-load configuration))
         (identifier (agenda-tool--string-argument
                      arguments "id" "agenda.remove" :required t)))
    (if (agenda-remove configuration state identifier)
        (tool-success (format nil "Removed agenda item ~A." identifier))
        (tool-failure (format nil "Agenda item ~A does not exist here."
                              identifier)))))

(defmethod tool-execute ((tool agenda-transport-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Enumerate, inspect, copy, or move persistent workspace agendas."
  (declare (ignore tool))
  (let* ((configuration (tool-context-configuration context))
         (state (agenda-load configuration))
         (operation (agenda-tool--string-argument
                     arguments "operation" "agenda.transport" :required t))
         (source (agenda-tool--string-argument
                  arguments "source-directory" "agenda.transport"))
         (target (or (agenda-tool--string-argument
                      arguments "target-directory" "agenda.transport")
                     (namestring
                      (configuration-working-directory configuration)))))
    (cond
      ((string-equal operation "workspaces")
       (tool-success (agenda-tool--render-workspaces state)))
      ((string-equal operation "view")
       (unless source
         (error 'tool-error
                :message "agenda.transport view requires source-directory."
                :tool-name "agenda.transport"))
       (let* ((directory (agenda-directory-name configuration source))
              (record (agenda-find state directory)))
         (if record
             (tool-success (agenda-tool--render-record record))
             (tool-failure (format nil "No agenda is keyed by ~A." directory)))))
      ((or (string-equal operation "copy")
           (string-equal operation "move"))
       (unless source
         (error 'tool-error
                :message (format nil
                                 "agenda.transport ~A requires source-directory."
                                 operation)
                :tool-name "agenda.transport"))
       (let ((record
               (agenda-transport
                :configuration configuration
                :state state
                :source-directory source
                :target-directory target
                :move-p (string-equal operation "move"))))
         (tool-success
          (format nil "~A agenda into ~A with ~D item~:P."
                  (if (string-equal operation "move") "Moved" "Copied")
                  (workspace-agenda-directory record)
                  (length (workspace-agenda-items record))))))
      (t
       (error 'tool-error
              :message "agenda.transport operation must be workspaces, view, copy, or move."
              :tool-name "agenda.transport")))))
