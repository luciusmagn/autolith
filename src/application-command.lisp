(in-package #:autolith)

;;;; -- Interactive Command Protocol --

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +application-command-metadata-keys+
    '(:name :aliases :argument :description :tip
      :busy-behavior :terminal-behavior)
    "The literal metadata keys accepted by DEFINE-APPLICATION-COMMAND.")

  (defparameter +application-command-required-metadata-keys+
    '(:name :argument :description :tip :busy-behavior :terminal-behavior)
    "The command metadata keys every defining form must state explicitly.")

  (defparameter +application-command-busy-behaviors+
    '(:hold :inspect :cancel)
    "The supported command policies while application work is active.")

  (defparameter +application-command-terminal-behaviors+
    '(:shared :exclusive :exclusive-without-arguments)
    "The supported command policies for terminal reader ownership.")

  (defun application-command--proper-list-p (value)
    "Return true when VALUE is a finite proper list."
    (handler-case
        (and (listp value)
             (integerp (list-length value)))
      (type-error ()
        nil)))

  (defun application-command--identifier-p (value)
    "Return true when VALUE is one normalized slash-command identifier."
    (and (non-empty-string-p value)
         (> (length value) 1)
         (char= (char value 0) #\/)
         (string= value (string-downcase value))
         (not
          (find-if
           (lambda (character)
             (find character '(#\Space #\Tab #\Newline #\Return #\Page)))
           value))))

  (defun application-command--metadata-key-count (metadata key)
    "Return the number of KEY occurrences in literal METADATA."
    (loop for tail on metadata by #'cddr
          count (eq (first tail) key)))

  (defun application-command--validate-metadata
      (name aliases argument description tip busy-behavior terminal-behavior)
    "Validate command metadata values and return true."
    (unless (application-command--identifier-p name)
      (error "Application command name ~S is not a lowercase slash identifier."
             name))
    (unless (application-command--proper-list-p aliases)
      (error "Application command ~A aliases are not a proper literal list."
             name))
    (unless (every #'application-command--identifier-p aliases)
      (error "Application command ~A has an invalid alias." name))
    (when (member name aliases :test #'string=)
      (error "Application command ~A repeats its canonical name as an alias."
             name))
    (unless (= (length aliases)
               (length (remove-duplicates aliases :test #'string=)))
      (error "Application command ~A repeats an alias." name))
    (unless (or (null argument) (non-empty-string-p argument))
      (error "Application command ~A has an invalid argument hint." name))
    (unless (non-empty-string-p description)
      (error "Application command ~A requires a non-empty description." name))
    (unless (non-empty-string-p tip)
      (error "Application command ~A requires a non-empty tip." name))
    (unless (member busy-behavior
                    +application-command-busy-behaviors+
                    :test #'eq)
      (error "Application command ~A has invalid busy behavior ~S."
             name busy-behavior))
    (unless (member terminal-behavior
                    +application-command-terminal-behaviors+
                    :test #'eq)
      (error "Application command ~A has invalid terminal behavior ~S."
             name terminal-behavior))
    t)

  (defun application-command--validate-defining-form
      (definition-name metadata lambda-list)
    "Validate one literal DEFINE-APPLICATION-COMMAND header."
    (unless (and (symbolp definition-name)
                 definition-name
                 (not (keywordp definition-name))
                 (symbol-package definition-name))
      (error
       "An application command definition name must be an interned non-keyword symbol."))
    (unless (and (application-command--proper-list-p metadata)
                 (evenp (length metadata)))
      (error "Application command ~S metadata is not a literal property list."
             definition-name))
    (loop for key in metadata by #'cddr
          unless (member key +application-command-metadata-keys+ :test #'eq)
            do (error "Application command ~S has unknown metadata key ~S."
                      definition-name key))
    (dolist (key +application-command-metadata-keys+)
      (when (> (application-command--metadata-key-count metadata key) 1)
        (error "Application command ~S repeats metadata key ~S."
               definition-name key)))
    (dolist (key +application-command-required-metadata-keys+)
      (unless (= (application-command--metadata-key-count metadata key) 1)
        (error "Application command ~S requires literal metadata key ~S."
               definition-name key)))
    (unless (and (application-command--proper-list-p lambda-list)
                 (= (length lambda-list) 2)
                 (every (lambda (parameter)
                          (and (symbolp parameter)
                               parameter
                               (not (keywordp parameter))))
                        lambda-list)
                 (not (eq (first lambda-list) (second lambda-list))))
      (error
       "Application command ~S needs two distinct required handler parameters."
       definition-name))
    (application-command--validate-metadata
     (getf metadata :name)
     (getf metadata :aliases)
     (getf metadata :argument)
     (getf metadata :description)
     (getf metadata :tip)
     (getf metadata :busy-behavior)
     (getf metadata :terminal-behavior))))

(defclass application-command ()
  ((definition-name
    :initarg :definition-name
    :reader application-command-definition-name
    :type symbol
    :documentation "The stable defining-form name used for replacement and replay.")
   (name
    :initarg :name
    :reader application-command-name
    :type non-empty-string
    :documentation "The canonical lowercase slash-command name.")
   (aliases
    :initarg :aliases
    :initform nil
    :reader application-command-aliases
    :type list
    :documentation "Alternative slash names sharing this command's behavior.")
   (argument
    :initarg :argument
    :reader application-command-argument
    :type (option string)
    :documentation "The optional argument hint rendered by help and completion.")
   (description
    :initarg :description
    :reader application-command-description
    :type non-empty-string
    :documentation "The concise help and completion description.")
   (tip
    :initarg :tip
    :reader application-command-tip
    :type non-empty-string
    :documentation "The startup advice attached to this command.")
   (busy-behavior
    :initarg :busy-behavior
    :reader application-command-busy-behavior
    :type (member :hold :inspect :cancel)
    :documentation "The command policy while application work is active.")
   (terminal-behavior
    :initarg :terminal-behavior
    :reader application-command-terminal-behavior
    :type (member :shared :exclusive :exclusive-without-arguments)
    :documentation "When command execution requires exclusive terminal input.")
   (handler
    :initarg :handler
    :reader application-command-handler
    :type function
    :documentation "The immutable behavior captured when this definition registered."))
  (:documentation
   "Immutable metadata and behavior for one canonical interactive command."))

(defclass application-command-invocation ()
  ((input
    :initarg :input
    :reader application-command-invocation-input
    :type string
    :documentation "The complete submitted command input.")
   (name
    :initarg :name
    :reader application-command-invocation-name
    :type string
    :documentation "The submitted command token normalized to lowercase.")
   (remainder
    :initarg :remainder
    :reader application-command-invocation-remainder
    :type string
    :documentation "The trimmed text following the submitted command token.")
   (argument
    :initarg :argument
    :reader application-command-invocation-argument
    :type (option string)
    :documentation "The first whitespace-delimited remainder argument.")
   (command
    :initarg :command
    :reader application-command-invocation-command
    :type (option application-command)
    :documentation "The canonical registered command resolved for this input."))
  (:documentation
   "One parsed command submission and its registry resolution snapshot."))

(defclass application-command-registration ()
  ((command
    :initarg :command
    :reader application-command-registration-command
    :type application-command
    :documentation "The immutable command contributed by this registry layer.")
   (source
    :initarg :source
    :reader application-command-registration-source
    :type keyword
    :documentation "The runtime or user layer that contributed the command."))
  (:documentation "One ordered, possibly shadowed command registration layer."))

(defvar *application-command-registrations* nil
  "Ordered command registration layers, including shadowed definitions.")

(defvar *application-command-effective* nil
  "The effective canonical commands in deterministic presentation order.")

(defvar *application-command-index* (make-hash-table :test #'equal)
  "Canonical command names and aliases mapped to effective command objects.")

(defvar *application-command-lock*
  (make-lock "Autolith application commands")
  "The lock protecting all application command registry projections.")


;;;; -- Command Construction --

(-> application-command--validate (application-command) application-command)
(defun application-command--validate (command)
  "Validate COMMAND's complete immutable state and return it."
  (unless (and (symbolp (application-command-definition-name command))
               (application-command-definition-name command)
               (not
                (keywordp (application-command-definition-name command)))
               (symbol-package
                (application-command-definition-name command)))
    (error 'configuration-error
           :message
           "An application command definition name must be an interned non-keyword symbol."))
  (handler-case
      (application-command--validate-metadata
       (application-command-name command)
       (application-command-aliases command)
       (application-command-argument command)
       (application-command-description command)
       (application-command-tip command)
       (application-command-busy-behavior command)
       (application-command-terminal-behavior command))
    (error (condition)
      (error 'configuration-error
             :message (princ-to-string condition))))
  (unless (functionp (application-command-handler command))
    (error 'configuration-error
           :message (format nil "Application command ~A has no callable handler."
                            (application-command-name command))))
  command)

(-> application-command-create
    (&key (:definition-name symbol) (:name string) (:aliases list)
          (:argument (option string)) (:description string) (:tip string)
          (:busy-behavior keyword) (:terminal-behavior keyword)
          (:handler function))
    application-command)
(defun application-command-create
    (&key definition-name name aliases argument description tip busy-behavior
          terminal-behavior handler)
  "Create and validate one immutable interactive command."
  (unless (and (symbolp definition-name)
               definition-name
               (not (keywordp definition-name))
               (symbol-package definition-name))
    (error 'configuration-error
           :message
           "An application command definition name must be an interned non-keyword symbol."))
  (handler-case
      (application-command--validate-metadata
       name aliases argument description tip busy-behavior terminal-behavior)
    (error (condition)
      (error 'configuration-error :message (princ-to-string condition))))
  (unless (functionp handler)
    (error 'configuration-error
           :message (format nil "Application command ~A has no callable handler."
                            name)))
  (application-command--validate
   (make-instance
    'application-command
    :definition-name definition-name
    :name (copy-seq name)
    :aliases (mapcar #'copy-seq aliases)
    :argument (and argument (copy-seq argument))
    :description (copy-seq description)
    :tip (copy-seq tip)
    :busy-behavior busy-behavior
    :terminal-behavior terminal-behavior
    :handler handler)))


;;;; -- Layered Registry --

(-> application-command--current-registration-source () keyword)
(defun application-command--current-registration-source ()
  "Return the registration source appropriate to the current load context."
  (if *user-init-loading-p* ':user ':runtime))

(-> application-command--effective-projections
    (list)
    (values list hash-table))
(defun application-command--effective-projections (registrations)
  "Return validated effective command order and identifier index."
  (let ((canonical-order nil)
        (canonical-seen (make-hash-table :test #'equal))
        (canonical-winners (make-hash-table :test #'equal)))
    (dolist (registration registrations)
      (unless (typep registration 'application-command-registration)
        (error 'configuration-error
               :message "The application command registry contains an invalid layer."))
      (let* ((command
               (application-command--validate
                (application-command-registration-command registration)))
             (name (application-command-name command)))
        (unless (gethash name canonical-seen)
          (setf (gethash name canonical-seen) t)
          (push name canonical-order))
        (setf (gethash name canonical-winners) command)))
    (let* ((effective
             (loop for name in (nreverse canonical-order)
                   collect (gethash name canonical-winners)))
           (index (make-hash-table :test #'equal)))
      (dolist (command effective)
        (dolist (identifier
                 (cons (application-command-name command)
                       (application-command-aliases command)))
          (let ((existing (gethash identifier index)))
            (when (and existing (not (eq existing command)))
              (error 'configuration-error
                     :message
                     (format nil
                             "Application command identifier ~A belongs to both ~A and ~A."
                             identifier
                             (application-command-name existing)
                             (application-command-name command)))))
          (setf (gethash identifier index) command)))
      (values effective index))))

(-> application-command--publish-registrations (list) null)
(defun application-command--publish-registrations (registrations)
  "Validate and publish REGISTRATIONS while the registry lock is held."
  (multiple-value-bind (effective index)
      (application-command--effective-projections registrations)
    (setf *application-command-registrations* registrations
          *application-command-effective* effective
          *application-command-index* index))
  nil)

(-> register-application-command
    (application-command &key (:source keyword))
    application-command)
(defun register-application-command
    (command &key (source (application-command--current-registration-source)))
  "Register immutable COMMAND in SOURCE and return COMMAND.

The same source and definition name replace their prior layer in place. A
different definition may shadow the same canonical command without destroying
the earlier layer. Identifier collisions among effective commands are rejected
without changing the registry."
  (application-command--validate command)
  (unless (keywordp source)
    (error 'configuration-error
           :message "An application command registration source must be a keyword."))
  (with-lock-held (*application-command-lock*)
    (let* ((definition-name (application-command-definition-name command))
           (replacement
             (make-instance 'application-command-registration
                            :command command
                            :source source))
           (existing
             (position-if
              (lambda (registration)
                (and
                 (eq source
                     (application-command-registration-source registration))
                 (eq definition-name
                     (application-command-definition-name
                      (application-command-registration-command registration)))))
              *application-command-registrations*))
           (candidate
             (if existing
                 (append
                  (subseq *application-command-registrations* 0 existing)
                  (list replacement)
                  (nthcdr (1+ existing) *application-command-registrations*))
                 (append *application-command-registrations*
                         (list replacement)))))
      (application-command--publish-registrations candidate)))
  command)

(-> unregister-application-command
    (symbol &key (:source keyword))
    boolean)
(defun unregister-application-command
    (definition-name
     &key (source (application-command--current-registration-source)))
  "Remove DEFINITION-NAME's registration from SOURCE and report a change."
  (unless (and (symbolp definition-name)
               definition-name
               (not (keywordp definition-name))
               (symbol-package definition-name))
    (error 'configuration-error
           :message
           "An application command definition name must be an interned non-keyword symbol."))
  (unless (keywordp source)
    (error 'configuration-error
           :message "An application command registration source must be a keyword."))
  (with-lock-held (*application-command-lock*)
    (let ((candidate
            (remove-if
             (lambda (registration)
               (and
                (eq source
                    (application-command-registration-source registration))
                (eq definition-name
                    (application-command-definition-name
                     (application-command-registration-command registration)))))
             *application-command-registrations*)))
      (if (= (length candidate)
             (length *application-command-registrations*))
          nil
          (progn
            (application-command--publish-registrations candidate)
            t)))))

(-> application-command-list () list)
(defun application-command-list ()
  "Return an ordered snapshot of effective canonical commands."
  (with-lock-held (*application-command-lock*)
    (copy-list *application-command-effective*)))

(-> application-command-find (string) (option application-command))
(defun application-command-find (identifier)
  "Return the effective command named by case-insensitive IDENTIFIER."
  (with-lock-held (*application-command-lock*)
    (gethash (string-downcase identifier) *application-command-index*)))

(-> application-command--registrations () list)
(defun application-command--registrations ()
  "Return detached descriptions of every ordered command registration layer."
  (with-lock-held (*application-command-lock*)
    (loop for registration in *application-command-registrations*
          for position from 0
          for command = (application-command-registration-command registration)
          collect (list :position position
                        :source
                        (application-command-registration-source registration)
                        :definition-name
                        (application-command-definition-name command)
                        :command command))))

(-> application-command--registry-snapshot () list)
(defun application-command--registry-snapshot ()
  "Return an exact ordered snapshot of command registration layers."
  (with-lock-held (*application-command-lock*)
    (copy-list *application-command-registrations*)))

(-> application-command--registry-restore (list) null)
(defun application-command--registry-restore (snapshot)
  "Atomically replace command registrations with exact ordered SNAPSHOT."
  (unless (application-command--proper-list-p snapshot)
    (error 'configuration-error
           :message "An application command registry snapshot must be a proper list."))
  (with-lock-held (*application-command-lock*)
    (application-command--publish-registrations (copy-list snapshot)))
  nil)

(-> application-command--remove-registration-source (keyword) null)
(defun application-command--remove-registration-source (source)
  "Remove every command registration contributed by SOURCE."
  (with-lock-held (*application-command-lock*)
    (application-command--publish-registrations
     (remove source
             *application-command-registrations*
             :test #'eq
             :key #'application-command-registration-source)))
  nil)

(-> application-command--registration-snapshot
    (symbol keyword)
    (option list))
(defun application-command--registration-snapshot (definition-name source)
  "Return DEFINITION-NAME's SOURCE registration and exact position."
  (with-lock-held (*application-command-lock*)
    (loop for registration in *application-command-registrations*
          for position from 0
          when
            (and
             (eq source
                 (application-command-registration-source registration))
             (eq definition-name
                 (application-command-definition-name
                  (application-command-registration-command registration))))
            return (list :position position :registration registration))))

(-> application-command--registration-restore
    (symbol keyword (option list))
    null)
(defun application-command--registration-restore
    (definition-name source snapshot)
  "Restore one registration to exact SNAPSHOT position, or remove it."
  (with-lock-held (*application-command-lock*)
    (let* ((remaining
             (remove-if
              (lambda (registration)
                (and
                 (eq source
                     (application-command-registration-source registration))
                 (eq definition-name
                     (application-command-definition-name
                      (application-command-registration-command registration)))))
              *application-command-registrations*))
           (candidate
             (if snapshot
                 (let ((position (getf snapshot :position))
                       (registration (getf snapshot :registration)))
                   (unless (and (typep position '(integer 0))
                                (typep registration
                                       'application-command-registration))
                     (error 'configuration-error
                            :message
                            "An application command registration snapshot is invalid."))
                   (let ((bounded-position (min position (length remaining))))
                     (append (subseq remaining 0 bounded-position)
                             (list registration)
                             (nthcdr bounded-position remaining))))
                 remaining)))
      (application-command--publish-registrations candidate)))
  nil)


;;;; -- Invocation and Dispatch --

(defparameter +application-command-whitespace+
  '(#\Space #\Tab #\Newline #\Return #\Page)
  "Characters separating command tokens and arguments.")

(-> application-command--first-token (string) (option string))
(defun application-command--first-token (text)
  "Return TEXT's first whitespace-delimited token, or NIL when empty."
  (when (non-empty-string-p text)
    (let ((end (position-if
                (lambda (character)
                  (find character +application-command-whitespace+))
                text)))
      (if end
          (subseq text 0 end)
          (copy-seq text)))))

(-> application-command-invocation-parse
    (string)
    application-command-invocation)
(defun application-command-invocation-parse (input)
  "Parse INPUT and resolve its command through one registry snapshot."
  (let* ((trimmed
           (string-left-trim +application-command-whitespace+ input))
         (separator
           (position-if
            (lambda (character)
              (find character +application-command-whitespace+))
            trimmed))
         (submitted-name
           (string-downcase
            (if separator (subseq trimmed 0 separator) trimmed)))
         (remainder
           (if separator
               (string-trim +application-command-whitespace+
                            (subseq trimmed separator))
               ""))
         (argument (application-command--first-token remainder)))
    (make-instance
     'application-command-invocation
     :input (copy-seq input)
     :name submitted-name
     :remainder remainder
     :argument argument
     :command (application-command-find submitted-name))))

(defgeneric application-command-execute (command application invocation)
  (:documentation
   "Execute COMMAND for APPLICATION and INVOCATION, returning a loop action."))

(defmethod application-command-execute
    ((command application-command)
     application
     (invocation application-command-invocation))
  "Invoke COMMAND's captured handler and validate its loop action."
  (let ((result
          (funcall (application-command-handler command)
                   application invocation)))
    (unless (member result '(:continue :quit) :test #'eq)
      (error 'configuration-error
             :message
             (format nil "Application command ~A returned invalid action ~S."
                     (application-command-name command)
                     result)))
    result))

(defgeneric application-command-busy-action (command invocation)
  (:documentation
   "Return :HOLD, :EXECUTE, or :CANCEL for COMMAND during active work."))

(defmethod application-command-busy-action
    ((command application-command)
     (invocation application-command-invocation))
  "Resolve COMMAND's declared busy policy for INVOCATION."
  (ecase (application-command-busy-behavior command)
    (:hold
     ':hold)
    (:inspect
     (if (zerop
          (length (application-command-invocation-remainder invocation)))
         ':execute
         ':hold))
    (:cancel
     ':cancel)))

(defgeneric application-command-terminal-owner-p (command invocation)
  (:documentation
   "Return whether COMMAND requires exclusive terminal input for INVOCATION."))

(defmethod application-command-terminal-owner-p
    ((command application-command)
     (invocation application-command-invocation))
  "Resolve COMMAND's terminal policy for INVOCATION."
  (case (application-command-terminal-behavior command)
    (:shared
     nil)
    (:exclusive
     t)
    (:exclusive-without-arguments
     (zerop
      (length (application-command-invocation-remainder invocation))))
    (otherwise
     nil)))

(defgeneric application-command-completion-entry (command)
  (:documentation "Return COMMAND's canonical terminal completion entry."))

(defmethod application-command-completion-entry
    ((command application-command))
  "Return a detached completion plist for COMMAND."
  (list :name (copy-seq (application-command-name command))
        :argument
        (let ((argument (application-command-argument command)))
          (and argument (copy-seq argument)))
        :description (copy-seq (application-command-description command))))

(-> application-command-completion-entries () list)
(defun application-command-completion-entries ()
  "Return fresh canonical completion entries in registry order."
  (mapcar #'application-command-completion-entry
          (application-command-list)))


;;;; -- Defining Form --

(defmacro define-application-command
    (definition-name metadata lambda-list &body body)
  "Define and register one complete, live-redefinable application command.

METADATA must contain literal :NAME, :ARGUMENT, :DESCRIPTION, :TIP,
:BUSY-BEHAVIOR, and :TERMINAL-BEHAVIOR values. :ALIASES defaults to NIL. The
handler receives APPLICATION and an APPLICATION-COMMAND-INVOCATION and must
return :CONTINUE or :QUIT."
  (application-command--validate-defining-form
   definition-name metadata lambda-list)
  `(progn
     (defun ,definition-name ,lambda-list
       ,@body)
     (eval-when (:load-toplevel :execute)
       (register-application-command
        (application-command-create
         :definition-name ',definition-name
         :name ,(getf metadata :name)
         :aliases ',(getf metadata :aliases)
         :argument ,(getf metadata :argument)
         :description ,(getf metadata :description)
         :tip ,(getf metadata :tip)
         :busy-behavior ',(getf metadata :busy-behavior)
         :terminal-behavior ',(getf metadata :terminal-behavior)
         :handler #',definition-name)))
     ',definition-name))
