(in-package #:frob)

;;;; -- Active Image Inspection --

(defvar *exploratory-definitions* (make-hash-table :test #'equal)
  "Complete source forms installed exploratorily in the active image.")

(-> self-read-form (string &key (:read-eval boolean)) t)
(defun self-read-form (source &key (read-eval t))
  "Read exactly one Common Lisp form from SOURCE."
  (let ((*read-eval* read-eval)
        (*package* (find-package '#:frob))
        (end-marker (cons nil nil)))
    (multiple-value-bind (form position)
        (read-from-string source t nil)
      (multiple-value-bind (extra ignored-position)
          (read-from-string source nil end-marker :start position)
        (declare (ignore ignored-position))
        (unless (eq extra end-marker)
          (error "Expected exactly one Common Lisp form.")))
      form)))

(-> self-resolve-symbol (string) symbol)
(defun self-resolve-symbol (name)
  "Resolve readable symbol NAME relative to the FROB package."
  (let ((value (self-read-form name :read-eval nil)))
    (unless (symbolp value)
      (error "~S does not name a symbol." name))
    value))

(-> self-symbol-lambda-list (symbol) t)
(defun self-symbol-lambda-list (symbol)
  "Return SYMBOL's function lambda list when introspection can recover it."
  (when (fboundp symbol)
    (handler-case
        (let ((function (symbol-function symbol)))
          (if (typep function 'generic-function)
              (closer-mop:generic-function-lambda-list function)
              (multiple-value-bind (expression closure-p lexical-name)
                  (function-lambda-expression function)
                (declare (ignore closure-p lexical-name))
                (and (consp expression)
                     (eq (first expression) 'lambda)
                     (second expression)))))
      (error ()
        nil))))

(-> self-inspect-symbol (symbol) string)
(defun self-inspect-symbol (symbol)
  "Return structured documentation and description for active SYMBOL."
  (with-output-to-string (stream)
    (format stream "Symbol: ~S~%Package: ~A~%"
            symbol
            (or (and (symbol-package symbol)
                     (package-name (symbol-package symbol)))
                "uninterned"))
    (when (fboundp symbol)
      (format stream "Function binding: yes~%Lambda list: ~S~%Documentation: ~A~%"
              (self-symbol-lambda-list symbol)
              (or (documentation symbol 'function) "none")))
    (when (boundp symbol)
      (format stream "Value binding: yes~%Value: ~A~%Documentation: ~A~%"
              (bounded-string (symbol-value symbol) :limit 2000)
              (or (documentation symbol 'variable) "none")))
    (let ((class (find-class symbol nil)))
      (when class
        (closer-mop:finalize-inheritance class)
        (format stream "Class binding: yes~%Class documentation: ~A~%Slots:~%"
                (or (documentation symbol 'type) "none"))
        (dolist (slot (closer-mop:class-slots class))
          (format stream "  ~S~@[ - ~A~]~%"
                  (closer-mop:slot-definition-name slot)
                  (documentation slot t)))))
    (format stream "~%Describe:~%")
    (describe symbol stream)))

(defmethod tool-execute ((tool self-inspect-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Inspect one required symbol in CONTEXT's active image."
  (declare (ignore tool context))
  (tool-success
   (self-inspect-symbol
    (self-resolve-symbol
     (tool-argument arguments "symbol" :required t)))))


;;;; -- Mutation Journal --

(-> mutation-journal-append (configuration list) list)
(defun mutation-journal-append (configuration record)
  "Append portable mutation RECORD to CONFIGURATION's journal."
  (let ((pathname (configuration-journal-path configuration))
        (entry (list* (first record)
                      :time (get-universal-time)
                      (rest record))))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (let ((*print-circle* t)
            (*print-readably* t))
        (prin1 entry stream)
        (terpri stream)
        (finish-output stream)))
    entry))

(-> self-capture-evaluation (function) (values list string))
(defun self-capture-evaluation (function)
  "Call FUNCTION in the active image while capturing output and rendered values."
  (let ((result-values nil))
    (let ((output
            (with-output-to-string (stream)
              (let ((*standard-output* stream)
                    (*error-output* stream)
                    (*trace-output* stream)
                    (*package* (find-package '#:frob)))
                (setf result-values
                      (multiple-value-list (funcall function)))))))
      (values (mapcar #'worker-render-value result-values) output))))

(-> self-evaluation-result (list string) string)
(defun self-evaluation-result (result-values output)
  "Render active-image RESULT-VALUES and captured OUTPUT for a tool result."
  (with-output-to-string (stream)
    (when (non-empty-string-p output)
      (format stream "Output:~%~A~%" output))
    (format stream "Values:~%~{~A~%~}" result-values)))

(defmethod tool-execute ((tool self-eval-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Evaluate one exploratory form in CONTEXT's active image and journal it."
  (declare (ignore tool))
  (let* ((source (tool-argument arguments "form" :required t))
         (configuration (tool-context-configuration context)))
    (mutation-journal-append
     configuration
     (list :mutation :kind :eval :proposed source :result :pending))
    (handler-case
        (multiple-value-bind (result-values output)
            (self-capture-evaluation
             (lambda ()
               (eval (self-read-form source))))
          (mutation-journal-append
           configuration
           (list :mutation :kind :eval :proposed source :result :installed))
          (tool-success (self-evaluation-result result-values output)))
      (error (condition)
        (mutation-journal-append
         configuration
         (list :mutation
               :kind :eval
               :proposed source
               :result :failed
               :condition (princ-to-string condition)))
        (error condition)))))


;;;; -- Definition Installation --

(defparameter +definition-operators+
  '(defun defgeneric defmethod defmacro defclass defstruct define-condition
    deftype define-compiler-macro)
  "Top-level defining operators accepted by self.redefine and source persistence.")

(-> definition-form-p (t) boolean)
(defun definition-form-p (form)
  "Return true when FORM is one supported complete top-level definition."
  (and (consp form)
       (symbolp (first form))
       (member (first form) +definition-operators+ :test #'eq)
       (symbolp (second form))))

(-> definition-signature (list) list)
(defun definition-signature (definition)
  "Return the semantic source identity of one top-level DEFINITION."
  (if (eq (first definition) 'defmethod)
      (let* ((tail (cddr definition))
             (lambda-position (position-if #'listp tail)))
        (unless lambda-position
          (error "DEFMETHOD has no lambda list."))
        (list* (first definition)
               (second definition)
               (subseq tail 0 (1+ lambda-position))))
      (list (first definition) (second definition))))

(-> definition-key (list) string)
(defun definition-key (definition)
  "Return a stable readable key for DEFINITION's semantic signature."
  (write-to-string (definition-signature definition)
                   :readably t
                   :case :downcase))

(-> self-previous-definition (list) t)
(defun self-previous-definition (definition)
  "Return the best recoverable active representation preceding DEFINITION."
  (let ((name (second definition)))
    (or (gethash (definition-key definition) *exploratory-definitions*)
        (when (and (member (first definition)
                           '(defun defgeneric defmethod defmacro define-compiler-macro)
                           :test #'eq)
                   (fboundp name))
          (multiple-value-bind (lambda-expression closure-p lexical-name)
              (function-lambda-expression (symbol-function name))
            (declare (ignore closure-p lexical-name))
            (and lambda-expression
                 (write-to-string lambda-expression
                                  :circle t
                                  :level 10
                                  :length 100)))))))

(-> self-install-definition (configuration string) t)
(defun self-install-definition (configuration source)
  "Compile and install one complete SOURCE definition, recording its active history."
  (let ((definition (self-read-form source)))
    (unless (definition-form-p definition)
      (error 'source-mutation-error
             :message "self.redefine accepts one complete supported definition."
             :tool-name "self.redefine"
             :pathname nil))
    (let ((key (definition-key definition))
          (previous (self-previous-definition definition)))
      (mutation-journal-append
       configuration
       (list :mutation
             :kind :definition
             :target key
             :previous previous
             :proposed source
             :result :pending))
      (handler-case
          (let ((result (eval definition)))
            (setf (gethash key *exploratory-definitions*) source)
            (mutation-journal-append
             configuration
             (list :mutation
                   :kind :definition
                   :target key
                   :previous previous
                   :proposed source
                   :result :installed))
            result)
        (error (condition)
          (mutation-journal-append
           configuration
           (list :mutation
                 :kind :definition
                 :target key
                 :previous previous
                 :proposed source
                 :result :failed
                 :condition (princ-to-string condition)))
          (error condition))))))

(defmethod tool-execute ((tool self-redefine-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Install one exploratory top-level definition in CONTEXT's active image."
  (declare (ignore tool))
  (let ((source (tool-argument arguments "definition" :required t)))
    (self-install-definition (tool-context-configuration context) source)
    (tool-success "The definition was compiled and installed in the active image.")))

(defmethod tool-execute ((tool self-set-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Set one active global binding after journaling its previous value."
  (declare (ignore tool))
  (let* ((symbol (self-resolve-symbol
                  (tool-argument arguments "symbol" :required t)))
         (value-source (tool-argument arguments "value" :required t))
         (configuration (tool-context-configuration context))
         (previous (and (boundp symbol)
                        (worker-render-value (symbol-value symbol)))))
    (mutation-journal-append
     configuration
     (list :mutation
           :kind :set
           :target (write-to-string symbol)
           :previous previous
           :proposed value-source
           :result :pending))
    (let ((value (eval (self-read-form value-source))))
      (setf (symbol-value symbol) value)
      (mutation-journal-append
       configuration
       (list :mutation
             :kind :set
             :target (write-to-string symbol)
             :previous previous
             :proposed value-source
             :result :installed))
      (tool-success (format nil "~S is now ~A." symbol (worker-render-value value))))))


;;;; -- Form-Aware Source Persistence --

(defclass source-form ()
  ((form
    :initarg :form
    :reader source-form-form
    :type t
    :documentation "The parsed top-level form.")
   (start
    :initarg :start
    :reader source-form-start
    :type integer
    :documentation "The character offset at which the form begins.")
   (end
    :initarg :end
    :reader source-form-end
    :type integer
    :documentation "The character offset immediately after the form."))
  (:documentation "One parsed top-level form and its exact source span."))

(-> source--skip-block-comment (string integer) integer)
(defun source--skip-block-comment (source start)
  "Return the first offset after the nested block comment at START."
  (let ((position (+ start 2))
        (depth 1)
        (length (length source)))
    (loop while (and (< position length) (plusp depth))
          do (cond
               ((and (< (1+ position) length)
                     (char= (char source position) #\#)
                     (char= (char source (1+ position)) #\|))
                (incf depth)
                (incf position 2))
               ((and (< (1+ position) length)
                     (char= (char source position) #\|)
                     (char= (char source (1+ position)) #\#))
                (decf depth)
                (incf position 2))
               (t
                (incf position))))
    (when (plusp depth)
      (error "Unterminated block comment in source file."))
    position))

(-> source--next-form-start (string integer) integer)
(defun source--next-form-start (source start)
  "Skip whitespace and comments in SOURCE beginning at START."
  (let ((position start)
        (length (length source)))
    (loop
      (loop while (and (< position length)
                       (find (char source position)
                             '(#\Space #\Tab #\Newline #\Return #\Page)))
            do (incf position))
      (cond
        ((and (< position length) (char= (char source position) #\;))
         (let ((newline (position #\Newline source :start position)))
           (setf position (if newline (1+ newline) length))))
        ((and (< (1+ position) length)
              (char= (char source position) #\#)
              (char= (char source (1+ position)) #\|))
         (setf position (source--skip-block-comment source position)))
        (t
         (return position))))))

(-> source-read-forms (string) list)
(defun source-read-forms (source)
  "Read complete top-level forms and exact spans from SOURCE."
  (let ((stream (make-string-input-stream source))
        (position 0)
        (forms nil)
        (*read-eval* nil)
        (*package* (find-package '#:frob)))
    (loop
      (setf position (source--next-form-start source position))
      (when (>= position (length source))
        (return (nreverse forms)))
      (file-position stream position)
      (let ((form (read stream t nil))
            (start position)
            (end (file-position stream)))
        (push (make-instance 'source-form :form form :start start :end end)
              forms)
        (setf position end)))))

(-> source-definition-match-p (source-form list) boolean)
(defun source-definition-match-p (source-form definition)
  "Return true when SOURCE-FORM defines the same operator and name as DEFINITION."
  (let ((form (source-form-form source-form)))
    (and (definition-form-p form)
         (equal (definition-signature form)
                (definition-signature definition)))))

(-> self-source-pathname (configuration string) pathname)
(defun self-source-pathname (configuration relative-name)
  "Resolve RELATIVE-NAME to an existing editable file beneath Frob's src directory."
  (let* ((source-root (configuration-source-root configuration))
         (editable-root (merge-pathnames "src/" source-root))
         (pathname (merge-pathnames relative-name source-root)))
    (unless (and (uiop:subpathp pathname editable-root)
                 (probe-file pathname))
      (error 'source-mutation-error
             :message "Durable self modification is limited to existing files under src/."
             :tool-name "self.persist-definition"
             :pathname pathname))
    pathname))

(-> source--atomic-write (pathname string) pathname)
(defun source--atomic-write (pathname content)
  "Atomically replace PATHNAME with CONTENT through a sibling temporary file."
  (let ((temporary
          (merge-pathnames
           (format nil ".~A.~D.tmp"
                   (pathname-name pathname)
                   (sb-posix:getpid))
           (uiop:pathname-directory-pathname pathname))))
    (with-open-file (stream temporary
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (write-string content stream)
      (finish-output stream))
    (uiop:rename-file-overwriting-target temporary pathname)
    pathname))

(-> source-replace-definition (pathname string) string)
(defun source-replace-definition (pathname definition-source)
  "Replace the matching complete definition in PATHNAME with DEFINITION-SOURCE."
  (let* ((definition (self-read-form definition-source :read-eval nil))
         (source (uiop:read-file-string pathname))
         (match (find-if
                 (lambda (source-form)
                   (source-definition-match-p source-form definition))
                 (source-read-forms source))))
    (unless (definition-form-p definition)
      (error 'source-mutation-error
             :message "The durable source is not a supported complete definition."
             :tool-name "self.persist-definition"
             :pathname pathname))
    (unless match
      (error 'source-mutation-error
             :message (format nil "No matching definition exists in ~A." pathname)
             :tool-name "self.persist-definition"
             :pathname pathname))
    (let ((updated
            (concatenate 'string
                         (subseq source 0 (source-form-start match))
                         definition-source
                         (subseq source (source-form-end match)))))
      (source--atomic-write pathname updated)
      updated)))

(defmethod tool-execute ((tool self-persist-definition-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Install and form-aware persist one complete definition in CONTEXT."
  (declare (ignore tool))
  (let* ((definition-source
           (tool-argument arguments "definition" :required t))
         (relative-name
           (tool-argument arguments "pathname" :required t))
         (configuration (tool-context-configuration context))
         (pathname (self-source-pathname configuration relative-name)))
    (self-install-definition configuration definition-source)
    (source-replace-definition pathname definition-source)
    (mutation-journal-append
     configuration
     (list :mutation
           :kind :source
           :target (enough-namestring pathname
                                      (configuration-source-root configuration))
           :proposed definition-source
           :result :source-written))
    (tool-success
     (format nil "Installed and wrote ~A. The change is not durable until committed."
             (enough-namestring pathname
                                (configuration-source-root configuration))))))


;;;; -- Git Operations --

(-> self-git-command (configuration list &key (:ignore-error-status boolean)) string)
(defun self-git-command (configuration arguments &key ignore-error-status)
  "Run Git ARGUMENTS in CONFIGURATION's source root and return combined output."
  (uiop:run-program
   (append (list "git" "-C"
                 (namestring (configuration-source-root configuration)))
           arguments)
   :output :string
   :error-output :output
   :ignore-error-status ignore-error-status))

(defmethod tool-execute ((tool self-diff-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Return the active Frob source diff."
  (declare (ignore tool arguments))
  (let ((output (self-git-command
                 (tool-context-configuration context)
                 '("diff" "--"))))
    (tool-success (if (non-empty-string-p output)
                      output
                      "The tracked worktree has no unstaged diff."))))

(-> self-validate-commit-title (string) string)
(defun self-validate-commit-title (title)
  "Return valid commit TITLE or signal a tool error."
  (unless (and (non-empty-string-p title)
               (< (length title) 72)
               (null (find #\Newline title))
               (null (find #\Return title)))
    (error 'tool-error
           :message "A commit title must be one non-empty line under 72 characters."
           :tool-name "self.commit"))
  title)

(-> self-validate-commit-paths (configuration vector) list)
(defun self-validate-commit-paths (configuration paths)
  "Return validated repository-relative PATHS beneath CONFIGURATION's source root."
  (unless (plusp (length paths))
    (error 'tool-error
           :message "self.commit requires at least one explicit path."
           :tool-name "self.commit"))
  (loop for path across paths
        do (unless (and (non-empty-string-p path)
                        (uiop:subpathp
                         (merge-pathnames path
                                          (configuration-source-root configuration))
                         (configuration-source-root configuration)))
             (error 'tool-error
                    :message (format nil "Commit path ~S is outside the repository." path)
                    :tool-name "self.commit"))
        collect path))

(defmethod tool-execute ((tool self-commit-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Check, stage, and commit only the explicit paths supplied in ARGUMENTS."
  (declare (ignore tool))
  (let* ((configuration (tool-context-configuration context))
         (title (self-validate-commit-title
                 (tool-argument arguments "title" :required t)))
         (raw-paths (tool-argument arguments "paths" :required t)))
    (unless (vectorp raw-paths)
      (error 'tool-error
             :message "self.commit paths must be a JSON array."
             :tool-name "self.commit"))
    (let ((paths (self-validate-commit-paths configuration raw-paths)))
      (self-git-command configuration (append '("diff" "--check" "--") paths))
      (self-git-command configuration (append '("add" "--") paths))
      (let ((output (self-git-command configuration
                                      (append (list "commit" "-m" title "--only" "--")
                                              paths)
                                      :ignore-error-status t)))
        (mutation-journal-append
         configuration
         (list :mutation
               :kind :commit
               :paths paths
               :title title
               :result (if (search "nothing to commit" output :test #'char-equal)
                           :unchanged
                           :committed)))
        (tool-success output)))))
