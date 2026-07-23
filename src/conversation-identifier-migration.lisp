(in-package #:autolith)

;;;; -- Legacy Conversation Identifier Migration --

(defparameter *conversation-identifier-migration-version* 1
  "The readable migration record version for conversation identifiers.")

(defvar *conversation-identifier-migration-lock*
  (make-lock "conversation identifier migration")
  "Serialize legacy identifier migration within one Autolith process.")

(-> conversation-identifier-migration--legacy-identifier-p (t) boolean)
(defun conversation-identifier-migration--legacy-identifier-p (value)
  "Return true when VALUE has the historical UUID conversation ID shape."
  (and (stringp value)
       (= (length value) 36)
       (loop for index below (length value)
             for character = (char value index)
             always (if (member index '(8 13 18 23))
                        (char= character #\-)
                        (not (null (digit-char-p character 16)))))))

(-> conversation-identifier-migration--proper-list-p (t) boolean)
(defun conversation-identifier-migration--proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (handler-case
      (and (listp value) (or (list-length value) (null value)) t)
    (type-error ()
      nil)))

(-> conversation-identifier-migration--signal
    (configuration keyword string &key (:pathname (option pathname)) (:cause t))
    null)
(defun conversation-identifier-migration--signal
    (configuration stage message &key pathname cause)
  "Signal a structured migration failure at STAGE with MESSAGE and CAUSE."
  (error 'conversation-identifier-migration-error
         :message message
         :pathname (or pathname
                       (configuration-conversation-identifier-migration-path
                        configuration))
         :sequence nil
         :stage stage
         :cause cause))

(-> conversation-identifier-migration--entry-p (t) boolean)
(defun conversation-identifier-migration--entry-p (value)
  "Return true when VALUE is one complete legacy-to-current mapping entry."
  (and (conversation-identifier-migration--proper-list-p value)
       (= (length value) 6)
       (conversation-identifier-migration--legacy-identifier-p
        (getf value :old))
       (conversation-identifier-stored-p (getf value :new))
       (typep (getf value :created-at) 'timestamp)
       (loop for key in value by #'cddr
             always (member key '(:old :new :created-at) :test #'eq))))

(-> conversation-identifier-migration--record-p (t) boolean)
(defun conversation-identifier-migration--record-p (value)
  "Return true when VALUE is a complete supported migration record."
  (and (conversation-identifier-migration--proper-list-p value)
       (= (length value) 9)
       (eq (first value) :conversation-identifier-migration)
       (= (or (getf (rest value) :version) 0)
          *conversation-identifier-migration-version*)
       (member (getf (rest value) :status)
               '(:prepared :conversations :references :artifacts :complete)
               :test #'eq)
       (typep (getf (rest value) :updated-at) 'timestamp)
       (loop for key in (rest value) by #'cddr
             always (member key '(:version :status :updated-at :entries)
                            :test #'eq))
       (let ((entries (getf (rest value) :entries)))
         (and (conversation-identifier-migration--proper-list-p entries)
              (every #'conversation-identifier-migration--entry-p entries)
              (= (length entries)
                 (length (remove-duplicates
                          entries :test #'string= :key
                          (lambda (entry) (getf entry :old)))))
              (= (length entries)
                 (length (remove-duplicates
                          entries :test #'string= :key
                          (lambda (entry) (getf entry :new)))))))))

(-> conversation-identifier-migration--record
    (keyword list)
    list)
(defun conversation-identifier-migration--record (status entries)
  "Return a complete migration record with STATUS and ENTRIES."
  (list :conversation-identifier-migration
        :version *conversation-identifier-migration-version*
        :status status
        :updated-at (get-universal-time)
        :entries entries))

(-> conversation-identifier-migration--read (configuration) (option list))
(defun conversation-identifier-migration--read (configuration)
  "Read and validate CONFIGURATION's migration record when it exists."
  (let ((pathname
          (configuration-conversation-identifier-migration-path configuration)))
    (when (probe-file pathname)
      (handler-case
          (let ((record (snapshot-read pathname)))
            (unless (conversation-identifier-migration--record-p record)
              (conversation-identifier-migration--signal
               configuration
               ':planning
               "The conversation identifier migration record is malformed."
               :pathname pathname))
            record)
        (conversation-identifier-migration-error (condition)
          (error condition))
        (error (cause)
          (conversation-identifier-migration--signal
           configuration
           ':planning
           (format nil "Could not read the conversation identifier migration record: ~A"
                   cause)
           :pathname pathname
           :cause cause))))))

(-> conversation-identifier-migration--write
    (configuration keyword list)
    list)
(defun conversation-identifier-migration--write (configuration status entries)
  "Atomically publish STATUS and ENTRIES for CONFIGURATION's migration."
  (let* ((pathname
           (configuration-conversation-identifier-migration-path configuration))
         (record (conversation-identifier-migration--record status entries)))
    (handler-case
        (progn
          (snapshot-write pathname record :mode #o600)
          record)
      (error (cause)
        (conversation-identifier-migration--signal
         configuration
         ':planning
         (format nil "Could not publish the conversation identifier migration record: ~A"
                 cause)
         :pathname pathname
         :cause cause)))))

(-> conversation-identifier-migration--header (pathname) (option list))
(defun conversation-identifier-migration--header (pathname)
  "Return PATHNAME's leading conversation form, or NIL when it is not one."
  (handler-case
      (with-open-file (stream pathname :direction :input :external-format :utf-8)
        (let* ((*read-eval* nil)
               (end-marker (cons nil nil))
               (form (read stream nil end-marker)))
          (and (listp form)
               (eq (first form) :conversation)
               form)))
    (error ()
      nil)))

(-> conversation-identifier-migration--legacy-files (configuration) list)
(defun conversation-identifier-migration--legacy-files (configuration)
  "Return valid resumable conversation files whose identifiers are legacy."
  (let ((root (configuration-conversation-root configuration)))
    (if (probe-file root)
        (sort
         (loop for pathname in (uiop:directory-files root "*.sexp")
               for header = (conversation-identifier-migration--header pathname)
               for identifier = (and header (getf (rest header) :id))
               when (conversation-identifier-migration--legacy-identifier-p
                     identifier)
                 collect (list :pathname pathname :header header))
         #'string<
         :key (lambda (entry) (namestring (getf entry :pathname))))
        nil)))

(-> conversation-identifier-migration--entry-for-old (string list) (option list))
(defun conversation-identifier-migration--entry-for-old (identifier entries)
  "Return IDENTIFIER's mapping from ENTRIES, when present."
  (find identifier entries :test #'string= :key
        (lambda (entry) (getf entry :old))))

(-> conversation-identifier-migration--plan
    (configuration (option list) list)
    (values list boolean))
(defun conversation-identifier-migration--plan
    (configuration existing-record legacy-files)
  "Return a durable mapping plan and whether migration work is required."
  (let* ((entries (copy-tree (and existing-record
                                  (getf (rest existing-record) :entries))))
         (reserved (mapcar (lambda (entry) (getf entry :new)) entries))
         (root (configuration-conversation-root configuration)))
    (dolist (legacy legacy-files)
      (let* ((pathname (getf legacy :pathname))
             (header (getf legacy :header))
             (old (getf (rest header) :id))
             (created-at (getf (rest header) :created-at)))
        (unless (string= (or (pathname-name pathname) "") old)
          (conversation-identifier-migration--signal
           configuration
           ':planning
           (format nil "Legacy conversation ~A disagrees with its header identifier ~S."
                   pathname old)
           :pathname pathname))
        (unless (typep created-at 'timestamp)
          (conversation-identifier-migration--signal
           configuration
           ':planning
           (format nil "Legacy conversation ~A has no valid creation time."
                   pathname)
           :pathname pathname))
        (unless (conversation-identifier-migration--entry-for-old old entries)
          (let ((new
                  (conversation-identifier-generate
                   root
                   :timestamp created-at
                   :reserved-identifiers reserved)))
            (push new reserved)
            (setf entries
                  (append entries
                          (list (list :old old
                                      :new new
                                      :created-at created-at))))))))
    (values entries
            (not
             (null
              (or legacy-files
                  (and existing-record
                       (not (eq (getf (rest existing-record) :status)
                                :complete)))))))))

(-> conversation-identifier-migration--replace-all
    (string string string)
    string)
(defun conversation-identifier-migration--replace-all (text old new)
  "Return TEXT with every exact OLD occurrence replaced by NEW."
  (let ((position (search old text :test #'char=)))
    (if (null position)
        text
        (with-output-to-string (stream)
          (loop with start = 0
                for match = (search old text :start2 start :test #'char=)
                while match
                do (write-string text stream :start start :end match)
                   (write-string new stream)
                   (setf start (+ match (length old)))
                finally (write-string text stream :start start))))))

(-> conversation-identifier-migration--rewrite-string (string list) string)
(defun conversation-identifier-migration--rewrite-string (value entries)
  "Replace every legacy identifier in VALUE according to ENTRIES."
  (reduce
   (lambda (text entry)
     (conversation-identifier-migration--replace-all
      text (getf entry :old) (getf entry :new)))
   entries
   :initial-value value))

(-> conversation-identifier-migration--rewrite-value (t list) t)
(defun conversation-identifier-migration--rewrite-value (value entries)
  "Return portable VALUE with exact legacy identifier strings rewritten."
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((rewrite (current)
               "Recursively rewrite CURRENT while preserving shared structure."
               (cond
                 ((stringp current)
                  (conversation-identifier-migration--rewrite-string
                   current entries))
                 ((pathnamep current)
                  (pathname
                   (conversation-identifier-migration--rewrite-string
                    (namestring current) entries)))
                 ((consp current)
                  (multiple-value-bind (copy present-p) (gethash current seen)
                    (if present-p
                        copy
                        (let ((result (cons nil nil)))
                          (setf (gethash current seen) result
                                (first result) (rewrite (first current))
                                (rest result) (rewrite (rest current)))
                          result))))
                 ((vectorp current)
                  (multiple-value-bind (copy present-p) (gethash current seen)
                    (if present-p
                        copy
                        (let ((result
                                (make-array
                                 (length current)
                                 :element-type (array-element-type current))))
                          (setf (gethash current seen) result)
                          (loop for index below (length current)
                                do (setf (aref result index)
                                         (rewrite (aref current index))))
                          result))))
                 (t
                  current))))
      (rewrite value))))

(-> conversation-identifier-migration--write-date (pathname integer) null)
(defun conversation-identifier-migration--write-date (pathname universal-time)
  "Set PATHNAME's access and modification times from UNIVERSAL-TIME."
  (let ((unix-time (max 0 (universal-time->unix-time universal-time))))
    (sb-posix:utime (namestring pathname) unix-time unix-time))
  nil)

(-> conversation-identifier-migration--write-forms
    (pathname list &key (:write-date (option integer)))
    pathname)
(defun conversation-identifier-migration--write-forms
    (pathname forms &key write-date)
  "Atomically replace PATHNAME with readable top-level FORMS."
  (let ((temporary
          (merge-pathnames
           (make-pathname
            :name (format nil ".~A.~D" (or (pathname-name pathname) "state")
                          (sb-posix:getpid))
            :type "tmp")
           (uiop:pathname-directory-pathname pathname))))
    (ensure-directories-exist pathname)
    (unwind-protect
         (progn
           (with-open-file (stream temporary
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (with-standard-io-syntax
               (let ((*print-readably* t)
                     (*print-pretty* t)
                     (*print-circle* t))
                 (dolist (form forms)
                   (prin1 form stream)
                   (terpri stream))))
             (finish-output stream))
           (sb-posix:chmod (namestring temporary) #o600)
           (uiop:rename-file-overwriting-target temporary pathname)
           (when write-date
             (conversation-identifier-migration--write-date pathname write-date))
           pathname)
      (when (probe-file temporary)
        (delete-file temporary)))))

(-> conversation-identifier-migration--read-forms (pathname) list)
(defun conversation-identifier-migration--read-forms (pathname)
  "Return every complete readable form in PATHNAME."
  (multiple-value-bind (forms incomplete-tail-p)
      (log-read pathname)
    (declare (ignore incomplete-tail-p))
    forms))

(-> conversation-identifier-migration--rewrite-file (pathname list) boolean)
(defun conversation-identifier-migration--rewrite-file (pathname entries)
  "Atomically rewrite exact legacy references in PATHNAME and report a change."
  (let* ((forms (conversation-identifier-migration--read-forms pathname))
         (rewritten
           (conversation-identifier-migration--rewrite-value forms entries)))
    (if (equalp forms rewritten)
        nil
        (let ((write-date (file-write-date pathname)))
          (conversation-identifier-migration--write-forms
           pathname rewritten :write-date write-date)
          t))))

(-> conversation-identifier-migration--conversation-path
    (configuration string)
    pathname)
(defun conversation-identifier-migration--conversation-path
    (configuration identifier)
  "Return the resumable conversation pathname for stored IDENTIFIER."
  (merge-pathnames (make-pathname :name identifier :type "sexp")
                   (configuration-conversation-root configuration)))

(-> conversation-identifier-migration--publish-conversations
    (configuration list)
    null)
(defun conversation-identifier-migration--publish-conversations
    (configuration entries)
  "Publish every rewritten conversation while retaining each legacy source."
  (dolist (entry entries)
    (let* ((old (getf entry :old))
           (new (getf entry :new))
           (source
             (conversation-identifier-migration--conversation-path
              configuration old))
           (target
             (conversation-identifier-migration--conversation-path
              configuration new)))
      (cond
        ((probe-file source)
         (let* ((forms
                  (conversation-identifier-migration--read-forms source))
                (rewritten
                  (conversation-identifier-migration--rewrite-value forms entries))
                (header (first rewritten)))
           (unless (and (listp header)
                        (eq (first header) :conversation)
                        (string= (or (getf (rest header) :id) "") new))
             (conversation-identifier-migration--signal
              configuration
              ':conversation
              (format nil "Legacy conversation ~A could not be rewritten to ~A."
                      source new)
              :pathname source))
           (if (probe-file target)
               (unless (equalp rewritten
                               (conversation-identifier-migration--read-forms
                                target))
                 (conversation-identifier-migration--signal
                  configuration
                  ':conversation
                  (format nil "Conversation migration target ~A conflicts with its source."
                          target)
                  :pathname target))
               (conversation-identifier-migration--write-forms
                target rewritten :write-date (file-write-date source)))))
        ((not (probe-file target))
         (conversation-identifier-migration--signal
          configuration
          ':conversation
          (format nil "Conversation migration lost both ~A and ~A."
                  source target)
          :pathname source)))))
  (let ((root (configuration-conversation-root configuration))
        (old-identifiers (mapcar (lambda (entry) (getf entry :old)) entries)))
    (dolist (pathname (uiop:directory-files root "*.sexp"))
      (unless (member (pathname-name pathname) old-identifiers :test #'string=)
        (conversation-identifier-migration--rewrite-file pathname entries))))
  nil)

(-> conversation-identifier-migration--sexp-files-recursively (pathname) list)
(defun conversation-identifier-migration--sexp-files-recursively (root)
  "Return every S-expression file recursively beneath existing ROOT."
  (labels ((collect-files (directory)
             "Collect S-expression files below DIRECTORY."
             (append (uiop:directory-files directory "*.sexp")
                     (mapcan #'collect-files (uiop:subdirectories directory)))))
    (if (uiop:directory-exists-p root)
        (collect-files (uiop:ensure-directory-pathname root))
        nil)))

(-> conversation-identifier-migration--rewrite-references
    (configuration list)
    null)
(defun conversation-identifier-migration--rewrite-references
    (configuration entries)
  "Rewrite durable memory, crash, and task-artifact references in place."
  (let ((memory (configuration-memory-path configuration)))
    (when (probe-file memory)
      (conversation-identifier-migration--rewrite-file memory entries)))
  (let ((crash-root
          (merge-pathnames "crashes/"
                           (configuration-state-root configuration))))
    (when (uiop:directory-exists-p crash-root)
      (dolist (pathname (uiop:directory-files crash-root "*.sexp"))
        (conversation-identifier-migration--rewrite-file pathname entries))))
  (dolist (pathname
           (conversation-identifier-migration--sexp-files-recursively
            (merge-pathnames "tasks/"
                             (configuration-data-root configuration))))
    (conversation-identifier-migration--rewrite-file pathname entries))
  nil)

(-> conversation-identifier-migration--move-directory
    (configuration pathname pathname)
    null)
(defun conversation-identifier-migration--move-directory
    (configuration source target)
  "Idempotently rename contained SOURCE to unoccupied TARGET."
  (let ((source-p (uiop:directory-exists-p source))
        (target-p (uiop:directory-exists-p target)))
    (cond
      ((and source-p target-p)
       (conversation-identifier-migration--signal
        configuration
        ':artifact
        (format nil "Both artifact directories exist during migration: ~A and ~A."
                source target)
        :pathname target))
      (source-p
       (rename-file source target))
      (t
       nil)))
  nil)

(-> conversation-identifier-migration--move-artifacts
    (configuration list)
    null)
(defun conversation-identifier-migration--move-artifacts (configuration entries)
  "Move identifier-keyed image and task artifact directories."
  (dolist (entry entries)
    (let ((old (getf entry :old))
          (new (getf entry :new)))
      (conversation-identifier-migration--move-directory
       configuration
       (merge-pathnames (format nil "conversation-images/~A/" old)
                        (configuration-data-root configuration))
       (merge-pathnames (format nil "conversation-images/~A/" new)
                        (configuration-data-root configuration)))
      (conversation-identifier-migration--move-directory
       configuration
       (merge-pathnames (format nil "tasks/~A/" (string-downcase old))
                        (configuration-data-root configuration))
       (merge-pathnames (format nil "tasks/~A/" new)
                        (configuration-data-root configuration)))))
  nil)

(-> conversation-identifier-migration--remove-sources
    (configuration list)
    null)
(defun conversation-identifier-migration--remove-sources
    (configuration entries)
  "Remove each legacy conversation only after its replacement is durable."
  (dolist (entry entries)
    (let ((source
            (conversation-identifier-migration--conversation-path
             configuration (getf entry :old)))
          (target
            (conversation-identifier-migration--conversation-path
             configuration (getf entry :new))))
      (unless (probe-file target)
        (conversation-identifier-migration--signal
         configuration
         ':cleanup
         (format nil "Conversation migration target ~A disappeared before cleanup."
                 target)
         :pathname target))
      (when (probe-file source)
        (delete-file source))))
  nil)

(-> conversation-identifier-migration--call-with-file-lock
    (configuration function)
    list)
(defun conversation-identifier-migration--call-with-file-lock
    (configuration function)
  "Call FUNCTION while holding CONFIGURATION's process-shared migration lock."
  (let* ((pathname
           (merge-pathnames "conversation-identifier-migration.lock"
                            (configuration-state-root configuration)))
         (descriptor nil))
    (ensure-directories-exist pathname)
    (unwind-protect
         (handler-case
             (progn
               (setf descriptor
                     (sb-posix:open
                      (namestring pathname)
                      (logior sb-posix:o-creat sb-posix:o-rdwr)
                      #o600))
               (sb-posix:lockf descriptor sb-posix:f-lock 0)
               (funcall function))
           (conversation-identifier-migration-error (condition)
             (error condition))
           (conversation-identifier-space-exhausted (condition)
             (error condition))
           (error (cause)
             (conversation-identifier-migration--signal
              configuration
              ':planning
              (format nil "Could not lock conversation identifier migration: ~A"
                      cause)
              :pathname pathname
              :cause cause)))
      (when descriptor
        (ignore-errors (sb-posix:lockf descriptor sb-posix:f-ulock 0))
        (ignore-errors (sb-posix:close descriptor))))))

(-> conversation-identifier-migrate (configuration) list)
(defun conversation-identifier-migrate (configuration)
  "Migrate legacy resumable conversation identifiers and return the mappings.

The durable phase record makes every atomic file replacement and directory
rename idempotent after interruption. A completed record remains as the alias
map for old resume commands and retained generations."
  (with-lock-held (*conversation-identifier-migration-lock*)
    (conversation-identifier-migration--call-with-file-lock
     configuration
     (lambda ()
       (let* ((record (conversation-identifier-migration--read configuration))
              (legacy-files
                (conversation-identifier-migration--legacy-files configuration)))
         (multiple-value-bind (entries work-p)
             (conversation-identifier-migration--plan
              configuration record legacy-files)
           (when work-p
             (conversation-identifier-migration--write
              configuration ':prepared entries)
             (let ((stage ':conversation))
               (handler-case
                   (progn
                     (conversation-identifier-migration--publish-conversations
                      configuration entries)
                     (conversation-identifier-migration--write
                      configuration ':conversations entries)
                     (setf stage ':reference)
                     (conversation-identifier-migration--rewrite-references
                      configuration entries)
                     (conversation-identifier-migration--write
                      configuration ':references entries)
                     (setf stage ':artifact)
                     (conversation-identifier-migration--move-artifacts
                      configuration entries)
                     (conversation-identifier-migration--write
                      configuration ':artifacts entries)
                     (setf stage ':cleanup)
                     (conversation-identifier-migration--remove-sources
                      configuration entries)
                     (conversation-identifier-migration--write
                      configuration ':complete entries))
                 (conversation-identifier-migration-error (condition)
                   (error condition))
                 (error (cause)
                   (conversation-identifier-migration--signal
                    configuration
                    stage
                    (format nil "Conversation identifier migration failed: ~A"
                            cause)
                    :cause cause)))))
           entries))))))

(-> conversation-identifier-migration-resolve
    (configuration string)
    string)
(defun conversation-identifier-migration-resolve (configuration identifier)
  "Resolve new display syntax or a retained legacy alias to stored form."
  (handler-case
      (conversation-identifier-normalize identifier)
    (conversation-identifier-error ()
      (let* ((record (conversation-identifier-migration--read configuration))
             (entry
               (and record
                    (conversation-identifier-migration--entry-for-old
                     identifier (getf (rest record) :entries)))))
        (if entry (getf entry :new) identifier)))))
