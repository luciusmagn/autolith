(in-package #:autolith)

;;;; -- Conversation Identifier Tests --

(-> test-conversation-identifier-format () null)
(defun test-conversation-identifier-format ()
  "Test deterministic derivation, parsing, display, and timestamp reduction."
  (let ((cases '((0  . "13VNGTr")
                 (1  . "25eRAfG")
                 (10 . "B4JFq84")
                 (31 . "Y65Hc5f")
                 (57 . "z7435Cs"))))
    (dolist (case cases)
      (test-assert
       (string= (conversation-identifier-from-seed 3994000000 (first case))
                (rest case))
       "seed parameter derivation has stable portable vectors")))
  (test-assert
   (string= (conversation-identifier-from-seed
             (+ 3994000000 +conversation-identifier-modulus+)
             10)
            "B4JFq84")
   "Universal Time is reduced modulo 2^32")
  (test-assert (string= (conversation-identifier-normalize "K-8vQ2mp")
                        "K8vQ2mp")
               "the displayed identifier normalizes to stored form")
  (test-assert (string= (conversation-identifier-normalize "K8vQ2mp")
                        "K8vQ2mp")
               "the stored identifier normalizes unchanged")
  (test-assert (string= (conversation-identifier-display "K8vQ2mp")
                        "K-8vQ2mp")
               "stored identifiers display with one visual hyphen")
  (test-assert
   (and (conversation-identifier-migration--legacy-identifier-p
         "cb472f21-969d-48f5-9c1e-e793d19054b9")
        (not (conversation-identifier-migration--legacy-identifier-p
              "arbitrary-legacy-name")))
   "legacy migration recognizes only historical UUID identifiers")
  (dolist (invalid '("K-8vQ2m" "K08vQ2m" "K-8vQ2m0" "k-8vq2m0" 42))
    (test-assert
     (handler-case
         (progn (conversation-identifier-normalize invalid) nil)
       (conversation-identifier-error ()
         t))
     "malformed or non-Base58 identifiers are rejected structurally"))
  nil)

(-> test-conversation-identifier-allocation () null)
(defun test-conversation-identifier-allocation ()
  "Test random first seeds, collision probing, and structured exhaustion."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (storage (configuration-conversation-root configuration))
         (timestamp 3994000000)
         (*conversation-identifier-reservations* (make-hash-table :test #'equal))
         (*conversation-identifier-random-index-function* (lambda (limit)
                                                            (declare (ignore limit))
                                                            10)))
    (unwind-protect
         (let* ((first (conversation-identifier-generate
                        storage :timestamp timestamp))
                (second (conversation-identifier-generate
                         storage :timestamp timestamp)))
           (test-assert (string= first "B4JFq84")
                        "allocation begins at the random seed")
           (test-assert
            (string= second
                     (conversation-identifier-from-seed timestamp 11))
            "allocation probes the next seed after a collision")
           (let ((reserved
                   (loop for seed below +conversation-identifier-base+
                         collect (conversation-identifier-from-seed
                                  timestamp seed))))
             (test-assert
              (handler-case
                  (progn
                    (conversation-identifier-generate
                     storage
                     :timestamp timestamp
                     :reserved-identifiers reserved)
                    nil)
                (conversation-identifier-space-exhausted (condition)
                  (= (conversation-identifier-space-exhausted-timestamp
                      condition)
                     timestamp)))
              "occupying all 58 seeds signals structured exhaustion")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-identifier--legacy-conversation
    (configuration string string)
    conversation)
(defun test-conversation-identifier--legacy-conversation
    (configuration identifier content)
  "Create and persist one legacy test conversation."
  (let ((conversation
          (conversation-create configuration :identifier identifier)))
    (conversation-append-user-message conversation content)
    conversation))

(-> test-conversation-identifier-migration () null)
(defun test-conversation-identifier-migration ()
  "Test complete durable-reference migration, aliases, and idempotence."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (old "cb472f21-969d-48f5-9c1e-e793d19054b9")
         (other "de916e8a-8227-443d-9e10-11794a62ebd6")
         (*conversation-identifier-reservations* (make-hash-table :test #'equal))
         (*conversation-identifier-random-index-function* (lambda (limit)
                                                            (declare (ignore limit))
                                                            10)))
    (unwind-protect
         (let* ((conversation
                  (test-conversation-identifier--legacy-conversation
                   configuration old (format nil "related conversation ~A" other)))
                (other-conversation
                  (test-conversation-identifier--legacy-conversation
                   configuration other "second legacy conversation"))
                (old-path (conversation-pathname conversation))
                (old-image-root
                  (merge-pathnames (format nil "conversation-images/~A/" old)
                                   (configuration-data-root configuration)))
                (old-task-root
                  (merge-pathnames (format nil "tasks/~A/" old)
                                   (configuration-data-root configuration)))
                (old-task-result (merge-pathnames "run/result.sexp" old-task-root))
                (crash
                  (merge-pathnames "crashes/legacy.sexp"
                                   (configuration-state-root configuration))))
           (declare (ignore other-conversation))
           (ensure-directories-exist (merge-pathnames "image.png" old-image-root))
           (with-open-file (stream (merge-pathnames "image.png" old-image-root)
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (write-string "artifact" stream))
           (snapshot-write
            old-task-result
            (list :task-result
                  :conversation-id old
                  :conversation-path
                  (namestring
                   (merge-pathnames (format nil "run/~A.sexp" old)
                                    old-task-root))))
           (snapshot-write
            crash
            (list :crash :version 1 :id "crash" :conversation-id old))
           (memory-remember configuration
                            :title "migration memory"
                            :content "remember the migrated conversation"
                            :scope ':global
                            :tags nil
                            :source-conversation old)
           (with-open-file (stream old-path
                                   :direction :output
                                   :if-exists :append
                                   :external-format :utf-8)
             (write-string "(:interrupted" stream))
           (let* ((old-write-date (file-write-date old-path))
                  (entries (conversation-identifier-migrate configuration))
                  (entry
                    (conversation-identifier-migration--entry-for-old
                     old entries))
                  (other-entry
                    (conversation-identifier-migration--entry-for-old
                     other entries))
                  (new (getf entry :new))
                  (other-new (getf other-entry :new))
                  (new-path
                    (conversation-identifier-migration--conversation-path
                     configuration new))
                  (new-image-root
                    (merge-pathnames
                     (format nil "conversation-images/~A/" new)
                     (configuration-data-root configuration)))
                  (new-task-root
                    (merge-pathnames (format nil "tasks/~A/" new)
                                     (configuration-data-root configuration)))
                  (new-task-result
                    (merge-pathnames "run/result.sexp" new-task-root))
                  (task-record (snapshot-read new-task-result))
                  (memory (first (memory-list configuration :visibility ':all)))
                  (record
                    (snapshot-read
                     (configuration-conversation-identifier-migration-path
                      configuration))))
             (test-assert (and (conversation-identifier-stored-p new)
                               (conversation-identifier-stored-p other-new)
                               (not (string= new other-new)))
                          "migration assigns distinct canonical identifiers")
             (test-assert (and (not (probe-file old-path))
                               (probe-file new-path)
                               (= (file-write-date new-path) old-write-date))
                          "conversation replacement is atomic and preserves activity time")
             (test-assert
              (string= (conversation-identifier
                        (conversation-load-by-id
                         configuration
                         (conversation-identifier-display new)))
                       new)
              "displayed identifiers resolve to canonical stored conversations")
             (test-assert
              (string= (conversation-identifier
                        (conversation-load-by-id configuration old))
                       new)
              "legacy resume identifiers remain durable aliases")
             (test-assert
              (search other-new
                      (getf (rest (second
                                   (conversation-identifier-migration--read-forms
                                    new-path)))
                            :content))
              "cross-conversation references in durable history are rewritten")
             (test-assert
              (and (uiop:directory-exists-p new-image-root)
                   (not (uiop:directory-exists-p old-image-root))
                   (probe-file (merge-pathnames "image.png" new-image-root)))
              "identifier-keyed image artifacts move with the conversation")
             (test-assert
              (and (uiop:directory-exists-p new-task-root)
                   (not (uiop:directory-exists-p old-task-root))
                   (string= (getf (rest task-record) :conversation-id) new)
                   (search new (getf (rest task-record) :conversation-path))
                   (not (search old
                                (getf (rest task-record) :conversation-path))))
              "task artifacts and their internal path references migrate together")
             (test-assert
              (string= (memory-source-conversation memory) new)
              "persistent memory source references use the migrated identifier")
             (test-assert
              (string= (getf (rest (snapshot-read crash)) :conversation-id) new)
              "crash capsule references use the migrated identifier")
             (test-assert
              (and (eq (getf (rest record) :status) ':complete)
                   (= (length (getf (rest record) :entries)) 2))
              "the retained alias record marks the complete migration")
             (test-assert
              (equal entries (conversation-identifier-migrate configuration))
              "running a completed migration again is idempotent")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-identifier-migration-resumption () null)
(defun test-conversation-identifier-migration-resumption ()
  "Test restart from a durable phase after new conversations were published."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (old "88e2a524-d03c-48ea-99ad-fcfa17416f10")
         (*conversation-identifier-reservations* (make-hash-table :test #'equal))
         (*conversation-identifier-random-index-function* (lambda (limit)
                                                            (declare (ignore limit))
                                                            31)))
    (unwind-protect
         (progn
           (test-conversation-identifier--legacy-conversation
            configuration old "resume an interrupted migration")
           (let* ((legacy
                    (conversation-identifier-migration--legacy-files
                     configuration))
                  (entries
                    (multiple-value-bind (planned work-p)
                        (conversation-identifier-migration--plan
                         configuration nil legacy)
                      (test-assert work-p "legacy data creates migration work")
                      planned)))
             (conversation-identifier-migration--write
              configuration ':prepared entries)
             (conversation-identifier-migration--publish-conversations
              configuration entries)
             (conversation-identifier-migration--write
              configuration ':conversations entries)
             (let* ((completed (conversation-identifier-migrate configuration))
                    (new (getf (first completed) :new)))
               (test-assert
                (and (not (probe-file
                           (conversation-identifier-migration--conversation-path
                            configuration old)))
                     (probe-file
                      (conversation-identifier-migration--conversation-path
                       configuration new))
                     (eq (getf (rest
                                (conversation-identifier-migration--read
                                 configuration))
                               :status)
                         ':complete))
                "a repeated migration safely completes every remaining phase"))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-conversation-identifiers () null)
(defun test-conversation-identifiers ()
  "Test the complete human-friendly conversation identifier subsystem."
  (test-conversation-identifier-format)
  (test-conversation-identifier-allocation)
  (test-conversation-identifier-migration)
  (test-conversation-identifier-migration-resumption)
  nil)
