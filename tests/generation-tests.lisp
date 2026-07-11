(in-package #:frob)

;;;; -- Subsystem Tests --

(-> test-generation-manifest () null)
(defun test-generation-manifest ()
  "Test generation publication, loading, selection, and compatibility checks."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (directory (merge-pathnames "generation-under-test/"
                                     (generation-root configuration)))
         (generation
           (make-instance 'generation
                          :identifier "generation-under-test"
                          :directory directory
                          :core-pathname (merge-pathnames "frob.core" directory)
                          :temporary-core-pathname
                          (merge-pathnames ".frob.core.tmp" directory)
                          :manifest-pathname
                          (merge-pathnames "manifest.sexp" directory)
                          :git-commit "0123456789abcdef"
                          :journal-position 27
                          :created-at 4000000000
                          :status ':pending)))
    (unwind-protect
         (progn
           (ensure-directories-exist
            (generation-temporary-core-pathname generation))
           (with-open-file (stream (generation-temporary-core-pathname generation)
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :element-type '(unsigned-byte 8))
             (write-byte 42 stream))
           (generation-publish configuration generation)
           (let ((loaded (generation-find configuration
                                          "generation-under-test")))
             (test-assert loaded
                          "a published generation appears in retained listings")
             (test-assert (not (generation-compatible-p loaded))
                          "a fake one-byte core is never reported as bootable")
             (test-assert
              (let ((*checkpoint-in-progress-p* t))
                (handler-case
                    (progn
                      (generation-select configuration loaded)
                      nil)
                  (checkpoint-error (condition)
                    (search "while a checkpoint publishes"
                            (frob-error-message condition)))))
              "rollback selection cannot race asynchronous publication")
             (test-assert (= (generation-journal-position loaded) 27)
                          "generation manifests preserve mutation journal position")
             (test-assert
              (string= (generation-identifier
                        (generation-selected configuration))
                       "generation-under-test")
              "publication atomically selects the ready generation")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(-> test-crash-capsule-correlation () null)
(defun test-crash-capsule-correlation ()
  "Test secret-free crash capsules and per-launch pointer publication."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation
           (conversation-create configuration :identifier "crash-capsule"))
         (application
           (make-instance 'application
                          :configuration configuration
                          :conversation conversation
                          :provider nil
                          :tool-registry (make-instance 'tool-registry)
                          :worker nil
                          :agent nil
                          :ui nil))
         (pointer (merge-pathnames "crash-pointers/test-launch.path"
                                   (configuration-state-root configuration)))
         (previous-pointer (uiop:getenv "FROB_CRASH_POINTER")))
    (unwind-protect
         (progn
           (setf (application-rendered-sequence application) 42)
           (sb-posix:setenv "FROB_CRASH_POINTER" (namestring pointer) 1)
           (test-assert (string= (uiop:getenv "FROB_CRASH_POINTER")
                                 (namestring pointer))
                        "the launch pointer is visible in the active environment")
           (test-assert (uiop:subpathp pointer
                                       (configuration-state-root configuration))
                        "the launch pointer is contained by private Frob state")
           (let* ((capsule
                    (application-write-crash-capsule
                     application
                     (make-condition 'simple-error
                                     :format-control "secret ~A"
                                     :format-arguments '("credential-value"))
                     :backtrace '((secret-frame "credential-value"))))
                  (record (read-portable-form capsule))
                  (mode (sb-posix:stat-mode
                         (sb-posix:stat (namestring capsule)))))
             (test-assert (= (logand mode #o777) #o600)
                          "crash capsules are private user state")
             (test-assert
              (not (search "credential-value"
                           (uiop:read-file-string capsule)))
              "crash capsules never serialize arbitrary condition arguments")
             (test-assert (= (getf (rest record) :rendered-sequence) 42)
                          "crash capsules retain scrollback presentation progress")
             (test-assert
              (string= (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    (uiop:read-file-string pointer))
                       (namestring capsule))
              "the exact launch pointer names its own crash capsule")))
      (if previous-pointer
          (sb-posix:setenv "FROB_CRASH_POINTER" previous-pointer 1)
          (sb-posix:unsetenv "FROB_CRASH_POINTER"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
