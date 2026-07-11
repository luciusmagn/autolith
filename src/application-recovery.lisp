(in-package #:frob)

;;;; -- Fatal Control Path --

(define-condition fatal-control-path-error (frob-error)
  ((cause
    :initarg :cause
    :reader fatal-control-path-error-cause
    :type serious-condition
    :documentation "The unexpected condition that made the active path untrustworthy.")
   (capsule-pathname
    :initarg :capsule-pathname
    :reader fatal-control-path-error-capsule-pathname
    :type pathname
    :documentation "The best-effort crash capsule written before recovery."))
  (:documentation "An unexpected active-agent failure requiring stable recovery."))

(-> application-safe-backtrace () list)
(defun application-safe-backtrace ()
  "Return bounded call names without argument values from the current stack."
  (handler-case
      (mapcar #'first
              (sb-debug:list-backtrace :count 30
                                       :argument-limit 0
                                       :from :current-frame))
    (error ()
      nil)))

(-> application-crash-condition-report (serious-condition) string)
(defun application-crash-condition-report (condition)
  "Return an allowlisted, secret-free report for crash CONDITION."
  (if (typep condition 'frob-error)
      (frob-error-message condition)
      (format nil "Unexpected condition of type ~S." (type-of condition))))

(-> application-publish-crash-pointer (application pathname) null)
(defun application-publish-crash-pointer (application capsule-pathname)
  "Publish CAPSULE-PATHNAME to this launcher's private pointer, when configured."
  (let ((pointer-value (uiop:getenv "FROB_CRASH_POINTER")))
    (when (non-empty-string-p pointer-value)
      (let* ((configuration (application-configuration application))
             (pointer-pathname (pathname pointer-value))
             (temporary-pathname
               (make-pathname :name (format nil ".crash-pointer.~D"
                                            (sb-posix:getpid))
                              :type "tmp"
                              :defaults pointer-pathname)))
        (when (uiop:subpathp pointer-pathname
                             (configuration-state-root configuration))
          (ensure-directories-exist pointer-pathname)
          (with-open-file (stream temporary-pathname
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
            (write-line (namestring capsule-pathname) stream)
            (finish-output stream))
          (sb-posix:chmod (namestring temporary-pathname) #o600)
          (uiop:rename-file-overwriting-target temporary-pathname
                                               pointer-pathname)))))
  nil)

(-> application-write-crash-capsule
    (application serious-condition &key (:backtrace list))
    pathname)
(defun application-write-crash-capsule
    (application condition &key (backtrace (application-safe-backtrace)))
  "Write a secret-free crash capsule for CONDITION and return its pathname."
  (let* ((configuration (application-configuration application))
         (identifier (make-identifier))
         (pathname (merge-pathnames
                    (make-pathname :name identifier :type "sexp")
                    (merge-pathnames "crashes/"
                                     (configuration-state-root configuration))))
         (commit
           (handler-case
               (string-trim
                '(#\Space #\Tab #\Newline #\Return)
                (self-git-command configuration '("rev-parse" "HEAD")))
             (error ()
               nil)))
         (safe-backtrace
           (mapcar (lambda (frame-name)
                     (if (or (symbolp frame-name) (stringp frame-name))
                         (bounded-string frame-name :limit 300)
                         (bounded-string (type-of frame-name) :limit 300)))
                   (subseq backtrace 0 (min 30 (length backtrace))))))
    (generation--write-form-atomically
     pathname
     (list :crash
           :version 1
           :id identifier
           :time (get-universal-time)
           :condition-type (bounded-string (type-of condition) :limit 300)
           :condition (application-crash-condition-report condition)
           :backtrace safe-backtrace
           :conversation-id
           (conversation-identifier (application-conversation application))
           :rendered-sequence (application-rendered-sequence application)
           :git-commit commit
           :journal-position
           (let ((journal (configuration-journal-path configuration)))
             (if (probe-file journal)
                 (with-open-file (stream journal
                                         :direction :input
                                         :element-type '(unsigned-byte 8))
                   (file-length stream))
                 0))))
    (sb-posix:chmod (namestring pathname) #o600)
    (application-publish-crash-pointer application pathname)
    pathname))

(-> application-raise-fatal
    (application serious-condition list)
    null)
(defun application-raise-fatal (application condition backtrace)
  "Write fatal CONDITION context and leave APPLICATION through recovery status."
  (let ((capsule (application-write-crash-capsule
                  application condition :backtrace backtrace)))
    (error 'fatal-control-path-error
           :message "The active agent path failed unexpectedly."
           :cause condition
           :capsule-pathname capsule)))
