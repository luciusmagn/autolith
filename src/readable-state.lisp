(in-package #:autolith)

;;;; -- Private Readable State --

(-> readable-state-property-present-p (list keyword) boolean)
(defun readable-state-property-present-p (properties property)
  "Return true when PROPERTIES contains PROPERTY as a property-list key."
  (not (null (loop for tail on properties by #'cddr
                   thereis (eq (first tail) property)))))

(-> readable-state-read-form (pathname) (values t boolean))
(defun readable-state-read-form (pathname)
  "Read PATHNAME with evaluation disabled.

Return the first form and true only when it is the file's sole complete form."
  (with-open-file (stream pathname
                          :direction :input
                          :external-format :utf-8)
    (let* ((*read-eval* nil)
           (end-marker (cons nil nil))
           (form (read stream nil end-marker))
           (extra (read stream nil end-marker)))
      (values form
              (and (not (eq form end-marker))
                   (eq extra end-marker))))))

(-> readable-state-write-form
    (pathname t &key (:mode (integer 0 #o777)))
    pathname)
(defun readable-state-write-form (pathname form &key (mode #o600))
  "Atomically write portable FORM to PATHNAME with private file MODE."
  (let ((temporary
          (make-pathname
           :name (format nil ".~A.~A"
                         (pathname-name pathname)
                         (make-identifier))
           :type "tmp"
           :defaults pathname)))
    (unwind-protect
         (progn
           (ensure-directories-exist pathname)
           (with-open-file (stream temporary
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
             (let ((*print-circle* t)
                   (*print-readably* t)
                   (*print-pretty* t))
               (prin1 form stream)
               (terpri stream)
               (finish-output stream)))
           (sb-posix:chmod (namestring temporary) mode)
           (uiop:rename-file-overwriting-target temporary pathname)
           (sb-posix:chmod (namestring pathname) mode))
      (when (probe-file temporary)
        (delete-file temporary))))
  pathname)
