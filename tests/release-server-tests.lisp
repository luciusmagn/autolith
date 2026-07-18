(in-package #:autolith)

;;;; -- Release Server Tests --

(-> release-server-tests--write-file (pathname string) pathname)
(defun release-server-tests--write-file (pathname content)
  "Write test CONTENT to PATHNAME and return PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream))
  pathname)

(-> test-release-server () null)
(defun test-release-server ()
  "Test semantic release selection and strict HTTP routing."
  (test-assert (release-tag-valid-p "v0.11.1")
               "three-component release tags are valid")
  (test-assert (not (release-tag-valid-p "0.11.1"))
               "release tags require their v prefix")
  (test-assert (not (release-tag-valid-p "v0.11.1.2"))
               "release tags reject extra components")
  (test-assert (release-tag< "v0.9.12" "v0.10.1")
               "release tags compare numeric components")
  (let* ((root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-release-server-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (source-root (merge-pathnames "source/" root))
         (public-root (merge-pathnames "public/" root))
         (configuration
           (release-server-configuration-create
            :source-root source-root
            :public-root public-root
            :address "127.0.0.1"
            :port 18098)))
    (unwind-protect
         (progn
           (release-server-tests--write-file
            (merge-pathnames "script/install" source-root)
            (format nil "#!/bin/sh~%"))
           (dolist (tag '("v0.9.12" "v0.10.1"))
             (let* ((directory
                      (release-server--release-directory configuration tag))
                    (archive (release-server--archive-name tag)))
               (release-server-tests--write-file
                (merge-pathnames archive directory)
                "archive")
               (release-server-tests--write-file
                (merge-pathnames (format nil "~A.sha256" archive) directory)
                "checksum")))
           (release-server-tests--write-file
            (merge-pathnames
             (release-server--archive-name "v0.12.0")
             (release-server--release-directory configuration "v0.12.0"))
            "incomplete")
           (test-assert
            (equal (release-server-published-tags configuration)
                   '("v0.9.12" "v0.10.1"))
            "only complete releases enter the semantic publication index")
           (test-assert
            (string= (release-server-latest-tag configuration) "v0.10.1")
            "the newest complete release becomes latest")
           (let ((response (release-server-route configuration "GET" "/autolith")))
             (test-assert (= (release-server-response-status response) 200)
                          "the installer route succeeds")
             (test-assert (pathnamep (release-server-response-body response))
                          "the installer route serves the tracked script"))
           (test-assert
            (string= (release-server-response-body
                      (release-server-route configuration "GET" "/health"))
                     (format nil "ok~%"))
            "health responses contain a real line ending")
           (let ((response
                   (release-server-route configuration "GET" "/releases/latest")))
             (test-assert (= (release-server-response-status response) 302)
                          "the latest route redirects")
             (test-assert
              (equal (assoc "Location"
                            (release-server-response-headers response)
                            :test #'string=)
                     '("Location" . "/releases/v0.10.1"))
              "the latest redirect names the newest complete release"))
           (let* ((tag "v0.10.1")
                  (archive (release-server--archive-name tag))
                  (response
                    (release-server-route
                     configuration "HEAD"
                     (format nil "/releases/~A/~A" tag archive))))
             (test-assert (= (release-server-response-status response) 200)
                          "published archives support HEAD")
             (test-assert
              (string= (release-server-response-content-type response)
                       "application/gzip")
              "archive responses use the gzip media type"))
           (test-assert
            (= (release-server-response-status
                (release-server-route
                 configuration "GET" "/releases/v0.10.1/../secret"))
               404)
            "release routes reject filesystem traversal")
           (test-assert
            (= (release-server-response-status
                (release-server-route configuration "POST" "/autolith"))
               405)
            "release routes reject mutating HTTP methods")
           (multiple-value-bind (method target)
               (release-server--parse-request-head
                (format nil "GET /health HTTP/1.1~C~CHost: localhost~C~C~C~C"
                        #\Return #\Newline #\Return #\Newline
                        #\Return #\Newline))
             (test-assert (and (string= method "GET")
                               (string= target "/health"))
                          "HTTP request parsing returns its method and target"))
           (test-assert
            (handler-case
                (progn
                  (release-server--parse-request-head
                   (format nil "broken~C~C~C~C"
                           #\Return #\Newline #\Return #\Newline))
                  nil)
              (release-server-request-error ()
                t))
            "malformed HTTP request lines signal a structured condition"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
