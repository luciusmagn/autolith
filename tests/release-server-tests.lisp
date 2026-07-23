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

(-> release-server-tests--source-tag (string string) release-source-tag)
(defun release-server-tests--source-tag (name commit)
  "Create one release source-tag fixture with NAME and COMMIT."
  (make-instance 'release-source-tag :name name :commit commit))

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
           (let* ((tag "v0.10.1")
                  (checksum
                    (format nil "~A.sha256" (release-server--archive-name tag)))
                  (response
                    (release-server-route
                     configuration "GET"
                     (format nil "/releases/~A/~A" tag checksum))))
             (test-assert
              (string= (release-server-response-content-type response)
                       "text/plain; charset=utf-8")
              "checksum responses use the plain-text media type"))
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
  (let* ((root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-release-identity-tests-~A/"
                     (make-identifier))
             (uiop:temporary-directory))))
         (source-a (merge-pathnames "source-a/" root))
         (source-b (merge-pathnames "source-b/" root))
         (archive-a (merge-pathnames "identity-a.tar" root))
         (archive-b (merge-pathnames "identity-b.tar" root)))
    (unwind-protect
         (progn
           (release-server-tests--write-file
            (merge-pathnames "fixture.txt" source-a)
            "deterministic release identity")
           (release-server-tests--write-file
            (merge-pathnames "fixture.txt" source-b)
            "deterministic release identity")
           (release-archive--run
            (list "touch" "-d" "@1"
                  (namestring (merge-pathnames "fixture.txt" source-a))))
           (release-archive--run
            (list "touch" "-d" "@2"
                  (namestring (merge-pathnames "fixture.txt" source-b))))
           (release-archive--create-source-identity
            source-a "v0.16.0" "1700000000")
           (release-archive--create-source-identity
            source-b "v0.16.0" "1700000000")
           (dolist (entry (list (list source-a archive-a)
                                (list source-b archive-b)))
             (release-archive--run
              (list "tar" "--sort=name" "--mtime=@0"
                    "--owner=0" "--group=0" "--numeric-owner"
                    "-cf" (namestring (second entry)) ".git")
              :directory (first entry)))
           (let ((checksum-a
                   (first
                    (uiop:split-string
                     (release-archive--run
                      (list "sha256sum" (namestring archive-a))
                      :output ':string
                      :error-output ':output)
                     :separator '(#\Space #\Tab))))
                 (checksum-b
                   (first
                    (uiop:split-string
                     (release-archive--run
                      (list "sha256sum" (namestring archive-b))
                      :output ':string
                      :error-output ':output)
                     :separator '(#\Space #\Tab)))))
             (test-assert
              (string= checksum-a checksum-b)
              "packaged source identities ignore source stat metadata"))
           (test-assert
            (and (not (probe-file (merge-pathnames ".git/hooks/" source-a)))
                 (not (probe-file (merge-pathnames ".git/logs/" source-a))))
            "packaged source identities omit template and reflog state"))
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist :ignore)))
  (let* ((root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-release-archive-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (dependency-root (merge-pathnames "dependencies/" root))
         (target-root (merge-pathnames "target/" root))
         (valid-link (merge-pathnames "valid" dependency-root))
         (broken-link (merge-pathnames "broken" dependency-root)))
    (unwind-protect
         (progn
           (release-server-tests--write-file
            (merge-pathnames "system.asd" target-root)
            "(asdf:defsystem #:fixture)")
           (ensure-directories-exist (merge-pathnames ".keep" dependency-root))
           (sb-posix:symlink (namestring target-root) (namestring valid-link))
           (sb-posix:symlink (namestring (merge-pathnames "missing/" root))
                             (namestring broken-link))
           (release-archive--materialize-dependency-links dependency-root)
           (test-assert
            (probe-file (merge-pathnames "valid/system.asd" dependency-root))
            "release archives replace dependency links with private copies")
           (test-assert
            (not (probe-file broken-link))
            "release archives remove broken dependency links"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (let* ((commit-a "0123456789abcdef0123456789abcdef01234567")
         (commit-b "89abcdef0123456789abcdef0123456789abcdef")
         (parsed
           (release-builder--parse-remote-tags
            (format nil "~A~Crefs/tags/v0.11.2~%~A~Crefs/tags/v0.10.9~%"
                    commit-a #\Tab commit-b #\Tab))))
    (test-assert
     (equal (mapcar #'release-source-tag-name parsed)
            '("v0.10.9" "v0.11.2"))
     "remote release tags are validated and sorted semantically"))
  (let* ((root
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "autolith-release-builder-tests-~A/" (make-identifier))
             (uiop:temporary-directory))))
         (source-root (merge-pathnames "source/" root))
         (public-root (merge-pathnames "public/" root))
         (state-root (merge-pathnames "state/" root))
         (builder
           (release-builder-configuration-create
            :source-root source-root
            :state-root state-root
            :public-root public-root
            :repository "https://example.invalid/autolith.git"
            :poll-seconds 30
            :container-command "container-test"))
         (commit "0123456789abcdef0123456789abcdef01234567")
         (tags (list (release-server-tests--source-tag "v0.11.0" commit)
                     (release-server-tests--source-tag "v0.11.1" commit)
                     (release-server-tests--source-tag "v0.12.0" commit))))
    (unwind-protect
         (progn
           (test-assert
            (equal (mapcar #'release-source-tag-name
                           (release-builder-pending-tags builder tags))
                   '("v0.12.0"))
            "an empty builder starts from only the newest remote tag")
           (let* ((server
                    (release-server-configuration-create
                     :source-root source-root
                     :public-root public-root))
                  (tag "v0.11.0")
                  (directory
                    (release-server--release-directory server tag))
                  (archive (release-server--archive-name tag)))
             (release-server-tests--write-file
              (merge-pathnames archive directory) "archive")
             (release-server-tests--write-file
              (merge-pathnames (format nil "~A.sha256" archive) directory)
              "checksum"))
           (test-assert
            (equal (mapcar #'release-source-tag-name
                           (release-builder-pending-tags builder tags))
                   '("v0.11.1" "v0.12.0"))
            "a builder catches up every tag newer than its latest publication")
           (release-server-tests--write-file
            (merge-pathnames "autolith.asd" source-root)
            (format nil
                    "(asdf:defsystem #:autolith~%  :version \"0.11.2\"~%)~%"))
           (test-assert
            (string= (release-builder--source-version source-root) "0.11.2")
            "builder source validation reads the declared ASDF version"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
