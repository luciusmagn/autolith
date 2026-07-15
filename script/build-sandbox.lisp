(require :asdf)

(let ((installed-helper (uiop:getenv "CL_EXEC_SANDBOX_HELPER")))
  (unless (and installed-helper (probe-file installed-helper))
    (let* ((system-root (asdf:system-source-directory :cl-exec-sandbox))
           (builder (merge-pathnames "scripts/build-helper" system-root)))
      (unless (probe-file builder)
        (error "cl-exec-sandbox helper builder is missing at ~A." builder))
      (uiop:run-program (list "/usr/bin/env" "bash" (namestring builder))
                        :output :interactive
                        :error-output :interactive))))
