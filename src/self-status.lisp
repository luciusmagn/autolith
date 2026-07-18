(in-package #:autolith)

;;;; -- Active Image Status --

(-> self-status--identifier ((option string)) string)
(defun self-status--identifier (identifier)
  "Render optional IDENTIFIER as a private image state name."
  (or identifier "base"))

(-> self-status--generation ((option generation)) string)
(defun self-status--generation (generation)
  "Render GENERATION's identity and compatibility, or NONE."
  (if generation
      (format nil "~A (~A)"
              (generation-identifier generation)
              (if (generation-compatible-p generation)
                  "compatible"
                  "incompatible"))
      "none"))

(-> self-status-render (configuration) string)
(defun self-status-render (configuration)
  "Return a concise summary of live mutation and retained recovery state."
  (multiple-value-bind (selected-identifier selected-history-commit)
      (image-commit--pointer-state configuration)
    (let* ((pending (image-commit-pending-records configuration))
           (effective (image-commit-effective-pending-records configuration))
           (generations (generation-list configuration))
           (newest-generation (first generations))
           (selected-generation (generation-selected configuration))
           (selection-current-p
             (and (equal *active-image-commit-identifier* selected-identifier)
                  (equal *active-image-history-commit*
                         selected-history-commit))))
      (with-output-to-string (stream)
        (format stream "private image~%  running   ~A~%  selected  ~A~%  synchronized  ~:[no~;yes~]~%"
                (self-status--identifier *active-image-commit-identifier*)
                (self-status--identifier selected-identifier)
                selection-current-p)
        (format stream "journal~%  lineage    ~A~%  pending     ~D installed, ~D effective~%"
                (or *active-image-lineage-identifier* "uninitialized")
                (length pending)
                (length effective))
        (dolist (record effective)
          (let ((properties (rest record)))
            (format stream "  ~A  ~A~%"
                    (getf properties :kind)
                    (getf properties :target))))
        (format stream "checkpoint~%  publishing  ~:[no~;yes~]~%"
                *checkpoint-in-progress-p*)
        (format stream "generations~%  retained  ~D~%  newest    ~A~%  selected  ~A"
                (length generations)
                (self-status--generation newest-generation)
                (self-status--generation selected-generation))))))

(defmethod tool-execute ((tool self-status-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Summarize CONTEXT's active image and retained recovery state."
  (declare (ignore tool arguments))
  (tool-success
   (self-status-render (tool-context-configuration context))))
