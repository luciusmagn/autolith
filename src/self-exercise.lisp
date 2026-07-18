(in-package #:autolith)

;;;; -- Focused Mutation Exercises --

(-> self-exercise--mutation-record (configuration (option string)) list)
(defun self-exercise--mutation-record (configuration identifier)
  "Return the effective mutation selected by IDENTIFIER for an exercise."
  (let* ((effective (image-commit-effective-diff-records configuration))
         (record
           (if identifier
               (find identifier effective
                     :key (lambda (candidate)
                            (getf (rest candidate) :id))
                     :test #'string=)
               (first (last effective)))))
    (unless record
      (error 'source-mutation-error
             :message
             (if identifier
                 (format nil "No effective pending mutation is named ~A."
                         identifier)
                 "The active image has no effective pending mutation to exercise.")
             :tool-name "self.exercise"
             :pathname (configuration-journal-path configuration)))
    record))

(-> self-exercise-mutation (configuration string (option string)) string)
(defun self-exercise-mutation (configuration source mutation-identifier)
  "Run SOURCE against one pending mutation and append pass or fail evidence."
  (with-live-mutation
    (let* ((record
             (self-exercise--mutation-record configuration mutation-identifier))
           (mutation-identifier (getf (rest record) :id))
           (exercise-identifier (make-identifier)))
      (mutation-journal-append
       configuration
       (list :mutation
             :kind :exercise
             :id exercise-identifier
             :lineage *active-image-lineage-identifier*
             :mutation mutation-identifier
             :proposed source
             :result :pending))
      (handler-case
          (multiple-value-bind (result-values output)
              (self-capture-evaluation
               (lambda ()
                 (eval (self-read-form source))))
            (mutation-journal-append
             configuration
             (list :mutation
                   :kind :exercise
                   :id exercise-identifier
                   :lineage *active-image-lineage-identifier*
                   :mutation mutation-identifier
                   :proposed source
                   :result :passed
                   :values result-values
                   :output (bounded-string output :limit 2000)))
            (format nil "Exercise ~A passed for mutation ~A.~2%~A"
                    exercise-identifier
                    mutation-identifier
                    (self-evaluation-result result-values output)))
        (error (condition)
          (mutation-journal-append
           configuration
           (list :mutation
                 :kind :exercise
                 :id exercise-identifier
                 :lineage *active-image-lineage-identifier*
                 :mutation mutation-identifier
                 :proposed source
                 :result :failed
                 :condition (bounded-string condition :limit 2000)))
          (error condition))))))

(defmethod tool-execute ((tool self-exercise-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Run and journal one focused exercise against an effective live mutation."
  (declare (ignore tool))
  (tool-success
   (self-exercise-mutation
    (tool-context-configuration context)
    (tool-argument arguments "form" :required t)
    (tool-argument arguments "mutation"))))
