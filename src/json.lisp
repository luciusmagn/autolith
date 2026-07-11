(in-package #:frob)

;;;; -- JSON Construction --

(-> json-object (&rest t) json-object)
(defun json-object (&rest key-values)
  "Return a string-keyed JSON object built from alternating KEY-VALUES."
  (unless (evenp (length key-values))
    (error 'configuration-error
           :message "JSON objects require an even number of key and value arguments."))
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on key-values by #'cddr
          do (unless (stringp key)
               (error 'configuration-error
                      :message (format nil "JSON object key ~S is not a string." key)))
             (setf (gethash key object) value))
    object))

(-> json-array (&rest t) vector)
(defun json-array (&rest elements)
  "Return a JSON array containing ELEMENTS."
  (coerce elements 'vector))

(-> json-get (json-object string &optional t) t)
(defun json-get (object key &optional default)
  "Return KEY from JSON OBJECT, or DEFAULT when the key is absent."
  (multiple-value-bind (value present-p)
      (gethash key object)
    (if present-p value default)))

(-> json-encode (json-value) string)
(defun json-encode (value)
  "Encode VALUE as a compact JSON string."
  (with-output-to-string (stream)
    (yason:encode value stream)))

(-> json-decode (string) json-value)
(defun json-decode (source)
  "Decode one JSON value from SOURCE."
  (let ((yason:*parse-json-arrays-as-vectors* t))
    (yason:parse source)))


;;;; -- Bounded Presentation --

(-> bounded-string (t &key (:limit integer)) string)
(defun bounded-string (value &key (limit 8000))
  "Render VALUE as a string no longer than LIMIT characters."
  (let ((text (if (stringp value)
                  value
                  (write-to-string value
                                   :circle t
                                   :level 8
                                   :length 80))))
    (if (<= (length text) limit)
        text
        (format nil "~A~%... ~:D characters omitted"
                (subseq text 0 limit)
                (- (length text) limit)))))
