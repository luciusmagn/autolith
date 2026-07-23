(in-package #:autolith)

;;;; -- Conversation Identifier Format --

(defparameter *conversation-identifier-alphabet*
  "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  "The Bitcoin Base58 alphabet used by conversation identifiers.")

(defparameter *conversation-identifier-suffix-length* 6
  "The fixed Base58 width of the scrambled timestamp suffix.")

(defvar *conversation-identifier-random-index-function*
  (lambda (limit)
    (random limit (sb-ext:seed-random-state t)))
  "Return an operating-system-seeded index below LIMIT for the first probe.")

(defvar *conversation-identifier-reservations*
  (make-hash-table :test #'equal)
  "Process-local reservations for not-yet-persisted conversation pathnames.")

(defvar *conversation-identifier-lock* (make-lock "conversation identifiers")
  "Serialize conversation identifier allocation and migration in this process.")

(-> conversation-identifier-base () integer)
(defun conversation-identifier-base ()
  "Return the live radix derived from the conversation identifier alphabet."
  (length *conversation-identifier-alphabet*))

(-> conversation-identifier-stored-length () integer)
(defun conversation-identifier-stored-length ()
  "Return the seed plus suffix width of a stored conversation identifier."
  (1+ *conversation-identifier-suffix-length*))

(-> conversation-identifier-modulus () integer)
(defun conversation-identifier-modulus ()
  "Return the fixed modulus used by the specified unsigned 32-bit scramble."
  #x100000000)

(-> conversation-identifier-mask () integer)
(defun conversation-identifier-mask ()
  "Return the fixed mask used by the specified unsigned 32-bit mixer."
  #xffffffff)

(-> conversation-identifier--base58-index (character) (option integer))
(defun conversation-identifier--base58-index (character)
  "Return CHARACTER's zero-based Bitcoin Base58 index, or NIL."
  (position character *conversation-identifier-alphabet* :test #'char=))

(-> conversation-identifier-stored-p (t) boolean)
(defun conversation-identifier-stored-p (value)
  "Return true when VALUE is one canonical stored conversation identifier."
  (not
   (null
    (and (stringp value)
         (= (length value) (conversation-identifier-stored-length))
         (every #'conversation-identifier--base58-index value)))))

(-> conversation-identifier-normalize (t) string)
(defun conversation-identifier-normalize (value)
  "Return VALUE as a canonical stored identifier, accepting its display form."
  (let ((normalized
          (cond
            ((conversation-identifier-stored-p value)
             value)
            ((and (stringp value)
                  (= (length value)
                     (1+ (conversation-identifier-stored-length)))
                  (char= (char value 1) #\-))
             (concatenate 'string (subseq value 0 1) (subseq value 2)))
            (t
             nil))))
    (unless (conversation-identifier-stored-p normalized)
      (error 'conversation-identifier-error
             :message
             "A conversation identifier must contain seven case-sensitive Bitcoin Base58 characters, with an optional hyphen after the first."
             :value value))
    normalized))

(-> conversation-identifier-display (string) string)
(defun conversation-identifier-display (identifier)
  "Return IDENTIFIER with its visual hyphen, retaining legacy values verbatim."
  (handler-case
      (let ((stored (conversation-identifier-normalize identifier)))
        (format nil "~A-~A" (subseq stored 0 1) (subseq stored 1)))
    (conversation-identifier-error ()
      identifier)))

(-> conversation-identifier-path-fragment (string) (option string))
(defun conversation-identifier-path-fragment (identifier)
  "Return canonical IDENTIFIER unchanged for case-sensitive private paths."
  (and (conversation-identifier-stored-p identifier) identifier))

(-> conversation-identifier--mix32 (integer) integer)
(defun conversation-identifier--mix32 (value)
  "Return the specified portable unsigned 32-bit mix of VALUE.

The operation is the MurmurHash3 32-bit finalizer. Every intermediate product
is reduced modulo 2^32, so its result does not depend on fixnum width."
  (let ((mixed (logand value (conversation-identifier-mask))))
    (setf mixed (logand (logxor mixed (ash mixed -16))
                        (conversation-identifier-mask))
          mixed (logand (* mixed #x85ebca6b)
                        (conversation-identifier-mask))
          mixed (logand (logxor mixed (ash mixed -13))
                        (conversation-identifier-mask))
          mixed (logand (* mixed #xc2b2ae35)
                        (conversation-identifier-mask))
          mixed (logand (logxor mixed (ash mixed -16))
                        (conversation-identifier-mask)))
    mixed))

(-> conversation-identifier--seed-parameters (integer) (values integer integer))
(defun conversation-identifier--seed-parameters (seed-index)
  "Return the odd multiplier and offset specified for Base58 SEED-INDEX.

The independent hexadecimal domain constants and CONVERSATION-IDENTIFIER--MIX32
fully specify the portable derivation."
  (unless (and (integerp seed-index)
               (<= 0 seed-index)
               (< seed-index (conversation-identifier-base)))
    (error 'conversation-identifier-error
           :message
           (format nil
                   "A conversation identifier seed index must be from 0 through ~D."
                   (1- (conversation-identifier-base)))
           :value seed-index))
  (values
   (logior 1
           (conversation-identifier--mix32
            (logxor seed-index #x73656564)))
   (conversation-identifier--mix32
    (logxor seed-index #x6f666673))))

(-> conversation-identifier--encode-suffix (integer) string)
(defun conversation-identifier--encode-suffix (value)
  "Encode unsigned 32-bit VALUE as exactly six Bitcoin Base58 characters."
  (unless (and (integerp value)
               (<= 0 value (conversation-identifier-mask)))
    (error 'conversation-identifier-error
           :message "A conversation identifier suffix value must be unsigned 32-bit."
           :value value))
  (let ((encoded
          (make-string *conversation-identifier-suffix-length*
                       :initial-element
                       (char *conversation-identifier-alphabet* 0)))
        (remaining value))
    (loop for position downfrom (1- *conversation-identifier-suffix-length*) to 0
          do (multiple-value-bind (quotient remainder)
                 (floor remaining (conversation-identifier-base))
               (setf (char encoded position)
                     (char *conversation-identifier-alphabet* remainder)
                     remaining quotient)))
    encoded))

(-> conversation-identifier-from-seed (timestamp integer) string)
(defun conversation-identifier-from-seed (timestamp seed-index)
  "Return the stored identifier for Universal TIMESTAMP and SEED-INDEX."
  (multiple-value-bind (multiplier offset)
      (conversation-identifier--seed-parameters seed-index)
    (let* ((seconds (mod timestamp (conversation-identifier-modulus)))
           (scrambled
             (mod (+ (* multiplier seconds) offset)
                  (conversation-identifier-modulus))))
      (concatenate
       'string
       (string (char *conversation-identifier-alphabet* seed-index))
       (conversation-identifier--encode-suffix scrambled)))))

(-> conversation-identifier--reserved-p (pathname string) boolean)
(defun conversation-identifier--reserved-p (storage-root identifier)
  "Return true when IDENTIFIER is occupied or reserved beneath STORAGE-ROOT."
  (let ((pathname
          (merge-pathnames
           (make-pathname :name identifier :type "sexp")
           storage-root)))
    (or (probe-file pathname)
        (gethash (namestring pathname)
                 *conversation-identifier-reservations*)
        nil)))

(-> conversation-identifier-generate
    (pathname &key (:timestamp timestamp) (:reserved-identifiers list))
    string)
(defun conversation-identifier-generate
    (storage-root &key (timestamp (get-universal-time)) reserved-identifiers)
  "Allocate one stored identifier for TIMESTAMP beneath STORAGE-ROOT.

The first seed is random. Collisions probe every remaining seed once before a
structured exhaustion condition is signaled. RESERVED-IDENTIFIERS extends the
occupied set for migration planning."
  (let ((root (uiop:ensure-directory-pathname storage-root)))
    (with-lock-held (*conversation-identifier-lock*)
      (let ((first-seed
              (funcall *conversation-identifier-random-index-function*
                       (conversation-identifier-base))))
        (unless (and (integerp first-seed)
                     (<= 0 first-seed)
                     (< first-seed (conversation-identifier-base)))
          (error 'conversation-identifier-error
                 :message "The conversation identifier entropy source returned an invalid seed index."
                 :value first-seed))
        (loop for probe below (conversation-identifier-base)
              for seed-index = (mod (+ first-seed probe)
                                    (conversation-identifier-base))
              for identifier = (conversation-identifier-from-seed
                                timestamp seed-index)
              unless (or (member identifier reserved-identifiers :test #'string=)
                         (conversation-identifier--reserved-p root identifier))
                do (let ((pathname
                           (merge-pathnames
                            (make-pathname :name identifier :type "sexp")
                            root)))
                     (setf (gethash (namestring pathname)
                                    *conversation-identifier-reservations*)
                           t)
                     (return identifier))
              finally
                 (error 'conversation-identifier-space-exhausted
                        :message
                        (format nil "All conversation identifier seeds are occupied for Universal Time ~D."
                                timestamp)
                        :pathname root
                        :sequence nil
                        :timestamp timestamp))))))
