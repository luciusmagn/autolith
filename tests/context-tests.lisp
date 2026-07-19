(in-package #:autolith)

;;;; -- Request-Local Context Tests --

(defvar *context-test-next-request-p* t
  "Whether the next-request test contributor is currently active.")

(-> context-tests--stacked (request-context) list)
(defun context-tests--stacked (context)
  "Return overlapping contributions used to exercise request resolution."
  (declare (ignore context))
  (list
   (make-context-contribution
    :identifier "generic"
    :instruction "Use the generic path."
    :priority 5)
   (make-context-contribution
    :identifier "specific"
    :instruction "Use the specific path."
    :priority 20
    :supersedes '("generic"))
   (make-context-contribution
    :identifier "stacked"
    :instruction "Also preserve the surrounding form."
    :priority 10)
   (make-context-contribution
    :identifier "duplicate-low"
    :instruction "Use the weaker duplicate."
    :priority 1
    :deduplication-key "duplicate")
   (make-context-contribution
    :identifier "duplicate-high"
    :instruction "Use the stronger duplicate."
    :priority 2
    :deduplication-key "duplicate")
   (make-context-contribution
    :identifier "conflict-low"
    :instruction "Choose the weaker conflict."
    :priority 3
    :conflict-group "mode")
   (make-context-contribution
    :identifier "conflict-high"
    :instruction "Choose the stronger conflict."
    :priority 4
    :conflict-group "mode")))

(-> context-tests--next-request (request-context) (option context-contribution))
(defun context-tests--next-request (context)
  "Return one edge-triggered contribution while its fixture is active."
  (declare (ignore context))
  (when *context-test-next-request-p*
    (make-context-contribution
     :identifier "next"
     :instruction "This appears on the next completed request only."
     :lifetime ':next-request)))

(-> context-tests--failure (request-context) null)
(defun context-tests--failure (context)
  "Signal the deterministic contributor failure used by diagnostics tests."
  (declare (ignore context))
  (error "broken contributor"))

(-> context-tests--mandatory (request-context) list)
(defun context-tests--mandatory (context)
  "Return mandatory and advisory values used to exercise the budget."
  (declare (ignore context))
  (list
   (make-context-contribution
    :identifier "mandatory"
    :instruction "Always retain this requirement."
    :class ':mandatory
    :priority -100)
   (make-context-contribution
    :identifier "high"
    :instruction "Keep this compact high-priority advice."
    :priority 100)
   (make-context-contribution
    :identifier "low"
    :instruction (make-string 200 :initial-element #\x)
    :priority 1)))

(-> context-tests--defining-form () null)
(defun context-tests--defining-form ()
  "Test durable contributor definition installation and exact undo."
  (let* ((name 'context-tests--defined)
         (source
           "(define-context-contributor context-tests--defined (context) (declare (ignore context)) (make-context-contribution :identifier \"defined-advice\" :instruction \"Defined advice.\"))")
         (definition (self-read-form source))
         (identifier (context--definition-identifier name)))
    (when (fboundp name)
      (fmakunbound name))
    (unregister-context-contributor identifier)
    (let ((undo (self--definition-undo-action
                 definition nil (find-package '#:autolith))))
      (unwind-protect
           (progn
             (test-assert (definition-form-p definition)
                          "self.redefine accepts context contributor definitions")
             (self--install-definition definition source)
             (test-assert
              (and (fboundp name)
                   (context--registration-find identifier))
              "installing a contributor definition registers its function"))
        (funcall undo)
        (remhash (definition-key definition) *exploratory-definitions*)))
    (test-assert
     (and (not (fboundp name))
          (null (context--registration-find identifier)))
     "discard restores both contributor function and registration state"))
  nil)

(-> test-request-local-context () null)
(defun test-request-local-context ()
  "Test contributor stacking, lifecycle, budgeting, failures, and projection."
  (context-tests--defining-form)
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (conversation (conversation-create configuration
                                            :identifier "context-test"))
         (*context-contributors* nil)
         (*context-next-request-delivered* (make-hash-table :test #'equal))
         (*context-last-delivery* nil)
         (*context-test-next-request-p* t))
    (unwind-protect
         (progn
           (conversation-append-user-message conversation "inspect this request")
           (register-context-contributor "stacked" 'context-tests--stacked
                                         :source ':built-in)
           (let* ((delivery
                    (context-resolve-request configuration conversation #()))
                  (rendered (context-delivery-rendered delivery)))
             (test-assert
              (and (search "specific path" rendered)
                   (search "preserve the surrounding form" rendered))
              "compatible context contributions stack in one request")
             (test-assert (not (search "generic path" rendered))
                          "explicit supersession removes generic advice")
             (test-assert
              (and (search "stronger duplicate" rendered)
                   (not (search "weaker duplicate" rendered)))
              "deduplication retains the highest-priority equivalent advice")
             (test-assert
              (and (search "stronger conflict" rendered)
                   (not (search "weaker conflict" rendered)))
              "explicit conflict groups select one strongest contribution"))
           (register-context-contributor "next" 'context-tests--next-request)
           (let ((first (context-resolve-request configuration conversation #())))
             (test-assert (search "next completed request"
                                  (context-delivery-rendered first))
                          "a next-request contribution is initially active")
             (context-delivery-complete first))
           (let ((second (context-resolve-request configuration conversation #())))
             (test-assert
              (not (search "next completed request"
                           (or (context-delivery-rendered second) "")))
              "a completed response consumes next-request advice"))
           (setf *context-test-next-request-p* nil)
           (context-resolve-request configuration conversation #())
           (setf *context-test-next-request-p* t)
           (test-assert
            (search "next completed request"
                    (context-delivery-rendered
                     (context-resolve-request configuration conversation #())))
            "a later activation may deliver the same next-request advice again")
           (setf *context-contributors* nil
                 *context-advice-token-budget* 20)
           (register-context-contributor "budget" 'context-tests--mandatory)
           (let ((delivery
                   (context-resolve-request configuration conversation #())))
             (test-assert
              (find "mandatory" (context-delivery-contributions delivery)
                    :test #'string=
                    :key #'context-contribution-identifier)
              "mandatory context never competes for the advice budget")
             (test-assert (context-delivery-omitted delivery)
                          "advice beyond the token budget is omitted visibly"))
           (setf *context-contributors* nil)
           (register-context-contributor "failure" 'context-tests--failure)
           (let ((delivery
                   (context-resolve-request configuration conversation #())))
             (test-assert (string= (first (first
                                           (context-delivery-failures delivery)))
                                   "failure")
                          "contributor failures degrade to diagnostics")
             (test-assert (search "broken contributor" (context-status))
                          "/context diagnostics expose contributor failures"))
           (setf *context-contributors* nil
                 *context-advice-token-budget* 1500)
           (register-context-contributor "next" 'context-tests--next-request)
           (let* ((provider (provider-create configuration))
                  (before (copy-list
                           (conversation-input-items conversation)))
                  (request (provider-request-object provider conversation #()))
                  (input (json-get request "input"))
                  (message (aref input (1- (length input)))))
             (test-assert (= (length input) 4)
                          "resolved context adds one provider-only message")
             (test-assert
              (search "Temporary context"
                      (json-get (aref (json-get message "content") 0) "text"))
              "request-local context is rendered after durable input")
             (test-assert
              (equal before (conversation-input-items conversation))
              "context request assembly never mutates durable conversation input"))
           (let ((request
                   (make-instance 'request-context
                                  :configuration configuration
                                  :conversation conversation
                                  :tool-namespaces #()
                                  :turn-budget-state :normal)))
             (test-assert
              (string= (request-context-latest-user-text request)
                       "inspect this request")
              "contributors receive the latest durable user text")))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)
