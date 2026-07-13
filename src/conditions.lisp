(in-package #:autolith)

;;;; -- Base Conditions --

(define-condition autolith-error (error)
  ((message
    :initarg :message
    :reader autolith-error-message
    :type string
    :documentation "A concise explanation suitable for the terminal."))
  (:documentation "The base condition for expected Autolith failures.")
  (:report (lambda (condition stream)
             (write-string (autolith-error-message condition) stream))))

(define-condition configuration-error (autolith-error)
  ()
  (:documentation "A failure caused by invalid or unavailable configuration."))

(define-condition rollback-requested (autolith-error)
  ((generation-id
    :initarg :generation-id
    :reader rollback-requested-generation-id
    :type non-empty-string
    :documentation "The retained generation selected for the next process."))
  (:documentation "A control condition requesting rollback to a retained generation."))


;;;; -- Authentication and Provider Conditions --

(define-condition authentication-error (autolith-error)
  ()
  (:documentation "The base condition for authentication failures."))

(define-condition credentials-unavailable (authentication-error)
  ((searched-paths
    :initarg :searched-paths
    :reader credentials-unavailable-searched-paths
    :type list
    :documentation "Credential pathnames inspected before the failure."))
  (:documentation "No usable model-provider credentials were found."))

(define-condition token-refresh-failed (authentication-error)
  ((status
    :initarg :status
    :reader token-refresh-failed-status
    :type (option integer)
    :documentation "The HTTP status returned by the OAuth server, if known.")
   (response
    :initarg :response
    :reader token-refresh-failed-response
    :type (option string)
    :documentation "A bounded, non-secret OAuth error response."))
  (:documentation "Refreshing a ChatGPT OAuth access token failed."))

(define-condition provider-error (autolith-error)
  ((status
    :initarg :status
    :reader provider-error-status
    :type (option integer)
    :documentation "The provider HTTP status, if a response was received.")
   (request-id
    :initarg :request-id
    :reader provider-error-request-id
    :type (option string)
    :documentation "The provider request identifier, if supplied.")
   (response
    :initarg :response
    :reader provider-error-response
    :type (option string)
    :documentation "A bounded provider response safe for display."))
  (:documentation "A model-provider request failed."))

(define-condition response-stream-error (provider-error)
  ()
  (:documentation "A provider stream ended without a valid terminal event."))

(define-condition provider-unauthorized (provider-error)
  ()
  (:documentation "A bounded provider attempt was rejected as unauthorized."))


;;;; -- Persistence and Tool Conditions --

(define-condition conversation-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader conversation-error-pathname
    :type pathname
    :documentation "The conversation file being processed.")
   (sequence
    :initarg :sequence
    :reader conversation-error-sequence
    :type (option integer)
    :documentation "The nearest record sequence number, if known."))
  (:documentation "A conversation file is corrupt or cannot be persisted."))

(define-condition conversation-invariant-error (conversation-error)
  ()
  (:documentation "Conversation persistence or replay violated a critical invariant."))

(define-condition tool-error (autolith-error)
  ((tool-name
    :initarg :tool-name
    :reader tool-error-tool-name
    :type string
    :documentation "The canonical dotted tool name."))
  (:documentation "A tool call could not be validated or executed."))

(define-condition worker-error (tool-error)
  ()
  (:documentation "The disposable Lisp worker failed or violated its protocol."))

(define-condition source-mutation-error (tool-error)
  ((pathname
    :initarg :pathname
    :reader source-mutation-error-pathname
    :type (option pathname)
    :documentation "The source file involved in the failed mutation."))
  (:documentation "An active-image or durable source mutation failed."))

(define-condition self-correctable-error (autolith-error)
  ((restart-names
    :initarg :restart-names
    :reader self-correctable-error-restart-names
    :type list
    :documentation "The invokable restart names offered by the failed operation."))
  (:documentation
   "An active-image operation failed while offering selectable restarts."))

(define-condition active-image-corruption (autolith-error)
  ((original-condition
    :initarg :original-condition
    :reader active-image-corruption-original-condition
    :type serious-condition
    :documentation "The mutation failure that initiated restoration.")
   (restoration-condition
    :initarg :restoration-condition
    :reader active-image-corruption-restoration-condition
    :type serious-condition
    :documentation "The second failure that prevented image restoration."))
  (:documentation "A failed mutation could not restore the preceding active definition."))

(define-condition checkpoint-error (autolith-error)
  ((stage
    :initarg :stage
    :reader checkpoint-error-stage
    :type keyword
    :documentation "The checkpoint stage that failed.")
   (pathname
    :initarg :pathname
    :reader checkpoint-error-pathname
    :type (option pathname)
    :documentation "The checkpoint artifact involved in the failure, if any."))
  (:documentation "A generation could not be validated, saved, or published."))
