(defpackage #:autolith
  (:use #:cl)
  (:import-from #:alexandria
                #:define-constant)
  (:import-from #:cl-base64
                #:base64-string-to-string)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:make-recursive-lock
                #:make-thread
                #:with-recursive-lock-held
                #:with-lock-held)
  (:import-from #:dexador.error
                #:http-request-failed
                #:response-body
                #:response-headers
                #:response-status)
  (:import-from #:quri
                #:url-encode-params)
  (:import-from #:serapeum
                #:->)
  (:import-from #:yason
                #:false)
  (:export #:main
           #:run-tests
           #:worker-main))

(in-package #:autolith)
