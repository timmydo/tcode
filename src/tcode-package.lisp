;;;; package.lisp

(cl:in-package #:common-lisp-user)

(defpackage #:tcode
  (:use :cl)
  (:local-nicknames (:bt :bordeaux-threads))
  (:export #:main
           #:backend-connection
           #:openrouter-connection
           #:make-openrouter-connection
           #:dispatch-command
           #:initialize-logging
           #:cleanup-logging
           #:log-debug
           #:log-info
           #:log-warn
           #:log-error
           #:log-command
           #:log-result
           #:make-lock-with-logging
           #:make-thread-with-logging
           #:join-thread-with-logging
           #:thread-name))

(in-package :tcode)

(defstruct history-item
  (command "")
  (result nil))

(defstruct repl-context
  (history '())
  (state :normal)  ; :normal, :waiting-for-command
  (context-directories (list (namestring (truename "."))))
  (current-thread nil)  ; Active background thread for HTTP requests
  (mutex nil)   ; Mutex for thread-safe context updates
  (current-stream nil)) ; Stream to close for cancellation
