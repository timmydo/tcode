;;;; package.lisp

(cl:in-package #:common-lisp-user)

(defpackage #:tcode
  (:use :cl)
  (:local-nicknames (:bt :bordeaux-threads))
  (:export ;; Entry points
           #:main
           #:client
           #:server
           ;; Backend connections
           #:backend-connection
           #:openrouter-connection
           #:make-openrouter-connection
           #:dispatch-command
           ;; Tool backend
           #:tool-backend
           #:make-tool-backend
           #:call-tool-via-backend
           ;; JSONRPC server
           #:start-jsonrpc-server
           #:stop-jsonrpc-server
           ;; Logging
           #:initialize-logging
           #:cleanup-logging
           #:log-debug
           #:log-info
           #:log-warn
           #:log-error
           #:log-command
           #:log-result
           ;; Threading
           #:make-lock-with-logging
           #:make-thread-with-logging
           #:join-thread-with-logging
           #:thread-name
           ;; Tool API
           #:register-tool
           #:get-registered-tools
           #:get-tool
           #:call-tool
           #:tools-to-openrouter-format
           ;; Shell tool
           #:start-shell
           #:execute-in-shell))

