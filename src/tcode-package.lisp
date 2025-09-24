;;;; package.lisp

(cl:in-package #:common-lisp-user)

(defpackage #:tcode
  (:use :cl)
  (:local-nicknames (:bt :bordeaux-threads))
  (:export #:main
           #:backend-connection
           #:openrouter-connection
           #:make-openrouter-connection
           #:dispatch-command))
