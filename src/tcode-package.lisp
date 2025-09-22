;;;; package.lisp

(cl:in-package #:common-lisp-user)

(defpackage #:tcode
  (:use :cl)
  (:export #:main
           #:backend-connection
           #:openrouter-connection
           #:make-openrouter-connection))
