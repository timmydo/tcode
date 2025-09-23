;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; Programmer: Kevin Rosenberg


(in-package #:cl-user)
(defpackage #:puri-system (:use #:cl #:asdf))
(in-package #:puri-system)


(defsystem puri
  :name "cl-puri"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "GNU Lesser General Public License"
  :description "Portable Universal Resource Indentifier Library"
  :components
  ((:file "src")))

(defmethod perform ((o test-op) (c (eql (find-system 'puri))))
  (oos 'load-op 'puri-tests)
  (oos 'test-op 'puri-tests))
