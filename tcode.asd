(asdf:defsystem "tcode"
  :description "Coding CLI"
  :author "Timmy Douglas <mail@timmydouglas.com>"
  :license  "MPL2"
  :version "0.0.1"
  :depends-on ("jsown" "bordeaux-threads" "cffi" "drakma" "flexi-streams")
  :pathname "src"
  :serial t
  :components (
               (:file "tcode-package")
               (:file "backend")
               (:file "pty")
               (:file "config")
               (:file "logs")
               (:file "eval")
	       ;; entry point
               (:file "main")
))
