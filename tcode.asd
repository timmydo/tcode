(asdf:defsystem "tcode"
  :description "Coding CLI"
  :author "Timmy Douglas <mail@timmydouglas.com>"
  :license  "MPL2"
  :version "0.0.1"
  :depends-on ("jsown" "bordeaux-threads" "cffi" "drakma" "flexi-streams" "webview" "usocket" "babel")
  :pathname "src"
  :serial t
  :components (
               (:file "tcode-package")
               (:file "struct")
               (:file "logs")
               (:file "locks")
               (:file "tool")
               (:file "tools/shell")
               (:file "tools/read")
               (:file "tools/grep")
               (:file "jsonrpc-server")
               (:file "backend")
               (:file "config")
               (:file "dom")
               (:file "eval")
               (:file "web-ui")
	       ;; entry point
               (:file "main")
))
