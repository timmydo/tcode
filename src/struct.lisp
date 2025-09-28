;;;; struct.lisp

(in-package :tcode)

(defstruct history-item
  (command "")
  (result nil)
  (usage nil))

(defstruct repl-context
  (history '())
  (state :normal)  ; :normal, :waiting-for-command
  (context-directories (list (namestring (truename "."))))
  (current-thread nil)  ; Active background thread for HTTP requests
  (mutex nil)   ; Mutex for thread-safe context updates
  (current-stream nil)) ; Stream to close for cancellation