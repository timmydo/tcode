;;;; struct.lisp

(in-package :tcode)

(defstruct history-item
  (command "")
  (result nil)
  (usage nil))

(defstruct repl-context
  (history '())
  (state :normal)  ; :normal, :waiting-for-command, :waiting-for-tool-approval
  (context-directories (list (namestring (truename "."))))
  (current-thread nil)  ; Active background thread for HTTP requests
  (mutex nil)   ; Mutex for thread-safe context updates
  (current-stream nil)  ; Stream to close for cancellation
  (pending-tool-calls nil)  ; Tool calls awaiting user approval
  (pending-accumulated-response nil)  ; Response text before tool calls
  (pending-backend nil)  ; Backend for continuing after approval
  (pending-input-string nil)  ; Original input string
  (pending-history-item nil)  ; History item for continuation
  (auto-approve-tools nil))  ; Auto-approve all tool calls