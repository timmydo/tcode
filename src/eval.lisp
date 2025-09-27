(in-package :tcode)

(defun safe-eval-string (input-string)
  "Simple evaluation function for testing (no context management)"
  (handler-case
      (let ((expr (read-from-string input-string)))
        (eval expr))
    (error (e)
      (format nil "Evaluation error: ~A" e))
    (reader-error (e)
      (format nil "Parse error: ~A" e))))

(defun submit-command (input-string ctx)
  "Process user command - handle special commands or evaluate Lisp expressions"
  (log-command input-string)
  (let ((result
         (handler-case
             (cond
               ;; Handle /add command for directory management
               ((and (> (length input-string) 4)
                     (string= (subseq input-string 0 4) "/add"))
                (let* ((dir-path (string-trim " " (subseq input-string 4)))
                       (result (if (and (> (length dir-path) 0)
                                        (probe-file dir-path)
                                        (not (member dir-path (repl-context-context-directories ctx) :test #'string=)))
                                   (progn
                                     (push dir-path (repl-context-context-directories ctx))
                                     (format nil "Added directory: ~A" dir-path))
                                   (format nil "Directory not found or already added: ~A" dir-path))))
                  ;; Add to history
                  (bt:with-lock-held ((repl-context-mutex ctx))
                    (push (make-history-item :command input-string :result result) (repl-context-history ctx)))
                  ;; Add to global history for web UI
                  (when (boundp '*global-history*)
                    (add-to-global-history input-string result))
                  result))

               ;; Handle /rmdir command for directory management
               ((and (> (length input-string) 6)
                     (string= (subseq input-string 0 6) "/rmdir"))
                (let* ((dir-path (string-trim " " (subseq input-string 6)))
                       (result (if (and (> (length dir-path) 0)
                                        (member dir-path (repl-context-context-directories ctx) :test #'string=))
                                   (progn
                                     (setf (repl-context-context-directories ctx)
                                           (remove dir-path (repl-context-context-directories ctx) :test #'string=))
                                     (format nil "Removed directory: ~A" dir-path))
                                   (format nil "Directory not in context: ~A" dir-path))))
                  ;; Add to history
                  (bt:with-lock-held ((repl-context-mutex ctx))
                    (push (make-history-item :command input-string :result result) (repl-context-history ctx)))
                  ;; Add to global history for web UI
                  (when (boundp '*global-history*)
                    (add-to-global-history input-string result))
                  result))

               ;; Handle /clear command to clear history
               ((string= (string-trim " " input-string) "/clear")
                (progn
                  (bt:with-lock-held ((repl-context-mutex ctx))
                    (setf (repl-context-history ctx) '()))
                  "History cleared"))

               ;; Handle /config command to initialize configuration file
               ((or (string= (string-trim " " input-string) "/config")
                    (string= (string-trim " " input-string) "/config -f"))
                (let* ((force (string= (string-trim " " input-string) "/config -f"))
                       (result (initialize-config-file force)))
                  ;; Add to history
                  (bt:with-lock-held ((repl-context-mutex ctx))
                    (push (make-history-item :command input-string :result result) (repl-context-history ctx)))
                  result))

               ;; Handle /lorem command to add test history items with lorem ipsum
               ((and (>= (length input-string) 6)
                     (string= (subseq input-string 0 6) "/lorem"))
                (let* ((param-str (string-trim " " (subseq input-string 6)))
                       (count (if (> (length param-str) 0)
                                  (handler-case
                                      (parse-integer param-str)
                                    (error () 1))
                                  1))
                       (lorem-paragraph "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                       (lorem-command (format nil "Generate ~A paragraph~:P of lorem ipsum" count))
                       (lorem-response (format nil "~{~A~^~%~%~}" (make-list count :initial-element lorem-paragraph)))
                       (history-item (make-history-item :command lorem-command :result lorem-response)))
                  ;; Add lorem history item
                  (bt:with-lock-held ((repl-context-mutex ctx))
                    (push history-item (repl-context-history ctx)))
                  (format nil "Added lorem ipsum test history item with ~A paragraph~:P" count)))

               ;; Default: dispatch to backend only
               (t (if (and (boundp '*tcode-backend*) *tcode-backend*)
                      ;; Dispatch to backend
                      (dispatch-command *tcode-backend* input-string ctx)
                      ;; No backend available - add to history
                      (let ((result "No backend configured. Use /config to set up a backend."))
                        (bt:with-lock-held ((repl-context-mutex ctx))
                          (push (make-history-item :command input-string :result result) (repl-context-history ctx)))
                        result))))
           (error (e)
             (log-error "Command evaluation error: ~A" e)
             (let ((result (format nil "Unhandled exception: ~A" e)))
               (bt:with-lock-held ((repl-context-mutex ctx))
                 (push (make-history-item :command input-string :result result) (repl-context-history ctx)))
               result)))))

    ;; Log the result
    (log-result result)


    ;; Reset context state
    (setf (repl-context-state ctx) :normal)

    ;; Update status with directory list after /add or /rmdir commands
    (when (or (and (> (length input-string) 4)
                   (string= (subseq input-string 0 4) "/add"))
              (and (> (length input-string) 6)
                   (string= (subseq input-string 0 6) "/rmdir")))
)


    result))