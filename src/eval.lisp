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
  (let ((result
         (handler-case
             (cond
               ;; Handle /add command for directory management
               ((and (> (length input-string) 4)
                     (string= (subseq input-string 0 4) "/add"))
                (let ((dir-path (string-trim " " (subseq input-string 4))))
                  (if (and (> (length dir-path) 0)
                           (probe-file dir-path)
                           (not (member dir-path (repl-context-context-directories ctx) :test #'string=)))
                      (progn
                        (push dir-path (repl-context-context-directories ctx))
                        (format nil "Added directory: ~A" dir-path))
                      (format nil "Directory not found or already added: ~A" dir-path))))

               ;; Handle /rmdir command for directory management
               ((and (> (length input-string) 6)
                     (string= (subseq input-string 0 6) "/rmdir"))
                (let ((dir-path (string-trim " " (subseq input-string 6))))
                  (if (and (> (length dir-path) 0)
                           (member dir-path (repl-context-context-directories ctx) :test #'string=))
                      (progn
                        (setf (repl-context-context-directories ctx)
                              (remove dir-path (repl-context-context-directories ctx) :test #'string=))
                        (format nil "Removed directory: ~A" dir-path))
                      (format nil "Directory not in context: ~A" dir-path))))

               ;; Handle /clear command to clear history
               ((string= (string-trim " " input-string) "/clear")
                (progn
                  (setf (repl-context-history ctx) '())
                  "History cleared"))

               ;; Default: evaluate as Lisp expression
               (t (handler-case
                      (let ((expr (read-from-string input-string)))
                        (eval expr))
                    (error (e)
                      (format nil "Evaluation error: ~A" e))
                    (reader-error (e)
                      (format nil "Parse error: ~A" e)))))
           (error (e)
             (format nil "Unhandled exception: ~A" e)))))

    ;; Add to history (except for /clear command)
    (unless (string= (string-trim " " input-string) "/clear")
      (push (make-history-item :command input-string :result result) (repl-context-history ctx)))

    ;; Reset context state
    (setf (repl-context-history-index ctx) 0
          (repl-context-scroll-offset ctx) 0
          (repl-context-input-buffer ctx) ""
          (repl-context-cursor-position ctx) 0
          (repl-context-original-input ctx) ""
          (repl-context-state ctx) :normal)

    ;; Update status with directory list after /add or /rmdir commands
    (when (or (and (> (length input-string) 4)
                   (string= (subseq input-string 0 4) "/add"))
              (and (> (length input-string) 6)
                   (string= (subseq input-string 0 6) "/rmdir")))
      (setf (repl-context-status-message ctx)
            (if (> (length (repl-context-context-directories ctx)) 0)
                (format nil "Dirs: ~{~A~^, ~}" (reverse (repl-context-context-directories ctx)))
                "")))

    ;; Update status after /clear command
    (when (string= (string-trim " " input-string) "/clear")
      (setf (repl-context-status-message ctx) "History cleared"))

    result))