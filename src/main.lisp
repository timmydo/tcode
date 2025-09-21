(in-package :tcode)


(defun main ()
  "Main entry point for tcode - creates a PTY-based terminal with REPL"
  (let ((pty (make-pty :width 80 :height 24)))
    (unwind-protect
        (handler-case
            (progn
              ;; Open the PTY
              (open-pty pty)

              ;; Print startup message
              (format t "~%tcode - Terminal-based CLI~%")
              (format t "Starting PTY session...~%~%")

              ;; Start the main REPL loop
              (repl-loop pty))

          (error (e)
            (format t "Error in main: ~A~%" e)))

      ;; Always cleanup, even on errors
      (cleanup-pty pty)
      (format t "~%Shutting down...~%"))))

(defun repl-loop (pty)
  "Main REPL loop with visual separators"
  (loop
    (handler-case
        (progn
          ;; Print gray separator line
          (print-separator)

          ;; Print prompt and read input
          (format t "tcode> ")
          (force-output)
          (let ((input (read-line nil nil)))
            (when (or (null input) (string= input "exit"))
              (return))

            ;; Evaluate and print result
            (when (and input (> (length (string-trim " " input)) 0))
              (let ((result (safe-eval-string input)))
                (format t "~A~%" result))))

          ;; Print closing separator
          (print-separator))

      (error (e)
        (format t "Error: ~A~%" e)))))

(defun print-separator ()
  "Print a gray separator line"
  ;; Using ANSI escape codes for gray color
  (format t "~C[90m~A~C[0m~%" #\Escape (make-string 60 :initial-element #\-) #\Escape))

(defun safe-eval-string (input-string)
  "Safely evaluate a Lisp expression from string"
  (handler-case
      (let ((expr (read-from-string input-string)))
        (eval expr))
    (error (e)
      (format nil "Evaluation error: ~A" e))
    (reader-error (e)
      (format nil "Parse error: ~A" e))))
