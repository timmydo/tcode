(in-package :tcode)


(defun main ()
  "Main entry point for tcode - creates a PTY-based terminal with curses-like interface"
  (let ((pty (make-pty :width 80 :height 24)))
    (unwind-protect
        (handler-case
            (progn
              ;; Open the PTY
              (open-pty pty)

              ;; Enable raw mode for curses-like interface
              (enable-raw-mode 0)

              ;; Initialize screen
              (clear-screen)
              (set-color 2) ; Green
              (format t "tcode - Terminal-based CLI")
              (reset-color)
              (move-cursor 2 1)
              (format t "PTY session active. Type 'exit' or Ctrl+C to quit.")
              (move-cursor 4 1)

              ;; Start the main REPL loop
              (repl-loop pty))

          (error (e)
            (move-cursor 20 1)
            (set-color 1) ; Red
            (format t "Error in main: ~A" e)
            (reset-color)))

      ;; Always cleanup, even on errors
      (disable-raw-mode 0)
      (cleanup-pty pty)
      (move-cursor 22 1)
      (format t "Shutting down...")
      (move-cursor 23 1))))

(defun repl-loop (pty)
  "Main REPL loop with curses-like interface"
  (let ((input-buffer "")
        (cursor-row 5)
        (prompt-col 8)
        (history '())
        (history-index 0))

    (loop
      (handler-case
          (progn
            ;; Display prompt and current input
            (display-prompt-line cursor-row input-buffer prompt-col)

            ;; Read character
            (let ((char (read-char-raw)))
              (cond
                ;; Ctrl+C or ESC - exit
                ((or (char= char (code-char 3)) (char= char (code-char 27)))
                 (return))

                ;; Enter - process input
                ((or (char= char #\Return) (char= char #\Newline))
                 (let ((trimmed-input (string-trim " " input-buffer)))
                   (when (string= trimmed-input "exit")
                     (return))

                   (when (> (length trimmed-input) 0)
                     ;; Add to history
                     (push trimmed-input history)
                     (setf history-index 0)

                     ;; Evaluate and display result
                     (let ((result (safe-eval-string trimmed-input)))
                       (display-result cursor-row result)))

                   ;; Move to next line and reset
                   (incf cursor-row 3)
                   (when (> cursor-row 20)
                     (setf cursor-row 5))
                   (setf input-buffer "")))

                ;; Backspace - remove character
                ((or (char= char #\Backspace) (char= char #\Del))
                 (when (> (length input-buffer) 0)
                   (setf input-buffer (subseq input-buffer 0 (1- (length input-buffer))))))

                ;; Ctrl+P/Ctrl+N for history (up/down)
                ((char= char (code-char 16)) ; Ctrl+P - Previous (up) in history
                 (when (and history (< history-index (length history)))
                   (incf history-index)
                   (setf input-buffer (nth (1- history-index) history))))

                ((char= char (code-char 14)) ; Ctrl+N - Next (down) in history
                 (when (and history (> history-index 1))
                   (decf history-index)
                   (setf input-buffer (nth (1- history-index) history))))

                ;; Ctrl+L to clear screen
                ((char= char (code-char 12)) ; Ctrl+L
                 (clear-screen)
                 (move-cursor 1 1)
                 (set-color 2)
                 (format t "tcode - Terminal-based CLI")
                 (reset-color)
                 (move-cursor 2 1)
                 (format t "PTY session active. Type 'exit' or Ctrl+C to quit."))

                ;; Regular characters - add to buffer
                ((and (graphic-char-p char) (< (length input-buffer) 200))
                 (setf input-buffer (concatenate 'string input-buffer (string char)))))))

        (error (e)
          (move-cursor 21 1)
          (set-color 1) ; Red
          (format t "Error: ~A" e)
          (reset-color)
          (move-cursor cursor-row 1))))))

(defun display-prompt-line (row input-buffer prompt-col)
  "Display the command prompt with current input"
  (move-cursor row 1)
  (clear-line)
  (set-color 6) ; Cyan
  (format t "tcode> ")
  (reset-color)
  (format t "~A" input-buffer)
  ;; Show cursor
  (format t "_")
  (force-output))

(defun display-result (row result)
  "Display the evaluation result"
  (move-cursor (1+ row) 1)
  (clear-line)
  (set-color 3) ; Yellow
  (format t "=> ")
  (reset-color)
  (format t "~A" result)
  (force-output))

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
