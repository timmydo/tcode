(in-package :tcode)


(defun main ()
  "Main entry point for tcode - creates a PTY-based terminal with curses-like interface"
  (multiple-value-bind (rows cols) (get-terminal-size)
    (let ((pty (make-pty :width cols :height rows)))
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
              (repl-loop pty rows cols))

          (error (e)
            (move-cursor (- rows 4) 1)
            (set-color 1) ; Red
            (format t "Error in main: ~A" e)
            (reset-color)))

      ;; Always cleanup, even on errors
      (disable-raw-mode 0)
      (cleanup-pty pty)
      (move-cursor (- rows 2) 1)
      (format t "Shutting down...")
      (move-cursor (1- rows) 1)))))

(defun repl-loop (pty rows cols)
  "Main REPL loop with fixed prompt at bottom and scrollable history above"
  (declare (ignore pty))  ; PTY not used in current implementation
  (let ((input-buffer "")
        (history '())
        (results '())
        (history-index 0)
        (content-height (- rows 3))  ; Reserve 3 lines for prompt area
        (scroll-offset 0))

    ;; Initial screen setup
    (clear-screen)
    (draw-bottom-interface rows cols input-buffer)

    (loop
      (handler-case
          (progn
            ;; Draw the content area with history and results
            (draw-content-area content-height history results scroll-offset)

            ;; Update prompt area
            (draw-bottom-interface rows cols input-buffer)

            ;; Read character
            (let ((char (read-char-raw)))
              (cond
                ;; Ctrl+C - exit
                ((char= char (code-char 3))
                 (return))

                ;; ESC sequence handling
                ((char= char (code-char 27))
                 (let ((escape-sequence (read-escape-sequence)))
                   (cond
                     ;; Page Up key
                     ((string= escape-sequence "[5~")
                      (let ((total-lines (length history)))
                        (when (< scroll-offset (max 0 (- total-lines content-height)))
                          (incf scroll-offset))))

                     ;; Page Down key
                     ((string= escape-sequence "[6~")
                      (when (> scroll-offset 0)
                        (decf scroll-offset)))

                     ;; Arrow Up key
                     ((string= escape-sequence "[A")
                      (when (and history (< history-index (length history)))
                        (incf history-index)
                        (setf input-buffer (nth (1- history-index) history))))

                     ;; Arrow Down key
                     ((string= escape-sequence "[B")
                      (when (and history (> history-index 1))
                        (decf history-index)
                        (setf input-buffer (nth (1- history-index) history))))

                     ;; Standalone ESC - exit
                     ((string= escape-sequence "")
                      (return)))))

                ;; Enter - process input
                ((or (char= char #\Return) (char= char #\Newline))
                 (let ((trimmed-input (string-trim " " input-buffer)))
                   (when (string= trimmed-input "exit")
                     (return))

                   (when (> (length trimmed-input) 0)
                     ;; Add to history and evaluate
                     (push trimmed-input history)
                     (let ((result (safe-eval-string trimmed-input)))
                       (push result results))
                     (setf history-index 0)

                     ;; Auto-scroll to show latest
                     (let ((total-lines (length history)))
                       (when (> total-lines content-height)
                         (setf scroll-offset (- total-lines content-height)))))

                   ;; Reset input buffer
                   (setf input-buffer "")))

                ;; Backspace - remove character
                ((or (char= char #\Backspace) (char= char #\Del))
                 (when (> (length input-buffer) 0)
                   (setf input-buffer (subseq input-buffer 0 (1- (length input-buffer))))))

                ;; Ctrl+L to clear content area only
                ((char= char (code-char 12)) ; Ctrl+L
                 (setf history '()
                       results '()
                       scroll-offset 0))

                ;; Regular characters - add to buffer
                ((and (graphic-char-p char) (< (length input-buffer) 200))
                 (setf input-buffer (concatenate 'string input-buffer (string char)))))))

        (error (e)
          (move-cursor (- rows 3) 1)
          (set-color 1) ; Red
          (format t "Error: ~A" e)
          (reset-color))))))

(defun draw-horizontal-line (row cols)
  "Draw a horizontal separator line"
  (move-cursor row 1)
  (format t "~C[90m" #\Escape) ; Gray color directly
  (format t "~A" (make-string cols :initial-element #\-))
  (reset-color))

(defun draw-bottom-interface (rows cols input-buffer)
  "Draw the fixed prompt interface at the bottom"
  ;; Top separator line
  (draw-horizontal-line (- rows 2) cols)

  ;; Prompt line
  (move-cursor (- rows 1) 1)
  (clear-line)
  (set-color 6) ; Cyan
  (format t "tcode> ")
  (reset-color)
  (format t "~A" input-buffer)
  ;; White rectangle cursor
  (format t "~C[47m ~C[0m" #\Escape #\Escape)

  ;; Bottom separator line
  (draw-horizontal-line rows cols)
  (force-output))

(defun draw-content-area (content-height history results scroll-offset)
  "Draw the scrollable content area with history and results"
  (let ((display-start (max 0 scroll-offset))
        (display-end (min (length history) (+ scroll-offset content-height)))
        (current-row 1))

    ;; Clear the content area
    (loop for row from 1 to content-height do
      (move-cursor row 1)
      (clear-line))

    ;; Display visible history entries and their results
    (loop for i from display-start below display-end
          for hist-item = (nth (- (length history) 1 i) history)
          for result-item = (nth (- (length results) 1 i) results)
          do
          (when (<= current-row content-height)
            ;; Display command
            (move-cursor current-row 1)
            (set-color 6) ; Cyan
            (format t "tcode> ")
            (reset-color)
            (format t "~A" hist-item)
            (incf current-row))

          (when (and result-item (<= current-row content-height))
            ;; Display result
            (move-cursor current-row 1)
            (set-color 3) ; Yellow
            (format t "=> ")
            (reset-color)
            (format t "~A" result-item)
            (incf current-row))

          ;; Add blank line between entries if space allows
          (when (<= current-row content-height)
            (incf current-row)))

    (force-output)))


(defun safe-eval-string (input-string)
  "Safely evaluate a Lisp expression from string"
  (handler-case
      (let ((expr (read-from-string input-string)))
        (eval expr))
    (error (e)
      (format nil "Evaluation error: ~A" e))
    (reader-error (e)
      (format nil "Parse error: ~A" e))))
