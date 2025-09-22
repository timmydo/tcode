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
      (cleanup-pty pty)))))

(defstruct history-item
  (command "")
  (result nil))

(defstruct repl-context
  (input-buffer "")
  (history '())
  (history-index 0)
  (original-input "")
  (content-height 0)
  (scroll-offset 0)
  (status-message "")
  (state :normal)  ; :normal, :exit-warning
  (context-directories (list (namestring (truename ".")))))

(defun repl-loop (pty rows cols)
  "Main REPL loop with fixed prompt at bottom and scrollable history above"
  (declare (ignore pty))  ; PTY not used in current implementation
  (let ((ctx (make-repl-context :content-height (- rows 4))))

    ;; Initial screen setup
    (clear-screen)
    (draw-bottom-interface rows cols (repl-context-input-buffer ctx) (repl-context-status-message ctx))

    (loop
      (handler-case
          (progn
            ;; Draw the content area with history
            (draw-content-area (repl-context-content-height ctx) (repl-context-history ctx) (repl-context-scroll-offset ctx) cols)

            ;; Update prompt area
            (draw-bottom-interface rows cols (repl-context-input-buffer ctx) (repl-context-status-message ctx))

            ;; Read character
            (let ((char (read-char-raw)))
              (cond
                ;; Ctrl+C - handle double press to exit
                ((char= char (code-char 3))
                 (if (eq (repl-context-state ctx) :exit-warning)
                     (progn
                       ;; Clear screen and reset cursor before exiting
                       (clear-screen)
                       (move-cursor 1 1)
                       (return))  ; Exit on second Ctrl+C
                     (progn
                       (setf (repl-context-state ctx) :exit-warning
                             (repl-context-status-message ctx) "Press Ctrl-C again to exit..."))))

                ;; ESC sequence handling
                ((char= char (code-char 27))
                 (let ((escape-sequence (read-escape-sequence)))
                   (cond
                     ;; Page Up key - scroll up to see older entries
                     ((string= escape-sequence "[5~")
                      (let* ((total-entries (length (repl-context-history ctx)))
                             (entries-that-fit (floor (repl-context-content-height ctx) 3))
                             (max-scroll (max 0 (- total-entries entries-that-fit))))
                        (when (> total-entries entries-that-fit)
                          (setf (repl-context-scroll-offset ctx) (min max-scroll (+ (repl-context-scroll-offset ctx) 3))))))

                     ;; Page Down key - scroll down to see newer entries
                     ((string= escape-sequence "[6~")
                      (setf (repl-context-scroll-offset ctx) (max 0 (- (repl-context-scroll-offset ctx) 3))))

                     ;; Home key - scroll to top (oldest entries)
                     ;; Various terminals send different sequences for Home
                     ((or (string= escape-sequence "[H")
                          (string= escape-sequence "[1~")
                          (string= escape-sequence "[7~"))
                      (let* ((total-entries (length (repl-context-history ctx)))
                             (entries-that-fit (floor (repl-context-content-height ctx) 3))
                             (max-scroll (max 0 (- total-entries entries-that-fit))))
                        (setf (repl-context-scroll-offset ctx) max-scroll)))

                     ;; End key - scroll to bottom (newest entries)
                     ;; Various terminals send different sequences for End
                     ((or (string= escape-sequence "[F")
                          (string= escape-sequence "[4~")
                          (string= escape-sequence "[8~"))
                      (setf (repl-context-scroll-offset ctx) 0))

                     ;; Arrow Up key
                     ((string= escape-sequence "[A")
                      (when (and (repl-context-history ctx) (< (repl-context-history-index ctx) (length (repl-context-history ctx))))
                        ;; Save original input when first navigating into history
                        (when (= (repl-context-history-index ctx) 0)
                          (setf (repl-context-original-input ctx) (repl-context-input-buffer ctx)))
                        (incf (repl-context-history-index ctx))
                        (setf (repl-context-input-buffer ctx) (history-item-command (nth (1- (repl-context-history-index ctx)) (repl-context-history ctx)))
                              (repl-context-state ctx) :normal
                              (repl-context-status-message ctx) "")))

                     ;; Arrow Down key
                     ((string= escape-sequence "[B")
                      (when (and (repl-context-history ctx) (> (repl-context-history-index ctx) 0))
                        (decf (repl-context-history-index ctx))
                        (if (= (repl-context-history-index ctx) 0)
                            (setf (repl-context-input-buffer ctx) (repl-context-original-input ctx))
                            (setf (repl-context-input-buffer ctx) (history-item-command (nth (1- (repl-context-history-index ctx)) (repl-context-history ctx)))))
                        (setf (repl-context-state ctx) :normal
                              (repl-context-status-message ctx) "")))

                     ;; Standalone ESC - exit
                     ((string= escape-sequence "")
                      (clear-screen)
                      (move-cursor 1 1)
                      (return)))))

                ;; Enter - process input
                ((or (char= char #\Return) (char= char #\Newline))
                 (let ((trimmed-input (string-trim " " (repl-context-input-buffer ctx))))
                   (when (string= trimmed-input "exit")
                     (clear-screen)
                     (move-cursor 1 1)
                     (return))

                   (when (> (length trimmed-input) 0)
                     ;; Evaluate and handle all context updates
                     (submit-command trimmed-input ctx))))

                ;; Backspace - remove character
                ((or (char= char #\Backspace) (char= char #\Del))
                 (when (> (length (repl-context-input-buffer ctx)) 0)
                   (setf (repl-context-input-buffer ctx) (subseq (repl-context-input-buffer ctx) 0 (1- (length (repl-context-input-buffer ctx)))))
                   ;; Reset history navigation since user is editing
                   (setf (repl-context-history-index ctx) 0
                         (repl-context-original-input ctx) (repl-context-input-buffer ctx)
                         (repl-context-state ctx) :normal
                         (repl-context-status-message ctx) "")))

                ;; Ctrl+L to clear content area only
                ((char= char (code-char 12)) ; Ctrl+L
                 (setf (repl-context-history ctx) '()
                       (repl-context-scroll-offset ctx) 0
                       (repl-context-state ctx) :normal
                       (repl-context-status-message ctx) ""))

                ;; Regular characters - add to buffer
                ((and (graphic-char-p char) (< (length (repl-context-input-buffer ctx)) 200))
                 (setf (repl-context-input-buffer ctx) (concatenate 'string (repl-context-input-buffer ctx) (string char)))
                 ;; Reset history navigation since user is editing
                 (setf (repl-context-history-index ctx) 0
                       (repl-context-original-input ctx) (repl-context-input-buffer ctx)
                       (repl-context-state ctx) :normal
                       (repl-context-status-message ctx) "")))))


        (error (e)
          (move-cursor (- rows 3) 1)
          (set-color 1) ; Red
          (format t "Error: ~A" e)
          (reset-color))))))

(defun draw-horizontal-line (row cols)
  "Draw a horizontal separator line using Unicode box drawing characters"
  (move-cursor row 1)
  (format t "~C[90m" #\Escape) ; Gray color directly
  (format t "~A" (make-string cols :initial-element #\─))  ; Unicode horizontal line
  (reset-color))

(defun draw-bottom-interface (rows cols input-buffer status-message)
  "Draw the fixed prompt interface at the bottom with status line"
  ;; Top separator line
  (draw-horizontal-line (- rows 3) cols)

  ;; Prompt line
  (move-cursor (- rows 2) 1)
  (clear-line)
  (set-color 6) ; Cyan
  (format t "tcode> ")
  (reset-color)
  (format t "~A" input-buffer)
  ;; White rectangle cursor
  (format t "~C[47m ~C[0m" #\Escape #\Escape)

  ;; Bottom separator line
  (draw-horizontal-line (- rows 1) cols)

  ;; Status line
  (move-cursor rows 1)
  (clear-line)
  (when (> (length status-message) 0)
    (set-color 3) ; Yellow for status messages
    (format t "~A" status-message)
    (reset-color))

  ;; Move cursor back to prompt area and hide it
  (move-cursor (- rows 2) (+ 8 (length input-buffer)))
  (force-output))

(defun draw-content-area (content-height history scroll-offset cols)
  "Draw the scrollable content area with history items"
  (let ((total-entries (length history))
        (current-row 1)
        (content-width (- cols 2))) ; Reserve 2 columns for scroll bar

    ;; Clear the content area
    (loop for row from 1 to content-height do
      (move-cursor row 1)
      (clear-line))

    ;; Calculate which entries to show
    ;; scroll-offset=0 means show most recent entries (default)
    ;; Higher scroll-offset means show older entries
    (when (> total-entries 0)
      (let* ((entries-that-fit (floor content-height 3)) ; Each entry takes ~3 lines (cmd + result + blank)
             ;; Since history is newest-first (push adds to front), we need to reverse indexing
             ;; When scroll-offset=0, show entries from 0 to entries-that-fit (newest)
             ;; When scroll-offset>0, show older entries
             (start-index scroll-offset)
             (end-index (min total-entries (+ scroll-offset entries-that-fit))))

        ;; Display visible history entries and their results
        (loop for i from (1- end-index) downto start-index
              for hist-item = (nth i history)
              do
              (when (<= current-row content-height)
                ;; Display command (truncate if too long for content width)
                (move-cursor current-row 1)
                (set-color 6) ; Cyan
                (format t "tcode> ")
                (reset-color)
                (let ((cmd-text (if (> (length (history-item-command hist-item)) (- content-width 7))
                                    (concatenate 'string
                                                 (subseq (history-item-command hist-item) 0 (- content-width 10))
                                                 "...")
                                    (history-item-command hist-item))))
                  (format t "~A" cmd-text))
                (incf current-row))

              (when (and (history-item-result hist-item) (<= current-row content-height))
                ;; Display result (truncate if too long for content width)
                (move-cursor current-row 1)
                (set-color 3) ; Yellow
                (format t "=> ")
                (reset-color)
                (let ((result-text (if (> (length (format nil "~A" (history-item-result hist-item))) (- content-width 3))
                                       (concatenate 'string
                                                    (subseq (format nil "~A" (history-item-result hist-item)) 0 (- content-width 6))
                                                    "...")
                                       (format nil "~A" (history-item-result hist-item)))))
                  (format t "~A" result-text))
                (incf current-row))

              ;; Add blank line between entries if space allows
              (when (<= current-row content-height)
                (incf current-row)))))

    ;; Draw scroll bar on the right side
    (draw-scroll-bar content-height total-entries scroll-offset cols)

    (force-output)))

(defun draw-scroll-bar (content-height total-entries scroll-offset cols)
  "Draw a scroll bar on the right side indicating scroll position"
  (when (> total-entries 0)
    (let* ((entries-that-fit (floor content-height 3))
           (scrollbar-col (- cols 1))
           (scrollbar-height content-height)
           (total-scrollable (max 0 (- total-entries entries-that-fit))))

      ;; Only show scroll bar if there's content to scroll
      (when (> total-scrollable 0)
        ;; Calculate thumb position and size
        (let* ((thumb-size (max 1 (floor (* scrollbar-height entries-that-fit) total-entries)))
               (scrollable-range (- scrollbar-height thumb-size))
               ;; Reverse the thumb position since scroll-offset=0 should show thumb at bottom (newest)
               (thumb-position (if (> total-scrollable 0)
                                 (- scrollable-range (floor (* scrollable-range scroll-offset) total-scrollable))
                                 0)))

          ;; Draw scroll track (background)
          (loop for row from 1 to scrollbar-height do
            (move-cursor row scrollbar-col)
            (format t "~C[90m│~C[0m" #\Escape #\Escape)) ; Gray track

          ;; Draw scroll thumb (foreground)
          (loop for row from (1+ thumb-position) to (+ thumb-position thumb-size) do
            (when (<= row scrollbar-height)
              (move-cursor row scrollbar-col)
              (format t "~C[97m█~C[0m" #\Escape #\Escape)))))))) ; White thumb

