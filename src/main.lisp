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

              ;; Load configuration file
              (unless (load-config-file)
                ;; If config loading failed, exit
                (return-from main nil))

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
  (cursor-position 0)  ; Position within input-buffer (0 = beginning)
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
    (draw-bottom-interface rows cols (repl-context-input-buffer ctx) (repl-context-cursor-position ctx) (repl-context-status-message ctx))

    (loop
      (handler-case
          (progn
            ;; Draw the content area with history
            (draw-content-area (repl-context-content-height ctx) (repl-context-history ctx) (repl-context-scroll-offset ctx) cols)

            ;; Update prompt area
            (draw-bottom-interface rows cols (repl-context-input-buffer ctx) (repl-context-cursor-position ctx) (repl-context-status-message ctx))

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

                ;; Ctrl+A - move cursor to beginning
                ((char= char (code-char 1))
                 (setf (repl-context-cursor-position ctx) 0
                       (repl-context-state ctx) :normal
                       (repl-context-status-message ctx) ""))

                ;; Ctrl+E - move cursor to end
                ((char= char (code-char 5))
                 (setf (repl-context-cursor-position ctx) (length (repl-context-input-buffer ctx))
                       (repl-context-state ctx) :normal
                       (repl-context-status-message ctx) ""))

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

                     ;; Delete key - delete character under cursor
                     ;; Different terminals send different sequences for Delete
                     ((or (string= escape-sequence "[3~")   ; Most common Delete key sequence
                          (string= escape-sequence "[P")    ; Some terminals
                          (string= escape-sequence "[M"))   ; Alternative sequence
                      (when (< (repl-context-cursor-position ctx) (length (repl-context-input-buffer ctx)))
                        (let* ((current-buffer (repl-context-input-buffer ctx))
                               (cursor-pos (repl-context-cursor-position ctx))
                               (before (subseq current-buffer 0 cursor-pos))
                               (after (subseq current-buffer (1+ cursor-pos)))
                               (new-buffer (concatenate 'string before after)))
                          (setf (repl-context-input-buffer ctx) new-buffer))
                        ;; Reset history navigation since user is editing
                        (setf (repl-context-history-index ctx) 0
                              (repl-context-original-input ctx) (repl-context-input-buffer ctx)
                              (repl-context-state ctx) :normal
                              (repl-context-status-message ctx) "")))

                     ;; Home key - move cursor to beginning of line
                     ;; Various terminals send different sequences for Home
                     ((or (string= escape-sequence "[H")
                          (string= escape-sequence "[1~")
                          (string= escape-sequence "[7~"))
                      (setf (repl-context-cursor-position ctx) 0
                            (repl-context-state ctx) :normal
                            (repl-context-status-message ctx) ""))

                     ;; End key - move cursor to end of line
                     ;; Various terminals send different sequences for End
                     ((or (string= escape-sequence "[F")
                          (string= escape-sequence "[4~")
                          (string= escape-sequence "[8~"))
                      (setf (repl-context-cursor-position ctx) (length (repl-context-input-buffer ctx))
                            (repl-context-state ctx) :normal
                            (repl-context-status-message ctx) ""))

                     ;; Arrow Up key
                     ((string= escape-sequence "[A")
                      (when (and (repl-context-history ctx) (< (repl-context-history-index ctx) (length (repl-context-history ctx))))
                        ;; Save original input when first navigating into history
                        (when (= (repl-context-history-index ctx) 0)
                          (setf (repl-context-original-input ctx) (repl-context-input-buffer ctx)))
                        (incf (repl-context-history-index ctx))
                        (setf (repl-context-input-buffer ctx) (history-item-command (nth (1- (repl-context-history-index ctx)) (repl-context-history ctx)))
                              (repl-context-cursor-position ctx) (length (repl-context-input-buffer ctx))
                              (repl-context-state ctx) :normal
                              (repl-context-status-message ctx) "")))

                     ;; Arrow Down key
                     ((string= escape-sequence "[B")
                      (when (and (repl-context-history ctx) (> (repl-context-history-index ctx) 0))
                        (decf (repl-context-history-index ctx))
                        (if (= (repl-context-history-index ctx) 0)
                            (setf (repl-context-input-buffer ctx) (repl-context-original-input ctx))
                            (setf (repl-context-input-buffer ctx) (history-item-command (nth (1- (repl-context-history-index ctx)) (repl-context-history ctx)))))
                        (setf (repl-context-cursor-position ctx) (length (repl-context-input-buffer ctx))
                              (repl-context-state ctx) :normal
                              (repl-context-status-message ctx) "")))

                     ;; Arrow Left key
                     ((string= escape-sequence "[D")
                      (when (> (repl-context-cursor-position ctx) 0)
                        (decf (repl-context-cursor-position ctx))
                        (setf (repl-context-state ctx) :normal
                              (repl-context-status-message ctx) "")))

                     ;; Arrow Right key
                     ((string= escape-sequence "[C")
                      (when (< (repl-context-cursor-position ctx) (length (repl-context-input-buffer ctx)))
                        (incf (repl-context-cursor-position ctx))
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

                ;; Backspace - remove character to the left of cursor
                ((or (char= char #\Backspace) (char= char #\Del))
                 (when (and (> (length (repl-context-input-buffer ctx)) 0)
                            (> (repl-context-cursor-position ctx) 0))
                   (let* ((current-buffer (repl-context-input-buffer ctx))
                          (cursor-pos (repl-context-cursor-position ctx))
                          (before (subseq current-buffer 0 (1- cursor-pos)))
                          (after (subseq current-buffer cursor-pos))
                          (new-buffer (concatenate 'string before after)))
                     (setf (repl-context-input-buffer ctx) new-buffer
                           (repl-context-cursor-position ctx) (1- cursor-pos)))
                   ;; Reset history navigation since user is editing
                   (setf (repl-context-history-index ctx) 0
                         (repl-context-original-input ctx) (repl-context-input-buffer ctx)
                         (repl-context-state ctx) :normal
                         (repl-context-status-message ctx) "")))

                ;; Ctrl+D - delete character under cursor, or exit if buffer is empty
                ((char= char (code-char 4)) ; Ctrl+D
                 (if (= (length (repl-context-input-buffer ctx)) 0)
                     ;; If buffer is empty, exit like in bash
                     (progn
                       (clear-screen)
                       (move-cursor 1 1)
                       (return))
                     ;; Otherwise, delete character under cursor
                     (when (< (repl-context-cursor-position ctx) (length (repl-context-input-buffer ctx)))
                       (let* ((current-buffer (repl-context-input-buffer ctx))
                              (cursor-pos (repl-context-cursor-position ctx))
                              (before (subseq current-buffer 0 cursor-pos))
                              (after (subseq current-buffer (1+ cursor-pos)))
                              (new-buffer (concatenate 'string before after)))
                         (setf (repl-context-input-buffer ctx) new-buffer))
                       ;; Reset history navigation since user is editing
                       (setf (repl-context-history-index ctx) 0
                             (repl-context-original-input ctx) (repl-context-input-buffer ctx)
                             (repl-context-state ctx) :normal
                             (repl-context-status-message ctx) ""))))

                ;; Ctrl+K - kill line from cursor to end
                ((char= char (code-char 11)) ; Ctrl+K
                 (when (< (repl-context-cursor-position ctx) (length (repl-context-input-buffer ctx)))
                   (let* ((current-buffer (repl-context-input-buffer ctx))
                          (cursor-pos (repl-context-cursor-position ctx))
                          (new-buffer (subseq current-buffer 0 cursor-pos)))
                     (setf (repl-context-input-buffer ctx) new-buffer))
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

                ;; Regular characters - insert at cursor position
                ((and (graphic-char-p char) (< (length (repl-context-input-buffer ctx)) 200))
                 (let* ((current-buffer (repl-context-input-buffer ctx))
                        (cursor-pos (repl-context-cursor-position ctx))
                        (before (subseq current-buffer 0 cursor-pos))
                        (after (subseq current-buffer cursor-pos))
                        (new-buffer (concatenate 'string before (string char) after)))
                   (setf (repl-context-input-buffer ctx) new-buffer
                         (repl-context-cursor-position ctx) (1+ cursor-pos)))
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

(defun draw-bottom-interface (rows cols input-buffer cursor-position status-message)
  "Draw the fixed prompt interface at the bottom with status line"
  ;; Top separator line
  (draw-horizontal-line (- rows 3) cols)

  ;; Prompt line
  (move-cursor (- rows 2) 1)
  (clear-line)
  (set-color 6) ; Cyan
  (format t "tcode> ")
  (reset-color)

  ;; Draw input buffer first
  (format t "~A" input-buffer)

  ;; If cursor is at the end, add a space; otherwise we'll overwrite the character
  (when (= cursor-position (length input-buffer))
    (format t " "))

  ;; Position cursor and draw inverted character at cursor position
  (when (>= cursor-position 0)
    (move-cursor (- rows 2) (+ 8 cursor-position))
    ;; Draw inverted character at cursor position (black text on white background)
    (format t "~C[47;31m~A~C[0m" #\Escape
            (if (< cursor-position (length input-buffer))
                (char input-buffer cursor-position)
                #\Space)
            #\Escape))

  ;; Bottom separator line
  (draw-horizontal-line (- rows 1) cols)

  ;; Status line
  (move-cursor rows 1)
  (clear-line)
  (when (> (length status-message) 0)
    (set-color 3) ; Yellow for status messages
    (format t "~A" status-message)
    (reset-color))

  ;; Position cursor at the cursor location (already done above, but ensure it's there)
  (move-cursor (- rows 2) (+ 8 cursor-position))
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
                (let ((cmd-lines (wrap-text (history-item-command hist-item) (- content-width 7))))
                  (loop for line in cmd-lines
                        for first-line = t then nil
                        do
                        (when (<= current-row content-height)
                          (when (not first-line)
                            (move-cursor current-row 1)
                            (format t "       ")) ; 7 spaces to align continuation lines
                          (format t "~A" line)
                          (incf current-row)))))

              (when (and (history-item-result hist-item) (<= current-row content-height))
                ;; Display result with word wrapping
                (move-cursor current-row 1)
                (set-color 3) ; Yellow
                (format t "=> ")
                (reset-color)
                (let ((result-lines (wrap-text (format nil "~A" (history-item-result hist-item)) (- content-width 3))))
                  (loop for line in result-lines
                        for first-line = t then nil
                        do
                        (when (<= current-row content-height)
                          (when (not first-line)
                            (move-cursor current-row 1)
                            (format t "   ")) ; 3 spaces to align continuation lines
                          (format t "~A" line)
                          (incf current-row)))))

              ;; Add blank line between entries if space allows
              (when (<= current-row content-height)
                (incf current-row)))))

    ;; Draw scroll bar on the right side
    (draw-scroll-bar content-height total-entries scroll-offset cols)

    (force-output)))

(defun wrap-text (text max-width)
  "Break text into lines that fit within max-width, preferring word boundaries"
  (when (or (null text) (zerop (length text)))
    (return-from wrap-text '("")))

  (let ((lines '())
        (current-line "")
        (words (split-string text #\Space)))

    (dolist (word words)
      (let ((potential-line (if (zerop (length current-line))
                                word
                                (concatenate 'string current-line " " word))))
        (if (<= (length potential-line) max-width)
            (setf current-line potential-line)
            (progn
              ;; Current line is full, start new line
              (when (> (length current-line) 0)
                (push current-line lines))
              ;; If word itself is too long, break it forcefully
              (if (> (length word) max-width)
                  (progn
                    ;; Break long word across multiple lines
                    (loop with remaining = word
                          while (> (length remaining) 0)
                          do (let ((chunk (subseq remaining 0 (min max-width (length remaining)))))
                               (push chunk lines)
                               (setf remaining (subseq remaining (length chunk)))))
                    (setf current-line ""))
                  (setf current-line word))))))

    ;; Add final line if not empty
    (when (> (length current-line) 0)
      (push current-line lines))

    ;; Return lines in correct order
    (nreverse lines)))

(defun split-string (string separator)
  "Split string by separator character"
  (when (null string)
    (return-from split-string '()))

  (let ((result '())
        (current "")
        (len (length string)))

    (loop for i from 0 below len
          do (let ((char (char string i)))
               (if (char= char separator)
                   (progn
                     (push current result)
                     (setf current ""))
                   (setf current (concatenate 'string current (string char))))))

    ;; Add final part
    (push current result)

    ;; Return in correct order
    (nreverse result)))

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

