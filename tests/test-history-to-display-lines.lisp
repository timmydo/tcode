;;;; test-history-to-display-lines.lisp
;;;; Test the history-to-display-lines function

(in-package :tcode)

(defvar *history-to-display-lines-test-results* '())

(defun record-history-display-test (name passed &optional message)
  "Record a history-to-display-lines test result"
  (push (list name passed message) *history-to-display-lines-test-results*)
  (if passed
      (format t "✓ ~A~%" name)
      (format t "✗ ~A: ~A~%" name (or message "Failed"))))

(defun run-history-display-test (name test-fn)
  "Run a single history-to-display-lines test and record the result"
  (handler-case
      (let ((result (funcall test-fn)))
        (record-history-display-test name result))
    (error (e)
      (record-history-display-test name nil (format nil "Error: ~A" e)))))

(defun test-empty-history ()
  "Test history-to-display-lines with empty history"
  (let ((result (history-to-display-lines '() 80)))
    (and (listp result)
         (null result))))

(defun test-single-history-item-command-only ()
  "Test history-to-display-lines with single item (command only)"
  (let* ((item (make-history-item :command "(+ 1 2)" :result nil))
         (history (list item))
         (result (history-to-display-lines history 80)))
    (and (= (length result) 2) ; command line + blank line
         (string= (first result)
                  (format nil "~C[36mtcode> ~C[0m(+ 1 2)" #\Escape #\Escape))
         (string= (second result) ""))))

(defun test-single-history-item-with-result ()
  "Test history-to-display-lines with single item (command and result)"
  (let* ((item (make-history-item :command "(+ 1 2)" :result 3))
         (history (list item))
         (result (history-to-display-lines history 80)))
    (and (= (length result) 3) ; command line + result line + blank line
         (string= (first result)
                  (format nil "~C[36mtcode> ~C[0m(+ 1 2)" #\Escape #\Escape))
         (string= (second result) "3")
         (string= (third result) ""))))

(defun test-multiple-history-items ()
  "Test history-to-display-lines with multiple items"
  (let* ((item1 (make-history-item :command "(+ 1 2)" :result 3))
         (item2 (make-history-item :command "(* 4 5)" :result 20))
         (history (list item1 item2)) ; newest first
         (result (history-to-display-lines history 80)))
    (and (= (length result) 6) ; 2 commands + 2 results + 2 blank lines
         ;; First item (newest)
         (string= (first result)
                  (format nil "~C[36mtcode> ~C[0m(+ 1 2)" #\Escape #\Escape))
         (string= (second result) "3")
         (string= (third result) "")
         ;; Second item (older)
         (string= (fourth result)
                  (format nil "~C[36mtcode> ~C[0m(* 4 5)" #\Escape #\Escape))
         (string= (fifth result) "20")
         (string= (sixth result) ""))))

(defun test-command-wrapping ()
  "Test history-to-display-lines with long command that needs wrapping"
  (let* ((long-command "This is a very long command that should wrap across multiple lines when the content width is small")
         (item (make-history-item :command long-command :result nil))
         (history (list item))
         (result (history-to-display-lines history 40))) ; Small width to force wrapping
    (and (>= (length result) 3) ; At least command lines + blank line
         ;; First line should have the tcode> prefix with escape sequences
         (string-match-p (string #\Escape) (first result))
         (string-match-p "tcode>" (first result))
         ;; Should have multiple lines for the command
         (> (count-if (lambda (line) (and (not (string= line ""))
                                          (not (string-match-p "tcode>" line))))
                      result) 0))))

(defun test-result-wrapping ()
  "Test history-to-display-lines with long result that needs wrapping"
  (let* ((item (make-history-item :command "(test)"
                                  :result "This is a very long result that should wrap across multiple lines when the content width is small"))
         (history (list item))
         (result (history-to-display-lines history 30))) ; Small width to force wrapping
    (and (>= (length result) 4) ; Command + multiple result lines + blank line
         ;; Should have multiple result lines
         (> (count-if (lambda (line) (and (not (string= line ""))
                                          (not (string-match-p "tcode>" line))))
                      result) 1))))

(defun test-multiline-command ()
  "Test history-to-display-lines with command containing newlines"
  (let* ((multiline-cmd "(let ((x 1)
      (y 2))
  (+ x y))")
         (item (make-history-item :command multiline-cmd :result 3))
         (history (list item))
         (result (history-to-display-lines history 80)))
    (and (>= (length result) 3) ; Multiple command lines + result + blank line
         ;; First line should have tcode> prefix with escape sequences
         (string-match-p (string #\Escape) (first result))
         (string-match-p "tcode>" (first result))
         ;; Should have the result
         (member "3" result :test #'string=))))

(defun test-nil-result-vs-no-result ()
  "Test that nil result is displayed vs no result field"
  (let* ((item-with-nil (make-history-item :command "(test)" :result nil))
         (item-no-result (make-history-item :command "(test)"))
         (history1 (list item-with-nil))
         (history2 (list item-no-result))
         (result1 (history-to-display-lines history1 80))
         (result2 (history-to-display-lines history2 80)))
    ;; Both should behave the same way - no result lines should be added for nil
    (= (length result1) (length result2))))

(defun test-various-result-types ()
  "Test history-to-display-lines with various result types"
  (let* ((item-num (make-history-item :command "(+ 1 2)" :result 3))
         (item-str (make-history-item :command "(format nil \"test\")" :result "test"))
         (item-list (make-history-item :command "'(1 2 3)" :result '(1 2 3)))
         (history (list item-num item-str item-list))
         (result (history-to-display-lines history 80)))
    (and (>= (length result) 6) ; 3 commands + results + blanks
         ;; Should contain string representations of all results
         (member "3" result :test #'string=)
         (member "test" result :test #'string=)
         (some (lambda (line) (string-match-p "1 2 3" line)) result))))

(defun string-match-p (pattern string)
  "Simple pattern matching for test purposes"
  (search pattern string))

(defun run-all-history-to-display-lines-tests ()
  "Run all history-to-display-lines tests"
  (setf *history-to-display-lines-test-results* '())
  (format t "~%Running history-to-display-lines tests...~%")

  (run-history-display-test "Empty history" #'test-empty-history)
  (run-history-display-test "Single item command only" #'test-single-history-item-command-only)
  (run-history-display-test "Single item with result" #'test-single-history-item-with-result)
  (run-history-display-test "Multiple history items" #'test-multiple-history-items)
  (run-history-display-test "Command wrapping" #'test-command-wrapping)
  (run-history-display-test "Result wrapping" #'test-result-wrapping)
  (run-history-display-test "Multiline command" #'test-multiline-command)
  (run-history-display-test "Nil result vs no result" #'test-nil-result-vs-no-result)
  (run-history-display-test "Various result types" #'test-various-result-types)

  (let ((passed (count-if (lambda (result) (second result)) *history-to-display-lines-test-results*))
        (total (length *history-to-display-lines-test-results*)))
    (format t "~%History-to-display-lines Tests: ~A/~A passed~%" passed total)
    (= passed total)))