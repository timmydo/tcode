;;;; test-history-refactor.lisp
;;;; Test the history-item refactoring

(in-package :tcode)

(defvar *history-refactor-test-results* '())

(defun record-history-refactor-test (name passed &optional message)
  "Record a history refactor test result"
  (push (list name passed message) *history-refactor-test-results*)
  (if passed
      (format t "✓ ~A~%" name)
      (format t "✗ ~A: ~A~%" name (or message "Failed"))))

(defun run-history-refactor-test (name test-fn)
  "Run a single history refactor test and record the result"
  (handler-case
      (let ((result (funcall test-fn)))
        (record-history-refactor-test name result))
    (error (e)
      (record-history-refactor-test name nil (format nil "Error: ~A" e)))))

(defun test-history-item-creation ()
  "Test creating history-item struct"
  (let ((item (make-history-item :command "(+ 1 2)" :result 3)))
    (and (string= (history-item-command item) "(+ 1 2)")
         (= (history-item-result item) 3))))

(defun test-history-item-with-result ()
  "Test history-item with evaluated result"
  (let* ((command "(* 6 7)")
         (result (safe-eval-string command))
         (item (make-history-item :command command :result result)))
    (and (string= (history-item-command item) command)
         (= (history-item-result item) 42))))

(defun test-repl-context-creation ()
  "Test creating repl-context with new structure"
  (let ((ctx (make-repl-context)))
    (and (null (repl-context-history ctx))
         (eq (repl-context-state ctx) :normal)
         (listp (repl-context-context-directories ctx)))))

(defun run-all-history-refactor-tests ()
  "Run all history refactor tests"
  (setf *history-refactor-test-results* '())
  (format t "~%Running history refactor tests...~%")

  (run-history-refactor-test "History item creation" #'test-history-item-creation)
  (run-history-refactor-test "History item with result" #'test-history-item-with-result)
  (run-history-refactor-test "REPL context creation" #'test-repl-context-creation)

  (let ((passed (count-if (lambda (result) (second result)) *history-refactor-test-results*))
        (total (length *history-refactor-test-results*)))
    (format t "~%History Refactor Tests: ~A/~A passed~%" passed total)
    (= passed total)))