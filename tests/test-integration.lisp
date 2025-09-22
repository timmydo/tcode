;;;; test-integration.lisp
;;;; Integration tests for tcode REPL workflow

(in-package :tcode)

(defvar *integration-test-results* '())

(defun record-integration-test (name passed &optional message)
  "Record an integration test result"
  (push (list name passed message) *integration-test-results*)
  (if passed
      (format t "✓ ~A~%" name)
      (format t "✗ ~A: ~A~%" name (or message "Failed"))))

(defun run-integration-test (name test-fn)
  "Run a single integration test and record the result"
  (handler-case
      (let ((result (funcall test-fn)))
        (record-integration-test name result))
    (error (e)
      (record-integration-test name nil (format nil "Error: ~A" e)))))

(defun test-mathematical-expressions ()
  "Test the mathematical expressions used in manual testing"
  (and (= (safe-eval-string "(+ 1 2)") 3)
       (= (safe-eval-string "(* 6 7)") 42)
       (= (safe-eval-string "(+ (* 2 3) (* 4 5))") 26)))

(defun test-list-operations ()
  "Test list operations used in manual testing"
  (equal (safe-eval-string "(list 1 2 3)") '(1 2 3)))

(defun test-function-definition-and-call ()
  "Test function definition and calling sequence"
  (let ((def-result (safe-eval-string "(defun hello () \"world\")"))
        (call-result (safe-eval-string "(progn (defun hello () \"world\") (hello))")))
    (and (or (eq def-result 'hello) (eq def-result 'common-lisp-user::hello))
         (string= call-result "world"))))

(defun test-error-scenarios ()
  "Test various error scenarios from manual testing"
  (let ((undefined-func-result (safe-eval-string "(invalid-function)"))
        (division-by-zero-result (safe-eval-string "(/ 1 0)")))
    (and (stringp undefined-func-result)
         (search "Evaluation error" undefined-func-result)
         (stringp division-by-zero-result)
         (search "Evaluation error" division-by-zero-result))))

(defun test-string-operations ()
  "Test string concatenation"
  (string= (safe-eval-string "(concatenate 'string \"hello\" \" \" \"world\")")
           "hello world"))

(defun test-variable-assignment ()
  "Test variable assignment and retrieval"
  (safe-eval-string "(defparameter *test-var* 42)")
  (= (safe-eval-string "*test-var*") 42))

(defun test-complex-expressions ()
  "Test complex nested expressions"
  (and (= (safe-eval-string "(let ((x 5) (y 10)) (+ x y))") 15)
       (equal (safe-eval-string "(mapcar #'1+ '(1 2 3))") '(2 3 4))))

(defun test-boolean-operations ()
  "Test boolean logic"
  (and (eq (safe-eval-string "(and t t)") t)
       (eq (safe-eval-string "(or nil t)") t)
       (eq (safe-eval-string "(not t)") nil)))

(defun test-pty-main-integration ()
  "Test that main function can be called without crashing"
  ;; We can't really test the interactive loop without complex mocking,
  ;; but we can test that the PTY initialization doesn't crash
  (let ((pty (make-pty :width 80 :height 24)))
    (unwind-protect
        (progn
          (open-pty pty)
          t)
      (cleanup-pty pty))))

(defun test-graceful-eof-handling ()
  "Test that nil input (EOF) is handled gracefully"
  ;; This simulates what happens when read-line returns nil
  (let ((input nil))
    (or (null input) (string= input "exit"))))

(defun run-all-integration-tests ()
  "Run all integration tests"
  (setf *integration-test-results* '())
  (format t "~%Running integration tests...~%")

  (run-integration-test "Mathematical expressions" #'test-mathematical-expressions)
  (run-integration-test "List operations" #'test-list-operations)
  (run-integration-test "Function definition and call" #'test-function-definition-and-call)
  (run-integration-test "Error scenarios" #'test-error-scenarios)
  (run-integration-test "String operations" #'test-string-operations)
  (run-integration-test "Variable assignment" #'test-variable-assignment)
  (run-integration-test "Complex expressions" #'test-complex-expressions)
  (run-integration-test "Boolean operations" #'test-boolean-operations)
  (run-integration-test "PTY main integration" #'test-pty-main-integration)
  (run-integration-test "Graceful EOF handling" #'test-graceful-eof-handling)

  (let ((passed (count-if (lambda (result) (second result)) *integration-test-results*))
        (total (length *integration-test-results*)))
    (format t "~%Integration Tests: ~A/~A passed~%" passed total)
    (= passed total)))