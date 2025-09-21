;;;; test-paren-checker.lisp - Tests for parenthesis checker utility
;;;;
;;;; To run these tests:
;;;;   cd util/tests && sbcl --non-interactive --load test-paren-checker.lisp
;;;;
;;;; Or from the project root:
;;;;   cd util/tests && sbcl --load test-paren-checker.lisp
;;;;
;;;; The tests will run automatically when the file is loaded.

(load "../paren-checker.lisp")

(defpackage :test-paren-checker
  (:use :common-lisp :paren-checker))

(in-package :test-paren-checker)

(defvar *test-count* 0)
(defvar *failed-tests* 0)

(defmacro deftest (name &body body)
  `(progn
     (incf *test-count*)
     (handler-case
         (progn
           ,@body
           (format t "PASS: ~A~%" ',name))
       (error (e)
         (incf *failed-tests*)
         (format t "FAIL: ~A - ~A~%" ',name e)))))

(defun test-valid-code (code description)
  "Test that valid code passes"
  (format t "Testing valid: ~A~%" description)
  (incf *test-count*)
  (handler-case
      (progn
        (check-parens code)
        (format t "PASS: ~A~%" description))
    (error (e)
      (incf *failed-tests*)
      (format t "FAIL: ~A - ~A~%" description e))))

(defun test-invalid-code (code description expected-line expected-column)
  "Test that invalid code fails with correct position"
  (format t "Testing invalid: ~A~%" description)
  (incf *test-count*)
  (handler-case
      (progn
        (check-parens code)
        (incf *failed-tests*)
        (format t "FAIL: ~A - Expected error but got none~%" description))
    (paren-error (e)
      (if (and (= (paren-error-line e) expected-line)
               (= (paren-error-column e) expected-column))
          (format t "PASS: ~A~%" description)
          (progn
            (incf *failed-tests*)
            (format t "FAIL: ~A - Expected line ~A col ~A, got line ~A col ~A~%"
                    description expected-line expected-column
                    (paren-error-line e) (paren-error-column e)))))
    (error (e)
      (incf *failed-tests*)
      (format t "FAIL: ~A - Unexpected error: ~A~%" description e))))

(defun run-tests ()
  (setf *test-count* 0
        *failed-tests* 0)
  
  (format t "Running parenthesis checker tests...~%~%")
  
  ;; Valid code tests
  (test-valid-code "(defun hello () \"world\")" "simple function")
  (test-valid-code "(+ 1 2 3)" "simple arithmetic")
  (test-valid-code "(list (cons 'a 'b) (cons 'c 'd))" "nested lists")
  (test-valid-code "[1 2 3]" "square brackets")
  (test-valid-code "{:key value}" "curly braces")
  (test-valid-code "(mixed [array {hash}])" "mixed bracket types")
  
  ;; String handling tests
  (test-valid-code "(print \"hello (world)\")" "parentheses in strings")
  (test-valid-code "(print \"quote\\\"test\\\"\")" "escaped quotes in strings")
  (test-valid-code "(print \"line1
line2\")" "newlines in strings")
  
  ;; Comment handling tests
  (test-valid-code "(+ 1 2) ; comment with (parens)" "parentheses in comments")
  (test-valid-code "; comment line
(+ 1 2)" "comment before code")
  (test-valid-code "(+ 1 2)
; comment after" "comment after code")
  
  ;; Empty and whitespace tests
  (test-valid-code "" "empty string")
  (test-valid-code "   
  	  " "whitespace only")
  (test-valid-code "()" "empty parentheses")
  
  ;; Invalid code tests
  (test-invalid-code "(" "unclosed parenthesis" 1 1)
  (test-invalid-code ")" "unexpected closing parenthesis" 1 1)
  (test-invalid-code "(defun test ()" "missing closing parenthesis" 1 1)
  (test-invalid-code "(+ 1 2]" "mismatched brackets" 1 7)
  (test-invalid-code "[1 2 3)" "mismatched square to round" 1 7)
  (test-invalid-code "{key value]" "mismatched brace to square" 1 11)
  
  ;; Multi-line error tests
  (test-invalid-code "(defun test
  (let ((x 1))
    (+ x 2]" "multiline mismatch" 3 11)
  (test-invalid-code "(first
(second
(third" "multiple unclosed" 3 1)
  
  ;; Complex cases
  (test-invalid-code "(print \"string with ) in it\") extra)" "extra closing after string" 1 36)
  (test-invalid-code "(code
; comment (
  more code]" "mismatch after comment" 3 12)
  
  (format t "~%Test Results:~%")
  (format t "Total tests: ~A~%" *test-count*)
  (format t "Failed tests: ~A~%" *failed-tests*)
  (format t "Passed tests: ~A~%" (- *test-count* *failed-tests*))
  
  (if (= *failed-tests* 0)
      (format t "~%All tests PASSED!~%")
      (format t "~%~A tests FAILED!~%" *failed-tests*))
  
  *failed-tests*)

;; File-based tests
(defun create-test-files ()
  (with-open-file (stream "valid-test.lisp" :direction :output :if-exists :supersede)
    (write-string "(defun factorial (n)
  (if (zerop n)
      1
      (* n (factorial (1- n)))))" stream))
  
  (with-open-file (stream "invalid-test.lisp" :direction :output :if-exists :supersede)
    (write-string "(defun broken (n)
  (if (zerop n)
      1
      (* n (factorial (1- n))" stream)))

(defun test-file-operations ()
  (format t "~%Testing file operations...~%")
  (create-test-files)
  
  (incf *test-count*)
  (handler-case
      (progn
        (check-parens-file "valid-test.lisp")
        (format t "PASS: valid file test~%"))
    (error (e)
      (incf *failed-tests*)
      (format t "FAIL: valid file test - ~A~%" e)))
  
  (incf *test-count*)
  (handler-case
      (progn
        (check-parens-file "invalid-test.lisp")
        (incf *failed-tests*)
        (format t "FAIL: invalid file test - Expected error but got none~%"))
    (paren-error (e)
      (format t "PASS: invalid file test - ~A~%" e))
    (error (e)
      (incf *failed-tests*)
      (format t "FAIL: invalid file test - Unexpected error: ~A~%" e)))
  
  (delete-file "valid-test.lisp")
  (delete-file "invalid-test.lisp"))

(defun main ()
  (run-tests)
  (test-file-operations)
  (if (= *failed-tests* 0)
      (progn
        (format t "~%All tests completed successfully!~%")
        0)
      (progn
        (format t "~%Test suite failed with ~A failures~%" *failed-tests*)
        1)))

;; Run tests when loaded
(main)