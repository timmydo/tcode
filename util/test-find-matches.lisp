;;; Test for find-matches-from-point function
;;; This test helps catch issues like the let vs let* bug we just fixed

(load "../tle.asd")
(asdf:load-system :tle)

(in-package :tle)

(defun test-find-matches-basic ()
  "Test basic functionality of find-matches-from-point"
  (let ((buffer (make-empty-buffer "test")))
    ;; Add some test content
    (buffer-insert buffer "hello world")
    (buffer-insert buffer (format nil "~%hello again"))
    (buffer-insert buffer (format nil "~%world hello"))
    
    ;; Reset cursor to beginning
    (buffer-set-point buffer 0 0)
    
    ;; Test finding "hello"
    (let ((matches (find-matches-from-point buffer "hello")))
      (format t "Test 1 - Find 'hello': ~A~%" matches)
      (assert (= (length matches) 3) () "Expected 3 matches for 'hello', got ~A" (length matches))
      (assert (equal (first matches) '(0 0)) () "First match should be at (0 0), got ~A" (first matches))
      (assert (equal (second matches) '(1 0)) () "Second match should be at (1 0), got ~A" (second matches))
      (assert (equal (third matches) '(2 6)) () "Third match should be at (2 6), got ~A" (third matches)))
    
    ;; Test finding "world"
    (let ((matches (find-matches-from-point buffer "world")))
      (format t "Test 2 - Find 'world': ~A~%" matches)
      (assert (= (length matches) 2) () "Expected 2 matches for 'world', got ~A" (length matches))
      (assert (equal (first matches) '(0 6)) () "First match should be at (0 6), got ~A" (first matches))
      (assert (equal (second matches) '(2 0)) () "Second match should be at (2 0), got ~A" (second matches)))
    
    ;; Test finding from middle position
    (buffer-set-point buffer 1 3)  ; Position after "hel" on second line
    (let ((matches (find-matches-from-point buffer "hello")))
      (format t "Test 3 - Find 'hello' from middle: ~A~%" matches)
      (assert (= (length matches) 1) () "Expected 1 match for 'hello' from middle, got ~A" (length matches))
      (assert (equal (first matches) '(2 6)) () "Match should be at (2 6), got ~A" (first matches)))
    
    ;; Test finding non-existent string
    (buffer-set-point buffer 0 0)
    (let ((matches (find-matches-from-point buffer "nonexistent")))
      (format t "Test 4 - Find 'nonexistent': ~A~%" matches)
      (assert (= (length matches) 0) () "Expected 0 matches for 'nonexistent', got ~A" (length matches)))
    
    (format t "Basic tests passed!~%")))

(defun test-find-matches-edge-cases ()
  "Test edge cases for find-matches-from-point"
  (let ((buffer (make-empty-buffer "test")))
    ;; Test with empty buffer
    (let ((matches (find-matches-from-point buffer "test")))
      (format t "Test 5 - Empty buffer: ~A~%" matches)
      (assert (= (length matches) 0) () "Expected 0 matches in empty buffer, got ~A" (length matches)))
    
    ;; Test with single character
    (buffer-insert buffer "a")
    (buffer-set-point buffer 0 0)
    (let ((matches (find-matches-from-point buffer "a")))
      (format t "Test 6 - Single character: ~A~%" matches)
      (assert (= (length matches) 1) () "Expected 1 match for 'a', got ~A" (length matches))
      (assert (equal (first matches) '(0 0)) () "Match should be at (0 0), got ~A" (first matches)))
    
    ;; Test overlapping matches
    (setf (lines buffer) (make-array 1 :fill-pointer 1 :adjustable t :initial-contents '("aaaa")))
    (buffer-set-point buffer 0 0)
    (let ((matches (find-matches-from-point buffer "aa")))
      (format t "Test 7 - Overlapping matches: ~A~%" matches)
      ;; Should find "aa" at positions 0, 1, 2 (overlapping)
      (assert (>= (length matches) 2) () "Expected at least 2 matches for 'aa' in 'aaaa', got ~A" (length matches)))
    
    (format t "Edge case tests passed!~%")))

(defun run-tests ()
  "Run all tests for find-matches-from-point"
  (format t "Running find-matches-from-point tests...~%")
  
  (handler-case
      (progn
        (test-find-matches-basic)
        (test-find-matches-edge-cases)
        (format t "~%All tests passed!~%")
        t)
    (error (e)
      (format t "~%Test failed with error: ~A~%" e)
      nil)))

;; Run tests if this file is executed directly
(when (string= (pathname-name *load-pathname*) "test-find-matches")
  (run-tests))