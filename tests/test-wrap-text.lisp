;;;; test-wrap-text.lisp
;;;; Test the wrap-text function functionality

(require 'asdf)
(push #P"/home/timmy/src/tcode/" asdf:*central-registry*)
(asdf:load-system :tcode)

(in-package :tcode)

(defun test-basic-wrapping ()
  "Test basic text wrapping functionality"
  (format t "~%Test 1 - Basic wrapping:~%")

  ;; Test normal text wrapping
  (let ((result (wrap-text "This is a simple test of text wrapping" 10)))
    (format t "Input: 'This is a simple test of text wrapping' (width 10)~%")
    (format t "Result: ~A~%" result)
    (assert (equal result '("This is a" "simple" "test of" "text" "wrapping"))))

  ;; Test exact width match
  (let ((result (wrap-text "hello" 5)))
    (format t "Input: 'hello' (width 5)~%")
    (format t "Result: ~A~%" result)
    (assert (equal result '("hello"))))

  ;; Test single word longer than width
  (let ((result (wrap-text "supercalifragilisticexpialidocious" 10)))
    (format t "Input: 'supercalifragilisticexpialidocious' (width 10)~%")
    (format t "Result: ~A~%" result)
    (assert (equal result '("supercalif" "ragilistic" "expialidoc" "ious"))))

  (format t "Basic wrapping tests passed!~%"))

(defun test-edge-cases ()
  "Test edge cases for wrap-text"
  (format t "~%Test 2 - Edge cases:~%")

  ;; Test empty string
  (let ((result (wrap-text "" 10)))
    (format t "Input: '' (width 10)~%")
    (format t "Result: ~A~%" result)
    (assert (equal result '(""))))

  ;; Test nil
  (let ((result (wrap-text nil 10)))
    (format t "Input: nil (width 10)~%")
    (format t "Result: ~A~%" result)
    (assert (equal result '(""))))

  ;; Test single space
  (let ((result (wrap-text " " 10)))
    (format t "Input: ' ' (width 10)~%")
    (format t "Result: ~A~%" result)
    (format t "Note: Single space splits into parts~%"))

  ;; Test multiple spaces
  (let ((result (wrap-text "  hello  world  " 10)))
    (format t "Input: '  hello  world  ' (width 10)~%")
    (format t "Result: ~A~%" result)
    (format t "Note: Multiple spaces create empty strings in result~%"))

  ;; Test very small width
  (let ((result (wrap-text "abc" 1)))
    (format t "Input: 'abc' (width 1)~%")
    (format t "Result: ~A~%" result)
    (format t "Note: Very small width forces character-by-character breaking~%"))

  (format t "Edge case tests passed!~%"))

(defun test-text-with-newlines ()
  "Test text containing newlines - this reveals current behavior"
  (format t "~%Test 3 - Text with newlines:~%")

  ;; Test text with single newline
  (let* ((input (concatenate 'string "Hello" (string #\Newline) "World"))
         (result (wrap-text input 10)))
    (format t "Input: 'Hello\\nWorld' (width 10)~%")
    (format t "Result: ~A~%" result)
    ;; Current implementation treats newline as part of word, so it gets wrapped
    (format t "Note: Current implementation treats newline as part of word~%"))

  ;; Test text with multiple newlines
  (let* ((input (concatenate 'string "Line1" (string #\Newline) (string #\Newline) "Line3"))
         (result (wrap-text input 10)))
    (format t "Input: 'Line1\\n\\nLine3' (width 10)~%")
    (format t "Result: ~A~%" result)
    (format t "Note: Current implementation doesn't handle newlines specially~%"))

  ;; Test mixed newlines and spaces
  (let* ((input (concatenate 'string "First line" (string #\Newline) "Second line"))
         (result (wrap-text input 15)))
    (format t "Input: 'First line\\nSecond line' (width 15)~%")
    (format t "Result: ~A~%" result)
    (format t "Note: Newline character becomes part of the word~%"))

  ;; Test paragraph with newlines
  (let* ((input (concatenate 'string "Paragraph one." (string #\Newline) (string #\Newline) "Paragraph two."))
         (result (wrap-text input 12)))
    (format t "Input: 'Paragraph one.\\n\\nParagraph two.' (width 12)~%")
    (format t "Result: ~A~%" result))

  (format t "Newline tests completed (behavior documented)~%"))

(defun test-whitespace-handling ()
  "Test various whitespace scenarios"
  (format t "~%Test 4 - Whitespace handling:~%")

  ;; Test tab characters
  (let* ((input (concatenate 'string "Hello" (string #\Tab) "World"))
         (result (wrap-text input 10)))
    (format t "Input: 'Hello\\tWorld' (width 10)~%")
    (format t "Result: ~A~%" result))

  ;; Test leading/trailing spaces
  (let ((result (wrap-text "  hello world  " 8)))
    (format t "Input: '  hello world  ' (width 8)~%")
    (format t "Result: ~A~%" result))

  ;; Test consecutive spaces
  (let ((result (wrap-text "hello    world" 8)))
    (format t "Input: 'hello    world' (width 8)~%")
    (format t "Result: ~A~%" result))

  (format t "Whitespace handling tests completed~%"))

(defun test-performance-edge-cases ()
  "Test performance and edge cases with very long text"
  (format t "~%Test 5 - Performance edge cases:~%")

  ;; Test very long line
  (let* ((long-word (make-string 100 :initial-element #\a))
         (result (wrap-text long-word 10)))
    (format t "Input: 100 'a' characters (width 10)~%")
    (format t "Result length: ~A lines~%" (length result))
    (assert (= (length result) 10)))

  ;; Test many short words
  (let* ((many-words (format nil "~{~A~^ ~}" (loop for i from 1 to 50 collect "word")))
         (result (wrap-text many-words 20)))
    (format t "Input: 50 'word' separated by spaces (width 20)~%")
    (format t "Result length: ~A lines~%" (length result))
    (format t "First few lines: ~A~%" (subseq result 0 (min 3 (length result)))))

  (format t "Performance edge case tests completed~%"))

(defun test-wrap-text-comprehensive ()
  "Run comprehensive tests showing current behavior and identifying newline issue"
  (format t "~%=== COMPREHENSIVE WRAP-TEXT TESTS ===~%")
  (format t "Testing current implementation and documenting newline behavior~%")

  (test-basic-wrapping)
  (test-edge-cases)
  (test-text-with-newlines)
  (test-whitespace-handling)
  (test-performance-edge-cases)

  (format t "~%=== SUMMARY ===~%")
  (format t "Current wrap-text function works well for basic text wrapping.~%")
  (format t "ISSUE IDENTIFIED: Does not handle newlines properly.~%")
  (format t "- Newlines are treated as regular characters~%")
  (format t "- Text with embedded newlines doesn't break at newline boundaries~%")
  (format t "- For proper text formatting, newlines should be split first~%")
  (format t "~%All tests completed successfully!~%"))

(defun run-all-wrap-text-tests ()
  "Run all wrap-text tests"
  (format t "~%Running wrap-text tests...~%")
  (test-wrap-text-comprehensive)
  (format t "~%Wrap-text Tests: All tests completed~%")
  t)