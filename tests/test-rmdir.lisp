;;;; test-rmdir.lisp
;;;; Test the /rmdir command functionality

(require 'asdf)
(push #P"/home/timmy/src/tcode/" asdf:*central-registry*)
(asdf:load-system :tcode)

(in-package :tcode)

(defun test-rmdir-command ()
  "Test the /rmdir command functionality"
  (let ((ctx (make-repl-context :mutex (bt:make-lock))))
    ;; Initialize with current directory
    (setf (repl-context-context-directories ctx) (list "/tmp" "/home" "."))

    ;; Test removing an existing directory
    (let ((result (submit-command "/rmdir /tmp" ctx)))
      (format t "Test 1 - Remove existing directory: ~A~%" result)
      (format t "Directories after removal: ~A~%" (repl-context-context-directories ctx)))

    ;; Test removing a non-existent directory
    (let ((result (submit-command "/rmdir /nonexistent" ctx)))
      (format t "Test 2 - Remove non-existent directory: ~A~%" result))

    ;; Test removing with empty path
    (let ((result (submit-command "/rmdir " ctx)))
      (format t "Test 3 - Remove with empty path: ~A~%" result))

    ;; Check final state
    (format t "Final context state: ~A~%" (repl-context-state ctx))))

(defun run-all-rmdir-tests ()
  "Run all rmdir tests"
  (format t "~%Running rmdir tests...~%")
  (test-rmdir-command)
  (format t "~%Rmdir Tests: All tests completed~%")
  t)
