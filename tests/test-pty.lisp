;;;; test-pty.lisp
;;;; Tests for PTY functionality in tcode

(in-package :tcode)

(defvar *pty-test-results* '())

(defun record-pty-test (name passed &optional message)
  "Record a PTY test result"
  (push (list name passed message) *pty-test-results*)
  (if passed
      (format t "✓ ~A~%" name)
      (format t "✗ ~A: ~A~%" name (or message "Failed"))))

(defun run-pty-test (name test-fn)
  "Run a single PTY test and record the result"
  (handler-case
      (let ((result (funcall test-fn)))
        (record-pty-test name result))
    (error (e)
      (record-pty-test name nil (format nil "Error: ~A" e)))))

(defun test-pty-creation ()
  "Test PTY instance creation"
  (let ((pty (make-pty :width 80 :height 24)))
    (and (typep pty 'pty)
         (= (pty-width pty) 80)
         (= (pty-height pty) 24)
         (= (pty-master-fd pty) -1)
         (= (pty-slave-fd pty) -1)
         (null (pty-slave-name pty))
         (= (pty-child-pid pty) -1)
         (not (pty-running-p pty)))))

(defun test-pty-open-close ()
  "Test PTY opening and closing"
  (let ((pty (make-pty)))
    (unwind-protect
        (progn
          (open-pty pty)
          ;; Check that PTY was opened successfully
          (and (>= (pty-master-fd pty) 0)
               (stringp (pty-slave-name pty))
               (> (length (pty-slave-name pty)) 0)))
      ;; Always cleanup
      (close-pty pty))))

(defun test-pty-window-size ()
  "Test PTY window size setting"
  (let ((pty (make-pty :width 100 :height 30)))
    (unwind-protect
        (progn
          (open-pty pty)
          (set-pty-window-size pty 120 40)
          (and (= (pty-width pty) 120)
               (= (pty-height pty) 40)))
      (close-pty pty))))

(defun test-pty-write-string ()
  "Test writing strings to PTY"
  (let ((pty (make-pty)))
    (unwind-protect
        (progn
          (open-pty pty)
          ;; Test that write-string doesn't crash
          (pty-write-string pty "hello world\n")
          t)
      (close-pty pty))))

(defun test-pty-cleanup ()
  "Test PTY cleanup function"
  (let ((pty (make-pty)))
    (open-pty pty)
    (let ((original-fd (pty-master-fd pty)))
      (cleanup-pty pty)
      ;; After cleanup, PTY should be closed
      (and (= (pty-master-fd pty) -1)
           (= (pty-slave-fd pty) -1)
           (not (pty-running-p pty))))))

(defun test-pty-multiple-instances ()
  "Test creating multiple PTY instances"
  (let ((pty1 (make-pty :width 80 :height 24))
        (pty2 (make-pty :width 100 :height 30)))
    (unwind-protect
        (progn
          (open-pty pty1)
          (open-pty pty2)
          ;; Both should have different file descriptors
          (and (/= (pty-master-fd pty1) (pty-master-fd pty2))
               (not (string= (pty-slave-name pty1) (pty-slave-name pty2)))))
      (progn
        (close-pty pty1)
        (close-pty pty2)))))

(defun run-all-pty-tests ()
  "Run all PTY tests"
  (setf *pty-test-results* '())
  (format t "~%Running PTY functionality tests...~%")

  (run-pty-test "PTY creation" #'test-pty-creation)
  (run-pty-test "PTY open/close" #'test-pty-open-close)
  (run-pty-test "PTY window size" #'test-pty-window-size)
  (run-pty-test "PTY write string" #'test-pty-write-string)
  (run-pty-test "PTY cleanup" #'test-pty-cleanup)
  (run-pty-test "Multiple PTY instances" #'test-pty-multiple-instances)

  (let ((passed (count-if (lambda (result) (second result)) *pty-test-results*))
        (total (length *pty-test-results*)))
    (format t "~%PTY Tests: ~A/~A passed~%" passed total)
    (= passed total)))
