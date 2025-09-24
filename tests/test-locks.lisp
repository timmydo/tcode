(in-package :tcode)

(defun test-lock-creation ()
  "Test that locks can be created with logging"
  (handler-case
      (progn
        (initialize-logging)
        (let ((lock (make-lock-with-logging "test-lock")))
          (cleanup-logging)
          (and lock t)))
    (error (e)
      (format t "ERROR: Lock creation test failed: ~A~%" e)
      nil)))

(defun test-with-lock-held ()
  "Test that bt:with-lock-held works"
  (handler-case
      (progn
        (initialize-logging)
        (let ((lock (bt:make-lock "test-with-lock"))
              (result nil))
          (bt:with-lock-held ((lock))
            (setf result t))
          (cleanup-logging)
          result))
    (error (e)
      (format t "ERROR: with-lock-held test failed: ~A~%" e)
      nil)))

(defun test-thread-creation-and-join ()
  "Test thread creation and joining with logging"
  (handler-case
      (progn
        (initialize-logging)
        (let* ((result nil)
               (thread (make-thread-with-logging (lambda () (setf result 42)) :name "test-thread")))
          (join-thread-with-logging thread)
          (cleanup-logging)
          (= result 42)))
    (error (e)
      (format t "ERROR: Thread test failed: ~A~%" e)
      nil)))

(defun test-concurrent-lock-access ()
  "Test concurrent access to locks with logging"
  (handler-case
      (progn
        (initialize-logging)
        (let ((lock (bt:make-lock "concurrent-test"))
              (shared-counter 0)
              (threads '()))
          ;; Create multiple threads that increment a shared counter
          (dotimes (i 3)
            (push (bt:make-thread
                   (lambda ()
                     (dotimes (j 10)
                       (bt:with-lock-held ((lock))
                         (incf shared-counter))))
                   :name (format nil "worker-~A" i))
                  threads))
          ;; Wait for all threads to complete
          (mapc #'bt:join-thread threads)
          (cleanup-logging)
          ;; Should be 30 (3 threads * 10 increments each)
          (= shared-counter 30)))
    (error (e)
      (format t "ERROR: Concurrent lock test failed: ~A~%" e)
      nil)))

(defun run-all-locks-tests ()
  "Run all lock wrapper tests"
  (let ((tests '(("Lock creation" . test-lock-creation)
                 ("with-lock-held macro" . test-with-lock-held)
                 ("Thread creation and join" . test-thread-creation-and-join)
                 ("Concurrent lock access" . test-concurrent-lock-access)))
        (passed 0)
        (failed 0))

    (format t "Running lock wrapper tests...~%")

    (dolist (test tests)
      (format t "Testing ~A... " (car test))
      (if (funcall (cdr test))
          (progn
            (format t "PASS~%")
            (incf passed))
          (progn
            (format t "FAIL~%")
            (incf failed))))

    (format t "~%Lock wrapper tests complete: ~A passed, ~A failed~%" passed failed)
    (= failed 0)))