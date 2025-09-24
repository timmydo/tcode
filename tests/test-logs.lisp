(in-package :tcode)

(defun test-logging-initialization ()
  "Test that logging system can be initialized"
  (handler-case
      (progn
        (initialize-logging)
        (log-info "Test log message")
        (cleanup-logging)
        t)
    (error (e)
      (format t "ERROR: Logging initialization failed: ~A~%" e)
      nil)))

(defun test-log-functions ()
  "Test all logging functions"
  (handler-case
      (progn
        (initialize-logging)
        (log-debug "Debug message: ~A" "test")
        (log-info "Info message: ~A" "test")
        (log-warn "Warning message: ~A" "test")
        (log-error "Error message: ~A" "test")
        (log-command "(+ 1 2)")
        (log-result "3")
        (cleanup-logging)
        t)
    (error (e)
      (format t "ERROR: Log functions test failed: ~A~%" e)
      nil)))

(defun test-logs-directory-creation ()
  "Test that logs directory is created"
  (handler-case
      (progn
        (ensure-logs-directory)
        (probe-file (get-logs-directory)))
    (error (e)
      (format t "ERROR: Logs directory creation failed: ~A~%" e)
      nil)))

(defun run-all-logs-tests ()
  "Run all logging tests"
  (let ((tests '(("Logging initialization" . test-logging-initialization)
                 ("Log functions" . test-log-functions)
                 ("Logs directory creation" . test-logs-directory-creation)))
        (passed 0)
        (failed 0))

    (format t "Running logging tests...~%")

    (dolist (test tests)
      (format t "Testing ~A... " (car test))
      (if (funcall (cdr test))
          (progn
            (format t "PASS~%")
            (incf passed))
          (progn
            (format t "FAIL~%")
            (incf failed))))

    (format t "~%Logging tests complete: ~A passed, ~A failed~%" passed failed)
    (= failed 0)))