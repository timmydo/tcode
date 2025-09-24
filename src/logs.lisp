(in-package :tcode)

(defvar *log-stream* nil
  "The current log file stream")

(defvar *log-file-path* nil
  "Path to the current log file")

(defun get-logs-directory ()
  "Get the tcode logs directory path"
  (merge-pathnames "logs/" (get-config-directory)))

(defun ensure-logs-directory ()
  "Ensure the logs directory exists"
  (let ((logs-dir (get-logs-directory)))
    (unless (probe-file logs-dir)
      (ensure-directories-exist logs-dir))))

(defun generate-log-filename ()
  "Generate a timestamped log filename"
  (multiple-value-bind (sec min hour date month year)
      (get-decoded-time)
    (format nil "tcode-~4,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d.log"
            year month date hour min sec)))

(defun initialize-logging ()
  "Initialize logging system - creates logs directory and opens log file"
  (ensure-logs-directory)
  (let ((log-filename (generate-log-filename)))
    (setf *log-file-path* (merge-pathnames log-filename (get-logs-directory)))
    (setf *log-stream* (open *log-file-path*
                             :direction :output
                             :if-exists :append
                             :if-does-not-exist :create))
    (when *log-stream*
      (log-info "Logging initialized - tcode session started"))))

(defun cleanup-logging ()
  "Close the log file stream"
  (when *log-stream*
    (log-info "tcode session ended")
    (finish-output *log-stream*)
    (close *log-stream*)
    (setf *log-stream* nil
          *log-file-path* nil)))

(defun format-log-timestamp ()
  "Format current timestamp for log entries"
  (multiple-value-bind (sec min hour date month year)
      (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour min sec)))

(defun write-log-entry (level message)
  "Write a log entry with timestamp and level"
  (when *log-stream*
    (format *log-stream* "[~A] ~A: ~A~%"
            (format-log-timestamp)
            level
            message)
    (finish-output *log-stream*)))

(defun log-debug (message &rest args)
  "Log a debug message"
  (write-log-entry "DEBUG" (apply #'format nil message args)))

(defun log-info (message &rest args)
  "Log an info message"
  (write-log-entry "INFO" (apply #'format nil message args)))

(defun log-warn (message &rest args)
  "Log a warning message"
  (write-log-entry "WARN" (apply #'format nil message args)))

(defun log-error (message &rest args)
  "Log an error message"
  (write-log-entry "ERROR" (apply #'format nil message args)))

(defun log-command (command)
  "Log a user command"
  (log-info "Command executed: ~A" command))

(defun log-result (result)
  "Log a command result"
  (log-debug "Command result: ~A" result))