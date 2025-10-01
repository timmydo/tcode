(in-package :tcode)
(declaim (optimize (debug 3) (safety 3)))

(defun client (&optional test-mode)
  "Client entry point for tcode - creates a webview-based interface.
   If test-mode is true, exits immediately without webview operations for compile checking."
  ;; Early exit for test mode to allow compilation checking without webview operations
  (when test-mode
    (format t "Test mode: Compilation successful, exiting without webview operations.~%")
    (return-from client t))

  (handler-case
      (progn
        ;; Initialize logging system
        (initialize-logging)

        ;; Load configuration file
        (unless (load-config-file)
          ;; If config loading failed, exit
          (log-error "Failed to load configuration file")
          (cleanup-logging)
          (return-from client nil))

        ;; Create web UI and find available port
        (let ((ui (make-web-ui))
              (port (let ((env-port (uiop:getenv "TCODE_PORT")))
                      (if env-port
                          (parse-integer env-port :junk-allowed t)
                          (find-available-port)))))

          ;; Start the server in a background thread
          (bt:make-thread
           (lambda ()
             (run-web-ui ui port))
           :name "tcode-server")

          ;; Give the server a moment to start
          (sleep 1)

          ;; Launch webview pointing to the server
          (run-webview :title "tcode - Terminal-based CLI"
                       :url (format nil "http://127.0.0.1:~D" port)
                       :width 1200
                       :height 800)

          ;; Exit when webview closes
          (uiop:quit)))

    (error (e)
      (log-error "Error in main: ~A" e)
      (format t "Error in main: ~A~%" e)
      (cleanup-logging))))

(defun main (&optional test-mode)
  "Legacy entry point - calls client for backwards compatibility"
  (client test-mode))

(defun server (&key (port 9876))
  "Server entry point - starts JSONRPC tool server"
  (handler-case
      (progn
        ;; Initialize logging system
        (initialize-logging)

        (format t "Starting tcode JSONRPC server on port ~A~%" port)
        (log-info "Starting JSONRPC server on port ~A" port)

        ;; Start the JSONRPC server
        (start-jsonrpc-server :port port)

        (format t "JSONRPC server running. Press Ctrl+C to stop.~%")

        ;; Keep the main thread alive
        (loop
          (sleep 1)))

    (error (e)
      (log-error "Error in server: ~A" e)
      (format t "Error in server: ~A~%" e)
      (cleanup-logging))))