;;;; shell.lisp - Persistent POSIX shell tool

(in-package :tcode)

(defvar *shell-process* nil
  "Global persistent shell process")

(defvar *shell-input-stream* nil
  "Input stream to shell process")

(defvar *shell-output-stream* nil
  "Output stream from shell process")

(defvar *shell-mutex* nil
  "Mutex for thread-safe shell access")

(defun start-shell ()
  "Start a persistent bash shell process"
  (when *shell-process*
    (log-info "Shell already running")
    (return-from start-shell t))

  (handler-case
      (progn
        ;; Create mutex if not exists
        (unless *shell-mutex*
          (setf *shell-mutex* (bt:make-lock "shell-lock")))

        ;; Start bash with unbuffered I/O
        (let ((process (sb-ext:run-program "/usr/bin/env"
                                           '("bash" "--norc" "--noprofile")
                                           :input :stream
                                           :output :stream
                                           :error :output
                                           :wait nil
                                           :search t)))
          (setf *shell-process* process
                *shell-input-stream* (sb-ext:process-input process)
                *shell-output-stream* (sb-ext:process-output process))

          ;; Set up shell with markers for command completion
          (write-line "PS1=''" *shell-input-stream*)
          (write-line "PS2=''" *shell-input-stream*)
          (force-output *shell-input-stream*)

          ;; Drain initial output
          (sleep 0.1)
          (clear-output-buffer)

          (log-info "Shell started successfully")
          t))
    (error (e)
      (log-error "Failed to start shell: ~A" e)
      nil)))

(defun clear-output-buffer ()
  "Clear any pending output from the shell"
  (loop while (listen *shell-output-stream*)
        do (read-char-no-hang *shell-output-stream*)))

(defun execute-in-shell (command &key (timeout 30))
  "Execute a command in the persistent shell and return the output.
   Timeout is in seconds (default 30)."
  (unless *shell-process*
    (start-shell))

  (bt:with-lock-held (*shell-mutex*)
    (handler-case
        (let* ((marker (format nil "TCODE_END_~A" (get-universal-time)))
               (full-command (format nil "~A~%echo '~A'~%" command marker))
               (output (make-array 0 :element-type 'character
                                  :fill-pointer 0
                                  :adjustable t))
               (start-time (get-universal-time)))

          ;; Send command with marker
          (write-string full-command *shell-input-stream*)
          (force-output *shell-input-stream*)

          ;; Read output until marker or timeout
          (loop
            (when (> (- (get-universal-time) start-time) timeout)
              (return-from execute-in-shell
                (format nil "Command timed out after ~A seconds~%~A" timeout output)))

            (when (listen *shell-output-stream*)
              (let ((line (read-line *shell-output-stream* nil nil)))
                (when (null line)
                  (return))

                ;; Check if we hit the marker
                (if (search marker line)
                    (return)
                    (progn
                      (when (> (length output) 0)
                        (vector-push-extend #\Newline output))
                      (loop for char across line
                            do (vector-push-extend char output))))))

            ;; Small sleep to avoid busy waiting
            (sleep 0.01))

          ;; Return output as string
          (if (> (length output) 0)
              (coerce output 'string)
              ""))
      (error (e)
        (format nil "Shell execution error: ~A" e)))))

(defun shell-tool-handler (arguments)
  "Handler function for the shell tool. Arguments is an alist with 'command' key."
  (let ((command (cdr (assoc "command" arguments :test #'string=))))
    (if command
        (execute-in-shell command)
        "Error: No command provided")))

(defun register-shell-tool ()
  "Register the shell tool with the tool registry"
  (register-tool
   "shell"
   "Execute a command in a persistent POSIX shell (bash). The shell maintains state between commands, so you can cd to directories, set environment variables, etc. Returns the command output."
   (jsown:new-js
     ("type" "object")
     ("properties" (jsown:new-js
                     ("command" (jsown:new-js
                                  ("type" "string")
                                  ("description" "The shell command to execute")))))
     ("required" (vector "command")))
   #'shell-tool-handler))

;; Auto-register shell tool on load
(register-shell-tool)
