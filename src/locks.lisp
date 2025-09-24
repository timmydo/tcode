(in-package :tcode)

(defun make-lock-with-logging (name)
  "Create a mutex lock with logging"
  (when *log-stream*
    (log-debug "Creating lock: ~A" name))
  (bt:make-lock name))

(defun make-thread-with-logging (function &key name initial-bindings)
  "Create a thread with logging"
  (when *log-stream*
    (log-info "Creating thread: ~A" (or name "unnamed")))
  (bt:make-thread function :name name :initial-bindings initial-bindings))

(defun join-thread-with-logging (thread)
  "Join a thread with logging"
  (when thread
    (when *log-stream*
      (log-debug "Joining thread: ~A" (bt:thread-name thread)))
    (prog1 (bt:join-thread thread)
      (when *log-stream*
        (log-debug "Thread joined: ~A" (bt:thread-name thread))))))

(defun thread-name (thread)
  "Get thread name - wrapper for consistency"
  (bt:thread-name thread))