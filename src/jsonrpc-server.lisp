;;;; jsonrpc-server.lisp - JSONRPC 2.0 server for tool execution

(in-package :tcode)

(defvar *jsonrpc-server* nil
  "Global JSONRPC server instance")

(defvar *jsonrpc-port* 9876
  "Default port for JSONRPC server")

(defclass jsonrpc-server ()
  ((socket
    :accessor jsonrpc-socket
    :initform nil
    :documentation "Server socket")
   (port
    :initarg :port
    :accessor jsonrpc-port
    :initform 9876
    :documentation "Port the server listens on")
   (running
    :accessor jsonrpc-running
    :initform nil
    :documentation "Whether the server is running"))
  (:documentation "JSONRPC 2.0 server for tool execution"))

(defun make-jsonrpc-response (id result)
  "Create a JSONRPC 2.0 success response"
  (jsown:to-json
   (jsown:new-js
     ("jsonrpc" "2.0")
     ("id" id)
     ("result" result))))

(defun make-jsonrpc-error (id code message &optional data)
  "Create a JSONRPC 2.0 error response"
  (let ((error-obj (jsown:new-js
                     ("code" code)
                     ("message" message))))
    (when data
      (setf (jsown:val error-obj "data") data))
    (jsown:to-json
     (jsown:new-js
       ("jsonrpc" "2.0")
       ("id" id)
       ("error" error-obj)))))

(defun handle-jsonrpc-request (request-string)
  "Handle a JSONRPC request and return a response string"
  (handler-case
      (let* ((request (jsown:parse request-string))
             (jsonrpc-version (jsown:val request "jsonrpc"))
             (method (jsown:val request "method"))
             (params (ignore-errors (jsown:val request "params")))
             (id (ignore-errors (jsown:val request "id"))))

        ;; Validate JSONRPC version
        (unless (string= jsonrpc-version "2.0")
          (return-from handle-jsonrpc-request
            (make-jsonrpc-error id -32600 "Invalid JSONRPC version")))

        ;; Handle methods
        (cond
          ;; listTools - return all registered tools
          ((string= method "listTools")
           (let ((tools-list '()))
             (maphash (lambda (name tool)
                        (declare (ignore name))
                        (push (jsown:new-js
                                ("name" (tool-name tool))
                                ("description" (tool-description tool))
                                ("parameters" (tool-parameters tool)))
                              tools-list))
                      *registered-tools*)
             (make-jsonrpc-response id (nreverse tools-list))))

          ;; executeTool - execute a tool with given arguments
          ((string= method "executeTool")
           (unless params
             (return-from handle-jsonrpc-request
               (make-jsonrpc-error id -32602 "Missing parameters")))

           (let* ((tool-name (jsown:val params "name"))
                  (tool-args (jsown:val params "arguments"))
                  ;; Convert jsown object to alist for tool handler
                  (args-alist (when tool-args
                                (let ((alist '()))
                                  (jsown:do-json-keys (key val) tool-args
                                    (push (cons key val) alist))
                                  alist))))

             (log-info "JSONRPC executeTool: ~A with args: ~A" tool-name args-alist)

             (let ((result (call-tool tool-name args-alist)))
               (make-jsonrpc-response id result))))

          ;; Unknown method
          (t
           (make-jsonrpc-error id -32601 (format nil "Method not found: ~A" method)))))

    (error (e)
      (log-error "JSONRPC request handling error: ~A" e)
      (make-jsonrpc-error nil -32603 (format nil "Internal error: ~A" e)))))

(defun handle-client-connection (client-socket)
  "Handle a single client connection"
  (handler-case
      (let* ((stream (usocket:socket-stream client-socket))
             (request-line (read-line stream nil nil)))

        (when request-line
          (log-debug "JSONRPC request: ~A" request-line)

          (let ((response (handle-jsonrpc-request request-line)))
            (log-debug "JSONRPC response: ~A" response)

            ;; Write response with newline delimiter
            (write-line response stream)
            (force-output stream))))

    (error (e)
      (log-error "Error handling client connection: ~A" e)))

  ;; Close the client socket
  (ignore-errors (usocket:socket-close client-socket)))

(defun run-jsonrpc-server (server)
  "Run the JSONRPC server main loop"
  (handler-case
      (progn
        ;; Create server socket
        (let ((socket (usocket:socket-listen "127.0.0.1" (jsonrpc-port server)
                                            :reuse-address t
                                            :element-type 'character)))
          (setf (jsonrpc-socket server) socket)
          (setf (jsonrpc-running server) t)

          (log-info "JSONRPC server listening on port ~A" (jsonrpc-port server))

          ;; Accept connections loop
          (loop while (jsonrpc-running server)
                do (handler-case
                       (let ((client-socket (usocket:socket-accept socket :element-type 'character)))
                         (log-debug "JSONRPC client connected")

                         ;; Handle client in a new thread
                         (bt:make-thread
                          (lambda ()
                            (handle-client-connection client-socket))
                          :name "jsonrpc-client-handler"))

                     (error (e)
                       (log-error "Error accepting JSONRPC client: ~A" e)
                       (sleep 0.1))))))

    (error (e)
      (log-error "JSONRPC server error: ~A" e)))

  ;; Cleanup
  (when (jsonrpc-socket server)
    (ignore-errors (usocket:socket-close (jsonrpc-socket server))))
  (setf (jsonrpc-running server) nil))

(defun start-jsonrpc-server (&key (port 9876))
  "Start the JSONRPC server on the specified port"
  (when *jsonrpc-server*
    (log-warn "JSONRPC server already running")
    (return-from start-jsonrpc-server *jsonrpc-server*))

  (let ((server (make-instance 'jsonrpc-server :port port)))
    (setf *jsonrpc-server* server)

    ;; Run server in background thread
    (bt:make-thread
     (lambda ()
       (run-jsonrpc-server server))
     :name "jsonrpc-server")

    server))

(defun stop-jsonrpc-server ()
  "Stop the JSONRPC server"
  (when *jsonrpc-server*
    (setf (jsonrpc-running *jsonrpc-server*) nil)
    (when (jsonrpc-socket *jsonrpc-server*)
      (ignore-errors (usocket:socket-close (jsonrpc-socket *jsonrpc-server*))))
    (setf *jsonrpc-server* nil)
    (log-info "JSONRPC server stopped")))
