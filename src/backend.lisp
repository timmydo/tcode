(in-package :tcode)

(defvar *tcode-backend* nil
  "Global backend connection instance")

(defvar *openrouter-default-model* "x-ai/grok-4-fast"
  "Default model for OpenRouter API")

(defclass backend-connection ()
  ()
  (:documentation "Base class for backend connections"))

(defclass openrouter-connection (backend-connection)
  ((api-key
    :initarg :api-key
    :accessor openrouter-api-key
    :documentation "OpenRouter API key"))
  (:documentation "OpenRouter API connection"))

(defclass tool-backend ()
  ((host
    :initarg :host
    :accessor tool-backend-host
    :initform "127.0.0.1"
    :documentation "JSONRPC server host")
   (port
    :initarg :port
    :accessor tool-backend-port
    :initform 9876
    :documentation "JSONRPC server port"))
  (:documentation "JSONRPC client for tool execution"))

(defun make-openrouter-connection (api-key)
  "Create a new OpenRouter connection with the given API key"
  (make-instance 'openrouter-connection :api-key api-key))

(defun make-tool-backend (&key (host "127.0.0.1") (port 9876))
  "Create a new tool backend JSONRPC client"
  (make-instance 'tool-backend :host host :port port))

(defun call-tool-via-backend (backend tool-name arguments)
  "Call a tool via the JSONRPC backend. Arguments should be an alist."
  (handler-case
      (let* ((host (tool-backend-host backend))
             (port (tool-backend-port backend))
             ;; Convert alist to jsown object
             (args-obj (let ((obj (jsown:new-js)))
                        (dolist (pair arguments)
                          (setf (jsown:val obj (car pair)) (cdr pair)))
                        obj))
             (request (jsown:to-json
                       (jsown:new-js
                         ("jsonrpc" "2.0")
                         ("method" "executeTool")
                         ("params" (jsown:new-js
                                     ("name" tool-name)
                                     ("arguments" args-obj)))
                         ("id" 1))))
             ;; Create socket connection
             (socket (usocket:socket-connect host port :element-type 'character))
             (stream (usocket:socket-stream socket)))

        (log-debug "Sending JSONRPC request: ~A" request)

        ;; Send request
        (write-line request stream)
        (force-output stream)

        ;; Read response
        (let* ((response-line (read-line stream nil nil))
               (response (jsown:parse response-line)))

          (log-debug "Received JSONRPC response: ~A" response-line)

          ;; Close socket
          (usocket:socket-close socket)

          ;; Check for error
          (let ((error-obj (ignore-errors (jsown:val response "error"))))
            (if error-obj
                (format nil "Tool backend error: ~A"
                        (jsown:val error-obj "message"))
                (jsown:val response "result")))))

    (error (e)
      (format nil "Tool backend connection error: ~A" e))))

(defgeneric dispatch-command (backend input-string repl-context)
  (:documentation "Dispatch a command to the backend, create history item, and stream the response"))

(defgeneric send-request (backend input-string repl-context history-item &optional additional-messages)
  (:documentation "Send HTTP request to backend and process streaming response"))

(defun process-streaming-chunk (json-data accumulated-response repl-context history-item tool-calls-acc)
  "Parse and process a single streaming chunk, returning updated accumulated response and tool calls.
   Returns (values new-accumulated-response new-tool-calls-acc)"
  (handler-case
      (let* ((parsed (jsown:parse json-data))
             (choices (jsown:val parsed "choices"))
             (first-choice (if (vectorp choices) (aref choices 0) (first choices)))
             (delta (jsown:val first-choice "delta"))
             (content (jsown:val delta "content"))
             (tool-calls-delta (ignore-errors (jsown:val delta "tool_calls")))
             (usage (ignore-errors (jsown:val parsed "usage"))))

        ;; Capture usage data if present (typically in final chunk)
        (when usage
          (bt:with-lock-held ((repl-context-mutex repl-context))
            (setf (history-item-usage history-item) usage)
            (log-debug "Captured usage data: ~A" usage))
          ;; Broadcast full history update when usage data is received
          (when (boundp '*sse-clients*)
            (broadcast-history-update)))

        ;; Handle tool calls accumulation
        (when tool-calls-delta
          (log-debug "Tool calls delta: ~A" tool-calls-delta)
          ;; Merge tool calls (OpenRouter sends them incrementally)
          (let ((tc-list (if (vectorp tool-calls-delta)
                             (coerce tool-calls-delta 'list)
                             (if (listp tool-calls-delta)
                                 tool-calls-delta
                                 (list tool-calls-delta)))))
            (dolist (tc-delta tc-list)
              (let ((index (jsown:val tc-delta "index")))
                ;; Ensure we have a slot for this index
                (when (>= index (length tool-calls-acc))
                  (setf tool-calls-acc (adjust-array tool-calls-acc (1+ index) :initial-element nil)))
                ;; Merge the delta into the accumulator
                (let ((existing (aref tool-calls-acc index))
                      (tc-id (ignore-errors (jsown:val tc-delta "id")))
                      (tc-type (ignore-errors (jsown:val tc-delta "type")))
                      (tc-function (ignore-errors (jsown:val tc-delta "function"))))
                  (if existing
                      ;; Merge into existing
                      (when tc-function
                        (let ((func-name (ignore-errors (jsown:val tc-function "name")))
                              (func-args (ignore-errors (jsown:val tc-function "arguments")))
                              (existing-func (jsown:val existing "function")))
                          (when func-name
                            (setf (jsown:val existing-func "name") func-name))
                          (when func-args
                            (setf (jsown:val existing-func "arguments")
                                  (concatenate 'string
                                               (or (jsown:val existing-func "arguments") "")
                                               func-args)))))
                      ;; Create new tool call entry
                      (setf (aref tool-calls-acc index)
                            (jsown:new-js
                              ("id" (or tc-id ""))
                              ("type" (or tc-type "function"))
                              ("function" (jsown:new-js
                                            ("name" (or (ignore-errors (jsown:val tc-function "name")) ""))
                                            ("arguments" (or (ignore-errors (jsown:val tc-function "arguments")) ""))))))))))))

        (if content
            (let ((new-accumulated (concatenate 'string accumulated-response content)))
	      ;; (log-debug "New content: '~A'" content)
              ;; Thread-safe update to history item
              (bt:with-lock-held ((repl-context-mutex repl-context))
                (setf (history-item-result history-item) new-accumulated))
              ;; Broadcast incremental update to web clients (only if non-empty)
              (when (and (boundp '*sse-clients*)
                         (> (length new-accumulated) 0))
                (broadcast-incremental-update (history-item-command history-item) new-accumulated))
              (values new-accumulated tool-calls-acc))
            (values accumulated-response tool-calls-acc)))
    (error (parse-err)
      ;; Continue processing even if one chunk fails
      (log-debug "Error processing chunk: ~A" parse-err)
      (values accumulated-response tool-calls-acc))))

(defun read-streaming-response (stream repl-context history-item)
  "Read and process streaming response from the HTTP stream.
   Returns (values accumulated-response tool-calls)"
  (let ((accumulated-response "")
        (tool-calls-acc (make-array 0 :adjustable t :initial-element nil)))
    (handler-case
        (loop for line = (read-line stream nil nil)
              while line do
                (progn
                  ;; Skip empty lines and non-data lines
                  (when (and (> (length line) 0)
                             (string= "data: " (subseq line 0 (min 6 (length line)))))

                    (let ((json-data (subseq line 6))) ; Remove "data: " prefix

                      ;; Log the actual JSON response data
                      ;; (log-debug "Raw JSON chunk: ~A" json-data)

                      ;; Check for end of stream
                      (if (string= json-data "[DONE]")
                          (return (values accumulated-response tool-calls-acc))

                          ;; Parse and process the chunk, capturing usage data and tool calls
                          (multiple-value-setq (accumulated-response tool-calls-acc)
                            (process-streaming-chunk json-data accumulated-response repl-context history-item tool-calls-acc)))))))
      (error (e)
        (ignore-errors (close stream))
        (bt:with-lock-held ((repl-context-mutex repl-context))
          (setf (history-item-result history-item)
                (format nil "Stream processing error: ~A" e))
          (log-error "Stream processing error: ~A" e))))
    (values accumulated-response tool-calls-acc)))

(defun execute-tool-calls (tool-calls repl-context)
  "Execute tool calls via JSONRPC backend and return results as a list of (tool-call-id . result-string) pairs."
  (let ((results '())
        (backend (repl-context-tool-backend repl-context)))
    (unless backend
      (error "No tool backend available in repl-context"))
    (loop for tc across tool-calls
          when tc
          do (let* ((tc-id (jsown:val tc "id"))
                    (tc-function (jsown:val tc "function"))
                    (func-name (jsown:val tc-function "name"))
                    (func-args-json (jsown:val tc-function "arguments"))
                    (func-args (handler-case
                                   (jsown:parse func-args-json)
                                 (error (e)
                                   (log-error "Failed to parse tool arguments: ~A" e)
                                   nil))))
               (log-info "Executing tool: ~A with args: ~A" func-name func-args-json)
               ;; Convert jsown object to alist for tool handler
               (let* ((args-alist (when func-args
                                   (let ((alist '()))
                                     (jsown:do-json-keys (key val) func-args
                                       (push (cons key val) alist))
                                     alist)))
                      ;; Call via JSONRPC backend
                      (result (call-tool-via-backend backend func-name args-alist)))
                 (log-info "Tool ~A result: ~A" func-name result)
                 (push (cons tc-id result) results))))
    (nreverse results)))

(defun continue-with-tool-results (backend input-string repl-context history-item tool-calls accumulated-response)
  "Execute tool calls and continue the conversation with results"
  (log-info "Processing ~A tool call(s)" (length tool-calls))

  ;; Execute tools (now passing repl-context for tool-backend access)
  (let ((tool-results (execute-tool-calls tool-calls repl-context)))

    ;; Build continuation messages with assistant's tool calls + tool results
    (let ((continuation-messages '()))
      ;; Add assistant message with tool calls
      (push (jsown:new-js
              ("role" "assistant")
              ("content" (or accumulated-response ""))
              ("tool_calls" tool-calls))
            continuation-messages)

      ;; Add tool result messages
      (dolist (result-pair tool-results)
        (push (jsown:new-js
                ("role" "tool")
                ("tool_call_id" (car result-pair))
                ("content" (cdr result-pair)))
              continuation-messages))

      ;; Update history item with tool call info
      (bt:with-lock-held ((repl-context-mutex repl-context))
        (setf (history-item-result history-item)
              (format nil "~A~%[Executed ~A tool call(s)]"
                      accumulated-response
                      (length tool-calls))))

      ;; Broadcast update
      (when (boundp '*sse-clients*)
        (broadcast-history-update))

      ;; Call backend with tool results
      (send-request backend input-string repl-context history-item
                    (nreverse continuation-messages)))))

(defmethod send-request ((backend openrouter-connection) input-string repl-context history-item &optional additional-messages)
  "Send HTTP request to OpenRouter API and process streaming response.
   If tool calls are made, automatically executes them and continues the conversation."
  (let* ((api-key (openrouter-api-key backend))
         (model *openrouter-default-model*)
         ;; Build messages vector from history
         (messages-list (bt:with-lock-held ((repl-context-mutex repl-context))
                          (let ((messages '()))
                            ;; Add previous history items (in reverse order since history is newest-first)
                            (dolist (item (reverse (repl-context-history repl-context)))
                              (when (and (history-item-command item)
                                        (history-item-result item)
                                        (stringp (history-item-result item))
                                        (> (length (history-item-result item)) 0))
                                ;; Add user message
                                (push (jsown:new-js ("role" "user") ("content" (history-item-command item))) messages)
                                ;; Add assistant response
                                (push (jsown:new-js ("role" "assistant") ("content" (history-item-result item))) messages)))
                            ;; Add current user input (unless we're continuing with tool results)
                            (unless additional-messages
                              (push (jsown:new-js ("role" "user") ("content" input-string)) messages))
                            ;; Add any additional messages (e.g., assistant tool calls + tool results)
                            (when additional-messages
                              (dolist (msg additional-messages)
                                (push msg messages)))
                            (nreverse messages))))
         (messages-vector (make-array (length messages-list) :initial-contents messages-list))
         ;; Get registered tools in OpenRouter format
         (tools (tools-to-openrouter-format))
         (payload-js (jsown:new-js
                       ("model" model)
                       ("messages" messages-vector)
                       ("stream" t)
                       ("usage" (jsown:new-js ("include" t)))))
         ;; Add tools to payload if available
         (payload (jsown:to-json
                   (if tools
                       (progn
                         (setf (jsown:val payload-js "tools") tools)
                         payload-js)
                       payload-js))))

    (log-debug "Sending HTTP request to OpenRouter API with model: ~A" model)
    (log-debug "Payload with ~A messages and ~A tools: ~A"
               (length messages-vector)
               (if tools (length tools) 0)
               payload)

    ;; Make HTTP request
    (multiple-value-bind (stream status-code)
        (drakma:http-request "https://openrouter.ai/api/v1/chat/completions"
                             :method :post
                             :content payload
                             :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" api-key))
                                                  ("Content-Type" . "application/json"))
                             :want-stream t
                             :external-format-in :utf-8)

      (log-debug "HTTP request initiated with status code: ~A, processing streaming response" status-code)

      ;; Log error response body for non-2xx status codes
      (when (>= status-code 400)
        (let ((error-body (with-output-to-string (out)
                           (loop for line = (read-line stream nil nil)
                                 while line
                                 do (write-line line out)))))
          (log-error "HTTP error ~A response body: ~A" status-code error-body)
          (ignore-errors (close stream))
          (bt:with-lock-held ((repl-context-mutex repl-context))
            (setf (history-item-result history-item)
                  (format nil "HTTP error ~A: ~A" status-code error-body)))
          (return-from send-request)))

      (unless additional-messages
        (log-debug "Creating history item for command: ~A" input-string)
        (bt:with-lock-held ((repl-context-mutex repl-context))
          (push history-item (repl-context-history repl-context)))

        ;; Send initial history update to web clients before streaming
        (when (boundp '*sse-clients*)
          (broadcast-history-update)))

      ;; Store stream for cancellation
      (setf (repl-context-current-stream repl-context) stream)

      ;; Read and process the streaming response
      (multiple-value-bind (accumulated-response tool-calls)
          (read-streaming-response stream repl-context history-item)
        (ignore-errors (close stream))
        (log-debug "Streaming response complete, final result length: ~A, tool calls: ~A"
                   (length accumulated-response)
                   (length tool-calls))

        ;; Check if we have tool calls to execute
        (if (and tool-calls (> (length tool-calls) 0) (aref tool-calls 0))
            (progn
              (log-info "Received ~A tool call(s), auto-executing" (length tool-calls))
              ;; Auto-execute tool calls
              (continue-with-tool-results backend input-string repl-context history-item
                                         tool-calls accumulated-response))

            ;; No tool calls - final result update
            (progn
              (bt:with-lock-held ((repl-context-mutex repl-context))
                (setf (history-item-result history-item) accumulated-response)
                (log-debug "Final history item result updated"))))))))

(defmethod dispatch-command ((backend openrouter-connection) input-string repl-context)
  "Dispatch command to OpenRouter API with streaming response in background thread"
  ;; Create history item first and store it in variable
  (let* ((initial-result "Processing request in background...")
         (history-item (make-history-item :command input-string :result initial-result)))

    (log-debug "History item added to repl context")

    ;; Set state to waiting
    (setf (repl-context-state repl-context) :waiting-for-command)

    ;; Create background thread for HTTP request
    (let ((thread (make-thread-with-logging
                    (lambda ()
                      (handler-case
                          (send-request backend input-string repl-context history-item)
                        (error (e)
                          (bt:with-lock-held ((repl-context-mutex repl-context))
                            (setf (history-item-result history-item)
                                  (format nil "Backend error: ~A" e))
                            (log-error "Backend error: ~A" e))))

                      ;; Reset state when done (unless waiting for tool approval)
                      (unless (eq (repl-context-state repl-context) :waiting-for-tool-approval)
                        (setf (repl-context-state repl-context) :normal))
                      (setf (repl-context-current-thread repl-context) nil
                            (repl-context-current-stream repl-context) nil))
                    :name "http-request-thread")))

      ;; Store thread reference
      (setf (repl-context-current-thread repl-context) thread)

      ;; Return immediately with placeholder
      initial-result)))
