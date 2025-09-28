(in-package :tcode)

(defvar *tcode-backend* nil
  "Global backend connection instance")

(defvar *openrouter-default-model* "x-ai/grok-4-fast:free"
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

(defun make-openrouter-connection (api-key)
  "Create a new OpenRouter connection with the given API key"
  (make-instance 'openrouter-connection :api-key api-key))

(defgeneric dispatch-command (backend input-string repl-context)
  (:documentation "Dispatch a command to the backend, create history item, and stream the response"))

(defgeneric send-request (backend input-string repl-context history-item)
  (:documentation "Send HTTP request to backend and process streaming response"))

(defun process-streaming-chunk (json-data accumulated-response repl-context history-item)
  "Parse and process a single streaming chunk, returning updated accumulated response"
  (handler-case
      (let* ((parsed (jsown:parse json-data))
             (choices (jsown:val parsed "choices"))
             (first-choice (if (vectorp choices) (aref choices 0) (first choices)))
             (delta (jsown:val first-choice "delta"))
             (content (jsown:val delta "content"))
             (usage (ignore-errors (jsown:val parsed "usage"))))

        ;; Capture usage data if present (typically in final chunk)
        (when usage
          (bt:with-lock-held ((repl-context-mutex repl-context))
            (setf (history-item-usage history-item) usage)
            (log-debug "Captured usage data: ~A" usage))
          ;; Broadcast full history update when usage data is received
          (when (boundp '*sse-clients*)
            (broadcast-history-update)))

        (if content
            (let ((new-accumulated (concatenate 'string accumulated-response content)))
	      (log-debug "New content: '~A'" content)
              ;; Thread-safe update to history item
              (bt:with-lock-held ((repl-context-mutex repl-context))
                (setf (history-item-result history-item) new-accumulated))
              ;; Broadcast incremental update to web clients (only if non-empty)
              (when (and (boundp '*sse-clients*)
                         (> (length new-accumulated) 0))
                (broadcast-incremental-update (history-item-command history-item) new-accumulated))
              new-accumulated)
            accumulated-response))
    (error (parse-err)
      ;; Continue processing even if one chunk fails
      (log-debug "Error processing chunk: ~A" parse-err)
      accumulated-response)))

(defun read-streaming-response (stream repl-context history-item)
  "Read and process streaming response from the HTTP stream"
  (let ((accumulated-response ""))
    (handler-case
        (loop for line = (read-line stream nil nil)
              while line do
                (progn
                  ;; Skip empty lines and non-data lines
                  (when (and (> (length line) 0)
                             (string= "data: " (subseq line 0 (min 6 (length line)))))

                    (let ((json-data (subseq line 6))) ; Remove "data: " prefix

                      ;; Log the actual JSON response data
                      (log-debug "Raw JSON chunk: ~A" json-data)

                      ;; Check for end of stream
                      (if (string= json-data "[DONE]")
                          (return accumulated-response)

                          ;; Parse and process the chunk, capturing usage data
                          (setf accumulated-response
                                (process-streaming-chunk json-data accumulated-response repl-context history-item)))))))
      (error (e)
        (ignore-errors (close stream))
        (bt:with-lock-held ((repl-context-mutex repl-context))
          (setf (history-item-result history-item)
                (format nil "Stream processing error: ~A" e))
          (log-error "Stream processing error: ~A" e))))
    accumulated-response))

(defmethod send-request ((backend openrouter-connection) input-string repl-context history-item)
  "Send HTTP request to OpenRouter API and process streaming response"
  (let* ((api-key (openrouter-api-key backend))
         (model (if (boundp '*openrouter-default-model*)
                    *openrouter-default-model*
                    "x-ai/grok-4-fast:free"))
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
                            ;; Add current user input
                            (push (jsown:new-js ("role" "user") ("content" input-string)) messages)
                            (nreverse messages))))
         (messages-vector (make-array (length messages-list) :initial-contents messages-list))
         (payload (jsown:to-json
                   (jsown:new-js
                     ("model" model)
                     ("messages" messages-vector)
                     ("stream" t)
                     ("usage" (jsown:new-js ("include" t)))))))

    (log-debug "Sending HTTP request to OpenRouter API with model: ~A" model)
    (log-debug "Payload with ~A messages: ~A" (length messages-vector) payload)

    ;; Make HTTP request
    (let ((stream (drakma:http-request "https://openrouter.ai/api/v1/chat/completions"
                                      :method :post
                                      :content payload
                                      :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" api-key))
                                                           ("Content-Type" . "application/json"))
                                      :want-stream t
                                      :external-format-in :utf-8)))

      (log-debug "HTTP request initiated, processing streaming response")

      (log-debug "Creating history item for command: ~A" input-string)
      (bt:with-lock-held ((repl-context-mutex repl-context))
	(push history-item (repl-context-history repl-context)))

      ;; Send initial history update to web clients before streaming
      (when (boundp '*sse-clients*)
        (broadcast-history-update))

      ;; Store stream for cancellation
      (setf (repl-context-current-stream repl-context) stream)

      ;; Read and process the streaming response
      (let ((accumulated-response (read-streaming-response stream repl-context history-item)))
        (ignore-errors (close stream))
        (log-debug "Streaming response complete, final result length: ~A" (length accumulated-response))

        ;; Final result update
        (bt:with-lock-held ((repl-context-mutex repl-context))
          (setf (history-item-result history-item) accumulated-response)
          (log-debug "Final history item result updated"))))))

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

                      ;; Reset state when done
                      (setf (repl-context-state repl-context) :normal
                            (repl-context-current-thread repl-context) nil
                            (repl-context-current-stream repl-context) nil))
                    :name "http-request-thread")))

      ;; Store thread reference
      (setf (repl-context-current-thread repl-context) thread)

      ;; Return immediately with placeholder
      initial-result)))
