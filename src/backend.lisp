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
             (content (jsown:val delta "content")))

        (if content
            (let ((new-accumulated (concatenate 'string accumulated-response content)))
              ;; Thread-safe update to history item
              (bt:with-lock-held ((repl-context-mutex repl-context))
                (setf (history-item-result history-item) new-accumulated)
                (log-debug "Updated history item result: ~A" new-accumulated))
              ;; Trigger display repaint when new content comes in
              (when (repl-context-repaint-callback repl-context)
                (log-debug "Calling repaint callback")
                (funcall (repl-context-repaint-callback repl-context)))
              new-accumulated)
            accumulated-response))
    (error (parse-err)
      ;; Continue processing even if one chunk fails
      (declare (ignore parse-err))
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

                      ;; Check for end of stream
                      (if (string= json-data "[DONE]")
                          (return accumulated-response)

                          ;; Parse and process the chunk
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
         (payload (jsown:to-json
                   (jsown:new-js
                     ("model" model)
                     ("messages" (vector (jsown:new-js
                                           ("role" "user")
                                           ("content" input-string))))
                     ("stream" t)))))

    (log-debug "Sending HTTP request to OpenRouter API with model: ~A" model)

    ;; Make HTTP request
    (let ((stream (drakma:http-request "https://openrouter.ai/api/v1/chat/completions"
                                      :method :post
                                      :content payload
                                      :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" api-key))
                                                           ("Content-Type" . "application/json"))
                                      :want-stream t)))

      (log-debug "HTTP request initiated, processing streaming response")

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

    (log-debug "Creating history item for command: ~A" input-string)
    (bt:with-lock-held ((repl-context-mutex repl-context))
      (push history-item (repl-context-history repl-context)))

    (log-debug "History item added to repl context")

    ;; Set state to waiting
    (setf (repl-context-state repl-context) :waiting-for-command
          (repl-context-status-message repl-context) "Waiting for response...")

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
                            (repl-context-current-stream repl-context) nil
                            (repl-context-status-message repl-context) ""))
                    :name "http-request-thread")))

      ;; Store thread reference
      (setf (repl-context-current-thread repl-context) thread)

      ;; Return immediately with placeholder
      initial-result)))
