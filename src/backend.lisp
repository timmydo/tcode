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
  (:documentation "Dispatch a command to the backend and stream the response"))

(defmethod dispatch-command ((backend openrouter-connection) input-string repl-context)
  "Dispatch command to OpenRouter API with streaming response in background thread"
  ;; Set state to waiting
  (setf (repl-context-state repl-context) :waiting-for-command
        (repl-context-status-message repl-context) "Waiting for response...")

  ;; Create background thread for HTTP request
  (let ((thread (make-thread-with-logging
                  (lambda ()
                    (handler-case
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
                                           ("stream" t))))
                               (accumulated-response ""))

                          ;; Make HTTP request
                          (let ((stream (drakma:http-request "https://openrouter.ai/api/v1/chat/completions"
                                                            :method :post
                                                            :content payload
                                                            :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" api-key))
                                                                                 ("Content-Type" . "application/json"))
                                                            :want-stream t)))

                            ;; Store stream for cancellation
                            (setf (repl-context-current-stream repl-context) stream)

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
                                                  (handler-case
                                                      (let* ((parsed (jsown:parse json-data))
                                                             (choices (jsown:val parsed "choices"))
                                                             (first-choice (if (vectorp choices) (aref choices 0) (first choices)))
                                                             (delta (jsown:val first-choice "delta"))
                                                             (content (jsown:val delta "content")))

                                                        (when content
                                                          (setf accumulated-response (concatenate 'string accumulated-response content))
                                                          ;; Thread-safe update to history
                                                          (bt:with-lock-held ((repl-context-history-mutex repl-context))
                                                            (when (repl-context-history repl-context)
                                                              (setf (history-item-result (first (repl-context-history repl-context)))
                                                                    accumulated-response)))))

                                                    (error (parse-err)
                                                      ;; Continue processing even if one chunk fails
                                                      (declare (ignore parse-err))
                                                      nil)))))))
                              (error (e)
                                (ignore-errors (close stream))
                                (bt:with-lock-held ((repl-context-history-mutex repl-context))
                                  (when (repl-context-history repl-context)
                                    (setf (history-item-result (first (repl-context-history repl-context)))
                                          (format nil "Stream processing error: ~A" e))))))

                            (ignore-errors (close stream))

                            ;; Final result update
                            (bt:with-lock-held ((repl-context-history-mutex repl-context))
                              (when (repl-context-history repl-context)
                                (setf (history-item-result (first (repl-context-history repl-context)))
                                      accumulated-response)))))
                      (error (e)
                        (bt:with-lock-held ((repl-context-history-mutex repl-context))
                          (when (repl-context-history repl-context)
                            (setf (history-item-result (first (repl-context-history repl-context)))
                                  (format nil "Backend error: ~A" e))))))

                    ;; Reset state when done
                    (setf (repl-context-state repl-context) :normal
                          (repl-context-current-thread repl-context) nil
                          (repl-context-current-stream repl-context) nil
                          (repl-context-status-message repl-context) ""))
                  :name "http-request-thread")))

    ;; Store thread reference
    (setf (repl-context-current-thread repl-context) thread)

    ;; Return immediately with placeholder
    "Processing request in background..."))
