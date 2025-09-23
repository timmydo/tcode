(in-package :tcode)

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
  "Dispatch command to OpenRouter API with streaming response"
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
             (response-stream nil)
             (accumulated-response ""))

        (setf response-stream
              (drakma:http-request "https://openrouter.ai/api/v1/chat/completions"
                                   :method :post
                                   :content payload
                                   :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" api-key))
                                                        ("Content-Type" . "application/json"))
                                   :stream t
                                   :want-stream t))

        (when response-stream
          (unwind-protect
              (loop for line = (read-line response-stream nil nil)
                    while line
                    do (when (and (> (length line) 6)
                                  (string= (subseq line 0 6) "data: "))
                         (let ((data-part (subseq line 6)))
                           (unless (string= data-part "[DONE]")
                             (handler-case
                                 (let* ((json-data (jsown:parse data-part))
                                        (choices (jsown:val json-data "choices"))
                                        (first-choice (and (> (length choices) 0) (aref choices 0)))
                                        (delta (and first-choice (jsown:val first-choice "delta")))
                                        (content (and delta (jsown:val delta "content"))))
                                   (when content
                                     (setf accumulated-response (concatenate 'string accumulated-response content))))
                               (error (e)
                                 (declare (ignore e))))))))
            (when response-stream
              (close response-stream))))

        accumulated-response)
    (error (e)
      (format nil "Backend error: ~A" e))))