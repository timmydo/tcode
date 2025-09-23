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
)))
             (accumulated-response ""))

        (let ((raw-response (drakma:http-request "https://openrouter.ai/api/v1/chat/completions"
                                                :method :post
                                                :content payload
                                                :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" api-key))
                                                                     ("Content-Type" . "application/json")))))
          ;; Convert byte array to string and parse JSON response
          (handler-case
              (let* ((response-string (if (stringp raw-response)
                                          raw-response
                                          (flexi-streams:octets-to-string raw-response :external-format :utf-8)))
                     (json-response (jsown:parse response-string))
                     (choices (jsown:val json-response "choices")))
                (if (and choices (> (length choices) 0))
                    (jsown:val (jsown:val (aref choices 0) "message") "content")
                    "No response from API"))
            (error (e)
              (format nil "Response parsing error: ~A" e)))))
    (error (e)
      (format nil "Backend error: ~A" e))))