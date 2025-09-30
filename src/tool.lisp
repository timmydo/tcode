;;;; tool.lisp - Tool registration and execution API

(in-package :tcode)

(defstruct tool
  "Represents a tool that can be called by the AI backend"
  (name "" :type string)
  (description "" :type string)
  (parameters nil)  ; JSON schema for parameters
  (handler nil :type function))

(defvar *registered-tools* (make-hash-table :test 'equal)
  "Global registry of available tools")

(defun register-tool (name description parameters handler)
  "Register a new tool with the given name, description, parameters schema, and handler function.
   Parameters should be a JSON schema object (as a plist or nested structure).
   Handler is a function that takes parsed arguments (as an alist) and returns a string result."
  (let ((tool (make-tool :name name
                         :description description
                         :parameters parameters
                         :handler handler)))
    (setf (gethash name *registered-tools*) tool)
    (log-info "Registered tool: ~A" name)
    tool))

(defun get-registered-tools ()
  "Return a list of all registered tools"
  (let ((tools '()))
    (maphash (lambda (name tool)
               (declare (ignore name))
               (push tool tools))
             *registered-tools*)
    (nreverse tools)))

(defun get-tool (name)
  "Get a tool by name, or NIL if not found"
  (gethash name *registered-tools*))

(defun call-tool (name arguments)
  "Call a tool by name with the given arguments (as an alist).
   Returns the tool result as a string, or an error message if the tool fails."
  (let ((tool (get-tool name)))
    (if tool
        (handler-case
            (funcall (tool-handler tool) arguments)
          (error (e)
            (format nil "Tool execution error: ~A" e)))
        (format nil "Tool not found: ~A" name))))

(defun tools-to-openrouter-format ()
  "Convert registered tools to OpenRouter API format"
  (let ((tools-list '()))
    (maphash (lambda (name tool)
               (declare (ignore name))
               (push (jsown:new-js
                       ("type" "function")
                       ("function" (jsown:new-js
                                     ("name" (tool-name tool))
                                     ("description" (tool-description tool))
                                     ("parameters" (tool-parameters tool)))))
                     tools-list))
             *registered-tools*)
    (if tools-list
        (make-array (length tools-list) :initial-contents (nreverse tools-list))
        nil)))