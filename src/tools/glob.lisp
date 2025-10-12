;;;; glob.lisp - File pattern matching tool

(in-package :tcode)

(defun glob-files (pattern &optional path)
  "Find files matching a glob pattern. Returns matching file paths sorted by modification time.
   Pattern uses glob syntax like **/*.js or src/**/*.ts.
   Path defaults to current working directory if not specified."
  (handler-case
      (let* ((search-path (or path (uiop:getcwd)))
             (full-pattern (if path
                               (merge-pathnames pattern path)
                               pattern))
             (matches (directory full-pattern)))
        (if matches
            ;; Sort by modification time (most recent first)
            (let ((sorted-matches (sort matches #'>
                                       :key #'file-write-date)))
              (format nil "~{~A~%~}"
                      (mapcar (lambda (p) (namestring p)) sorted-matches)))
            "No files match the pattern"))
    (error (e)
      (format nil "Error: ~A" e))))

(defun glob-tool-handler (arguments)
  "Handler function for the glob tool. Arguments is an alist with 'pattern' and optional 'path' keys."
  (let ((pattern (cdr (assoc "pattern" arguments :test #'string=)))
        (path (cdr (assoc "path" arguments :test #'string=))))

    (unless pattern
      (return-from glob-tool-handler "Error: No pattern provided"))

    (glob-files pattern path)))

(defun register-glob-tool ()
  "Register the glob tool with the tool registry"
  (register-tool
   "glob"
   "Fast file pattern matching tool that works with any codebase size. Supports glob patterns like \"**/*.js\" or \"src/**/*.ts\". Returns matching file paths sorted by modification time. Use this tool when you need to find files by name patterns. When you are doing an open ended search that may require multiple rounds of globbing and grepping, use the Agent tool instead. You have the capability to call multiple tools in a single response. It is always better to speculatively perform multiple searches as a batch that are potentially useful."
   (jsown:new-js
     ("type" "object")
     ("properties" (jsown:new-js
                     ("pattern" (jsown:new-js
                                  ("type" "string")
                                  ("description" "The glob pattern to match files against")))
                     ("path" (jsown:new-js
                               ("type" "string")
                               ("description" "The directory to search in. If not specified, the current working directory will be used. IMPORTANT: Omit this field to use the default directory. DO NOT enter \"undefined\" or \"null\" - simply omit it for the default behavior. Must be a valid directory path if provided.")))))
     ("required" (vector "pattern")))
   #'glob-tool-handler))

;; Auto-register glob tool on load
(register-glob-tool)
