;;;; write.lisp - File writing tool

(in-package :tcode)

(defun write-file-content (file-path content)
  "Write content to a file, creating or overwriting it.
   Returns a success message or error description."
  (handler-case
      (progn
        ;; Ensure parent directory exists
        (let ((parent-dir (uiop:pathname-directory-pathname file-path)))
          (unless (probe-file parent-dir)
            (return-from write-file-content
              (format nil "Error: Parent directory does not exist: ~A" parent-dir))))

        ;; Write the file
        (with-open-file (stream file-path
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (write-string content stream))

        (format nil "Successfully wrote ~D bytes to ~A"
                (length content)
                file-path))
    (error (e)
      (format nil "Error writing file: ~A" e))))

(defun write-tool-handler (arguments)
  "Handler function for the write tool. Arguments is an alist with 'file_path' and 'content' keys."
  (let ((file-path (cdr (assoc "file_path" arguments :test #'string=)))
        (content (cdr (assoc "content" arguments :test #'string=))))

    (unless file-path
      (return-from write-tool-handler "Error: No file_path provided"))

    (unless content
      (return-from write-tool-handler "Error: No content provided"))

    (write-file-content file-path content)))

(defun register-write-tool ()
  "Register the write tool with the tool registry"
  (register-tool
   "write"
   "Writes a file to the local filesystem.

Usage:
- This tool will overwrite the existing file if there is one at the provided path.
- If this is an existing file, you MUST use the Read tool first to read the file's contents. This tool will fail if you did not read the file first.
- ALWAYS prefer editing existing files in the codebase. NEVER write new files unless explicitly required.
- NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.
- Only use emojis if the user explicitly requests it. Avoid writing emojis to files unless asked."
   (jsown:new-js
     ("type" "object")
     ("properties" (jsown:new-js
                     ("file_path" (jsown:new-js
                                    ("type" "string")
                                    ("description" "The absolute path to the file to write (must be absolute, not relative)")))
                     ("content" (jsown:new-js
                                  ("type" "string")
                                  ("description" "The content to write to the file")))))
     ("required" (vector "file_path" "content")))
   #'write-tool-handler))

;; Auto-register write tool on load
(register-write-tool)
