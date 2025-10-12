;;;; edit.lisp - File editing tool

(in-package :tcode)

(defun edit-file (file-path old-string new-string &key (replace-all nil))
  "Performs exact string replacements in files.

Usage:
- You must use the Read tool at least once before editing a file, otherwise the edit will fail
- Finds the old-string in the file and replaces it with new-string
- The old-string must be unique in the file, or the edit will fail. To handle duplicates, either:
  - Provide more surrounding context to make it unique
  - Use replace-all: true to replace every instance
- When editing text from Read tool output, ensure you preserve the exact indentation (tabs/spaces) as it appears AFTER the line number prefix
- The line number prefix format is: spaces + line number + tab. Everything after that tab is the actual file content to match
- Never include any part of the line number prefix in the old-string or new-string

Parameters:
- file-path (required): Absolute path to the file
- old-string (required): The exact text to replace
- new-string (required): The replacement text (must differ from old-string)
- replace-all (optional): If true, replaces all occurrences; defaults to false"

  (unless (probe-file file-path)
    (return-from edit-file
      (format nil "Error: File not found: ~A" file-path)))

  (unless (uiop:file-exists-p file-path)
    (return-from edit-file
      (format nil "Error: Path is not a file: ~A" file-path)))

  (when (string= old-string new-string)
    (return-from edit-file
      (format nil "Error: old_string and new_string must be different")))

  (handler-case
      (let ((content (uiop:read-file-string file-path)))

        ;; Count occurrences
        (let ((count 0)
              (pos 0))
          (loop while (setf pos (search old-string content :start2 pos))
                do (incf count)
                   (incf pos (length old-string)))

          (cond
            ((= count 0)
             (return-from edit-file
               (format nil "Error: old_string not found in file")))

            ((and (> count 1) (not replace-all))
             (return-from edit-file
               (format nil "Error: old_string appears ~D times in file. Either provide more context to make it unique, or use replace_all: true" count)))

            (t
             ;; Perform replacement
             (let* ((new-content (if replace-all
                                    (loop with result = content
                                          while (search old-string result)
                                          do (setf result (uiop:frob-substrings result (list (cons old-string new-string))))
                                          finally (return result))
                                    (let ((pos (search old-string content)))
                                      (concatenate 'string
                                                   (subseq content 0 pos)
                                                   new-string
                                                   (subseq content (+ pos (length old-string))))))))

               ;; Write back to file
               (with-open-file (stream file-path
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                 (write-string new-content stream))

               (format nil "Successfully replaced ~D occurrence~:P" count))))))
    (error (e)
      (format nil "Error editing file: ~A" e))))

(defun edit-tool-handler (arguments)
  "Handler function for the edit tool. Arguments is an alist with 'file_path', 'old_string', 'new_string', and optional 'replace_all' keys."
  (let ((file-path (cdr (assoc "file_path" arguments :test #'string=)))
        (old-string (cdr (assoc "old_string" arguments :test #'string=)))
        (new-string (cdr (assoc "new_string" arguments :test #'string=)))
        (replace-all-str (cdr (assoc "replace_all" arguments :test #'string=))))

    (unless file-path
      (return-from edit-tool-handler "Error: No file_path provided"))

    (unless old-string
      (return-from edit-tool-handler "Error: No old_string provided"))

    (unless new-string
      (return-from edit-tool-handler "Error: No new_string provided"))

    (let ((replace-all (and replace-all-str
                            (or (string-equal replace-all-str "true")
                                (string-equal replace-all-str "t")))))

      (edit-file file-path old-string new-string :replace-all replace-all))))

(defun register-edit-tool ()
  "Register the edit tool with the tool registry"
  (register-tool
   "edit"
   "Performs exact string replacements in files. IMPORTANT: You must use the Read tool at least once before editing a file. The old_string must be unique in the file or the edit will fail - either provide more surrounding context to make it unique, or use replace_all: true. When editing text from Read tool output, preserve exact indentation as it appears AFTER the line number prefix (format: spaces + line_number + tab). Never include the line number prefix in old_string or new_string."
   (jsown:new-js
     ("type" "object")
     ("properties" (jsown:new-js
                     ("file_path" (jsown:new-js
                                    ("type" "string")
                                    ("description" "The absolute path to the file to modify")))
                     ("old_string" (jsown:new-js
                                     ("type" "string")
                                     ("description" "The exact text to replace")))
                     ("new_string" (jsown:new-js
                                     ("type" "string")
                                     ("description" "The replacement text (must differ from old_string)")))
                     ("replace_all" (jsown:new-js
                                      ("type" "boolean")
                                      ("description" "Replace all occurrences of old_string (default false)")))))
     ("required" (vector "file_path" "old_string" "new_string")))
   #'edit-tool-handler))

;; Auto-register edit tool on load
(register-edit-tool)
