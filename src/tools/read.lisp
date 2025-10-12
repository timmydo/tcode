;;;; read.lisp - File reading tool

(in-package :tcode)

(defun read-file-lines (file-path &key (offset 0) (limit 2000))
  "Read lines from a file with optional offset and limit.
   Returns a string with line numbers in cat -n format."
  (unless (probe-file file-path)
    (return-from read-file-lines
      (format nil "Error: File not found: ~A" file-path)))

  (unless (uiop:file-exists-p file-path)
    (return-from read-file-lines
      (format nil "Error: Path is not a file: ~A" file-path)))

  (handler-case
      (with-open-file (stream file-path :direction :input :if-does-not-exist nil)
        (unless stream
          (return-from read-file-lines
            (format nil "Error: Cannot open file: ~A" file-path)))

        (let ((output (make-array 0 :element-type 'character
                                   :fill-pointer 0
                                   :adjustable t))
              (line-num 1))

          ;; Skip lines until offset
          (loop repeat offset
                do (read-line stream nil nil)
                   (incf line-num))

          ;; Read up to limit lines
          (loop for i from 0 below limit
                for line = (read-line stream nil nil)
                while line
                do (progn
                     ;; Format with line number like cat -n (right-aligned, tab separator)
                     (let ((formatted-line (format nil "~6D→~A~%" line-num line)))
                       (loop for char across formatted-line
                             do (vector-push-extend char output)))
                     (incf line-num)))

          ;; Convert to string
          (if (> (length output) 0)
              (coerce output 'string)
              (format nil "File is empty or offset is beyond end of file"))))
    (error (e)
      (format nil "Error reading file: ~A" e))))

(defun read-tool-handler (arguments)
  "Handler function for the read tool. Arguments is an alist with 'file_path', optional 'offset' and 'limit' keys."
  (let ((file-path (cdr (assoc "file_path" arguments :test #'string=)))
        (offset-str (cdr (assoc "offset" arguments :test #'string=)))
        (limit-str (cdr (assoc "limit" arguments :test #'string=))))

    (unless file-path
      (return-from read-tool-handler "Error: No file_path provided"))

    (let ((offset (if offset-str
                      (handler-case
                          (parse-integer offset-str)
                        (error () 0))
                      0))
          (limit (if limit-str
                     (handler-case
                         (parse-integer limit-str)
                       (error () 2000))
                     2000)))

      (read-file-lines file-path :offset offset :limit limit))))

(defun register-read-tool ()
  "Register the read tool with the tool registry"
  (register-tool
   "read"
   "Read contents of a file from the local filesystem. Returns file contents with line numbers in cat -n format (lines longer than 2000 characters are truncated). By default reads up to 2000 lines from the beginning. Use offset and limit parameters for large files."
   (jsown:new-js
     ("type" "object")
     ("properties" (jsown:new-js
                     ("file_path" (jsown:new-js
                                    ("type" "string")
                                    ("description" "The absolute path to the file to read")))
                     ("offset" (jsown:new-js
                                 ("type" "integer")
                                 ("description" "The line number to start reading from (0-indexed, optional)")))
                     ("limit" (jsown:new-js
                               ("type" "integer")
                               ("description" "The number of lines to read (default 2000, optional)")))))
     ("required" (vector "file_path")))
   #'read-tool-handler))

;; Auto-register read tool on load
(register-read-tool)
