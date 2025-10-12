;;;; grep.lisp - Content search tool using ripgrep

(in-package :tcode)

(defun grep-search (pattern &key path output-mode type glob case-insensitive
                                 show-line-numbers context-after context-before
                                 context-both head-limit multiline)
  "Search file contents using ripgrep.

   Parameters:
   - pattern: Regular expression pattern to search for (required)
   - path: File or directory to search in (defaults to current working directory)
   - output-mode: \"content\" (matching lines), \"files_with_matches\" (file paths), or \"count\" (match counts)
   - type: File type to search (e.g., \"js\", \"py\", \"rust\")
   - glob: Glob pattern to filter files (e.g., \"*.js\", \"*.{ts,tsx}\")
   - case-insensitive: If true, perform case-insensitive search
   - show-line-numbers: If true, show line numbers (requires output-mode: \"content\")
   - context-after: Number of lines to show after each match (requires output-mode: \"content\")
   - context-before: Number of lines to show before each match (requires output-mode: \"content\")
   - context-both: Number of lines to show before and after each match (requires output-mode: \"content\")
   - head-limit: Limit output to first N lines/entries
   - multiline: If true, enable multiline mode where patterns can span lines"

  (handler-case
      (let ((args '("rg")))

        ;; Add output mode flags
        (cond
          ((string-equal output-mode "files_with_matches")
           (push "-l" args))  ; Files with matches only
          ((string-equal output-mode "count")
           (push "-c" args))  ; Count matches per file
          ;; For "content" mode, no special flag needed (default behavior)
          )

        ;; Add case-insensitive flag
        (when case-insensitive
          (push "-i" args))

        ;; Add line numbers flag (only for content mode)
        (when (and show-line-numbers (string-equal output-mode "content"))
          (push "-n" args))

        ;; Add context flags (only for content mode)
        (when (and context-both (string-equal output-mode "content"))
          (push (format nil "-C~D" context-both) args))
        (when (and context-after (string-equal output-mode "content"))
          (push (format nil "-A~D" context-after) args))
        (when (and context-before (string-equal output-mode "content"))
          (push (format nil "-B~D" context-before) args))

        ;; Add multiline flag
        (when multiline
          (push "-U" args)
          (push "--multiline-dotall" args))

        ;; Add type filter
        (when type
          (push (format nil "--type=~A" type) args))

        ;; Add glob filter
        (when glob
          (push (format nil "--glob=~A" glob) args))

        ;; Add pattern
        (push pattern args)

        ;; Add path if specified
        (when path
          (push path args))

        ;; Reverse args since we built them backwards
        (setf args (nreverse args))

        ;; Execute ripgrep
        (let* ((command (if head-limit
                           (format nil "~{~A~^ ~} | head -n ~D" args head-limit)
                           (format nil "~{~A~^ ~}" args)))
               (result (uiop:run-program command
                                        :output :string
                                        :error-output :string
                                        :ignore-error-status t)))

          (if (and result (> (length result) 0))
              result
              "No matches found")))
    (error (e)
      (format nil "Error: ~A" e))))

(defun grep-tool-handler (arguments)
  "Handler function for the grep tool. Arguments is an alist with search parameters."
  (let ((pattern (cdr (assoc "pattern" arguments :test #'string=)))
        (path (cdr (assoc "path" arguments :test #'string=)))
        (output-mode (cdr (assoc "output_mode" arguments :test #'string=)))
        (type (cdr (assoc "type" arguments :test #'string=)))
        (glob (cdr (assoc "glob" arguments :test #'string=)))
        (case-insensitive-str (cdr (assoc "-i" arguments :test #'string=)))
        (show-line-numbers-str (cdr (assoc "-n" arguments :test #'string=)))
        (context-after-str (cdr (assoc "-A" arguments :test #'string=)))
        (context-before-str (cdr (assoc "-B" arguments :test #'string=)))
        (context-both-str (cdr (assoc "-C" arguments :test #'string=)))
        (head-limit-str (cdr (assoc "head_limit" arguments :test #'string=)))
        (multiline-str (cdr (assoc "multiline" arguments :test #'string=))))

    (unless pattern
      (return-from grep-tool-handler "Error: No pattern provided"))

    ;; Default output mode to files_with_matches
    (unless output-mode
      (setf output-mode "files_with_matches"))

    ;; Parse boolean and integer parameters
    (let ((case-insensitive (and case-insensitive-str
                                (or (string-equal case-insensitive-str "true")
                                    (string-equal case-insensitive-str "t"))))
          (show-line-numbers (and show-line-numbers-str
                                 (or (string-equal show-line-numbers-str "true")
                                     (string-equal show-line-numbers-str "t"))))
          (multiline (and multiline-str
                         (or (string-equal multiline-str "true")
                             (string-equal multiline-str "t"))))
          (context-after (when context-after-str
                          (handler-case (parse-integer context-after-str)
                            (error () nil))))
          (context-before (when context-before-str
                           (handler-case (parse-integer context-before-str)
                             (error () nil))))
          (context-both (when context-both-str
                         (handler-case (parse-integer context-both-str)
                           (error () nil))))
          (head-limit (when head-limit-str
                       (handler-case (parse-integer head-limit-str)
                         (error () nil)))))

      (grep-search pattern
                   :path path
                   :output-mode output-mode
                   :type type
                   :glob glob
                   :case-insensitive case-insensitive
                   :show-line-numbers show-line-numbers
                   :context-after context-after
                   :context-before context-before
                   :context-both context-both
                   :head-limit head-limit
                   :multiline multiline))))

(defun register-grep-tool ()
  "Register the grep tool with the tool registry"
  (register-tool
   "grep"
   "A powerful search tool built on ripgrep

  Usage:
  - ALWAYS use Grep for search tasks. NEVER invoke `grep` or `rg` as a Bash command. The Grep tool has been optimized for correct permissions and access.
  - Supports full regex syntax (e.g., \"log.*Error\", \"function\\s+\\w+\")
  - Filter files with glob parameter (e.g., \"*.js\", \"**/*.tsx\") or type parameter (e.g., \"js\", \"py\", \"rust\")
  - Output modes: \"content\" shows matching lines, \"files_with_matches\" shows only file paths (default), \"count\" shows match counts
  - Use Task tool for open-ended searches requiring multiple rounds
  - Pattern syntax: Uses ripgrep (not grep) - literal braces need escaping (use `interface\\{\\}` to find `interface{}` in Go code)
  - Multiline matching: By default patterns match within single lines only. For cross-line patterns like `struct \\{[\\s\\S]*?field`, use `multiline: true`
"
   (jsown:new-js
     ("type" "object")
     ("properties" (jsown:new-js
                     ("pattern" (jsown:new-js
                                  ("type" "string")
                                  ("description" "The regular expression pattern to search for in file contents")))
                     ("path" (jsown:new-js
                               ("type" "string")
                               ("description" "File or directory to search in (rg PATH). Defaults to current working directory.")))
                     ("output_mode" (jsown:new-js
                                      ("type" "string")
                                      ("enum" (vector "content" "files_with_matches" "count"))
                                      ("description" "Output mode: \"content\" shows matching lines (supports -A/-B/-C context, -n line numbers, head_limit), \"files_with_matches\" shows file paths (supports head_limit), \"count\" shows match counts (supports head_limit). Defaults to \"files_with_matches\".")))
                     ("type" (jsown:new-js
                               ("type" "string")
                               ("description" "File type to search (rg --type). Common types: js, py, rust, go, java, etc. More efficient than include for standard file types.")))
                     ("glob" (jsown:new-js
                               ("type" "string")
                               ("description" "Glob pattern to filter files (e.g. \"*.js\", \"*.{ts,tsx}\") - maps to rg --glob")))
                     ("-i" (jsown:new-js
                             ("type" "boolean")
                             ("description" "Case insensitive search (rg -i)")))
                     ("-n" (jsown:new-js
                             ("type" "boolean")
                             ("description" "Show line numbers in output (rg -n). Requires output_mode: \"content\", ignored otherwise.")))
                     ("-A" (jsown:new-js
                             ("type" "number")
                             ("description" "Number of lines to show after each match (rg -A). Requires output_mode: \"content\", ignored otherwise.")))
                     ("-B" (jsown:new-js
                             ("type" "number")
                             ("description" "Number of lines to show before each match (rg -B). Requires output_mode: \"content\", ignored otherwise.")))
                     ("-C" (jsown:new-js
                             ("type" "number")
                             ("description" "Number of lines to show before and after each match (rg -C). Requires output_mode: \"content\", ignored otherwise.")))
                     ("head_limit" (jsown:new-js
                                     ("type" "number")
                                     ("description" "Limit output to first N lines/entries, equivalent to \"| head -N\". Works across all output modes: content (limits output lines), files_with_matches (limits file paths), count (limits count entries). When unspecified, shows all results from ripgrep.")))
                     ("multiline" (jsown:new-js
                                    ("type" "boolean")
                                    ("description" "Enable multiline mode where . matches newlines and patterns can span lines (rg -U --multiline-dotall). Default: false.")))))
     ("required" (vector "pattern")))
   #'grep-tool-handler))

;; Auto-register grep tool on load
(register-grep-tool)
