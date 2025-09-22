(in-package :tcode)

(defparameter *config-template*
  "(in-package :tcode)

;; tcode configuration file
;; This file is loaded at startup and can contain any valid Common Lisp code.
;; You can define variables, functions, and customize the behavior of tcode.

;; Backend configuration
;; Replace \"enter-api-key-here\" with your actual OpenRouter API key
(defvar *tcode-backend* (make-openrouter-connection \"enter-api-key-here\"))

;; Default model for OpenRouter
(defvar *openrouter-default-model* \"x-ai/grok-4-fast:free\")

;; Example: Set a custom prompt color
;; (defparameter *custom-prompt-color* 6)

;; Example: Define custom functions
;; (defun my-helper ()
;;   \"A custom helper function\"
;;   (format t \"This is my custom function!\"))

;; Example: Add initialization code
;; (format t \"Configuration loaded successfully!~%\")
")

(defun get-config-directory ()
  "Get the tcode configuration directory path"
  (merge-pathnames ".tcode/" (user-homedir-pathname)))

(defun get-config-file-path ()
  "Get the full path to the configuration file"
  (merge-pathnames "config.lisp" (get-config-directory)))

(defun ensure-config-directory ()
  "Ensure the configuration directory exists"
  (let ((config-dir (get-config-directory)))
    (unless (probe-file config-dir)
      (ensure-directories-exist config-dir))))

(defun initialize-config-file ()
  "Initialize the configuration file if it doesn't exist"
  (let ((config-file (get-config-file-path)))
    (if (probe-file config-file)
        (format nil "Configuration file already exists at: ~A" config-file)
        (progn
          (ensure-config-directory)
          (with-open-file (stream config-file
                                  :direction :output
                                  :if-exists :error
                                  :if-does-not-exist :create)
            (write-string *config-template* stream))
          (format nil "Created configuration file at: ~A" config-file)))))

(defun load-config-file ()
  "Load the configuration file if it exists, with error handling"
  (let ((config-file (get-config-file-path)))
    (if (probe-file config-file)
        (handler-case
            (progn
              (load config-file)
              t) ; Return t on success
          (error (e)
            (format t "Error loading configuration file ~A: ~A~%" config-file e)
            (finish-output)
            nil)) ; Return nil on error
        t))) ; Return t if no config file exists (not an error)