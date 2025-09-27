(in-package :tcode)

(defclass web-ui ()
  ((server-socket :initform nil :accessor server-socket)
   (client-connections :initform nil :accessor client-connections))
  (:documentation "A web UI implementation for tcode using a basic webserver."))

(defvar *web-ui-instance* nil)
(defvar *server-running* nil)
(defvar *web-repl-context* nil)

(defun quit-tcode ()
  "Quit tcode without prompting."
  (format t "Quitting tcode...~%")
  (setf *server-running* nil)
  (bt:make-thread
   (lambda () (sleep 0.5) (uiop:quit 0))
   :name "quit-thread"))

(defun split-string (string separator)
  "Split string by separator character"
  (when (null string)
    (return-from split-string '()))

  (let ((result '())
        (current "")
        (len (length string)))

    (loop for i from 0 below len
          do (let ((char (char string i)))
               (if (char= char separator)
                   (progn
                     (push current result)
                     (setf current ""))
                   (setf current (concatenate 'string current (string char))))))

    ;; Add final part
    (push current result)

    ;; Return in correct order
    (nreverse result)))

(defparameter *html-template*
  "<!DOCTYPE html>
<html>
<head>
    <title>tcode - Terminal-based CLI</title>
    <style>
        body {
            background-color: #282c34;
            color: #abb2bf;
            font-family: 'Courier New', monospace;
            margin: 0;
            padding: 0;
            overflow: hidden;
            position: relative;
            height: 100vh;
        }
        #app-container {
            height: 100vh;
            display: flex;
            flex-direction: column;
        }
        #content-area {
            flex: 1;
            overflow-y: auto;
            padding: 10px;
            border-bottom: 1px solid #5c6370;
            white-space: pre-wrap;
            font-family: 'Courier New', monospace;
            line-height: 1.4;
        }
        #bottom-interface {
            height: 80px;
            background-color: #21252b;
            border-top: 1px solid #5c6370;
            display: flex;
            flex-direction: column;
        }
        #prompt-area {
            flex: 1;
            display: flex;
            align-items: center;
            padding: 0 10px;
            border-bottom: 1px solid #5c6370;
        }
        #prompt-label {
            color: #56b6c2;
            margin-right: 5px;
            font-weight: bold;
        }
        #command-input {
            flex: 1;
            background: transparent;
            border: none;
            color: #abb2bf;
            font-family: 'Courier New', monospace;
            font-size: 14px;
            outline: none;
        }
        #status-area {
            height: 25px;
            padding: 5px 10px;
            color: #e5c07b;
            font-size: 12px;
            display: flex;
            align-items: center;
        }
        .history-item {
            margin-bottom: 10px;
        }
        .command-line {
            color: #56b6c2;
            margin-bottom: 5px;
        }
        .result-line {
            color: #abb2bf;
            margin-left: 0;
            white-space: pre-wrap;
        }
        .error-line {
            color: #e06c75;
        }
        #scroll-indicator {
            position: absolute;
            right: 10px;
            top: 50%;
            transform: translateY(-50%);
            color: #5c6370;
            font-size: 12px;
        }
    </style>
</head>
<body>
    <div id=\"app-container\">
        <div id=\"content-area\">
            <div style=\"color: #56b6c2; margin-bottom: 10px;\">tcode - Terminal-based CLI</div>
            <div style=\"color: #abb2bf; margin-bottom: 20px;\">Webview session active. Type commands or 'exit' to quit.</div>
            <div id=\"history-container\"></div>
        </div>
        <div id=\"bottom-interface\">
            <div id=\"prompt-area\">
                <span id=\"prompt-label\">tcode&gt;</span>
                <input type=\"text\" id=\"command-input\" autocomplete=\"off\" spellcheck=\"false\">
            </div>
            <div id=\"status-area\" id=\"status-message\"></div>
        </div>
    </div>

    <script>
        let commandHistory = [];
        let historyIndex = 0;
        let originalInput = '';

        const commandInput = document.getElementById('command-input');
        const historyContainer = document.getElementById('history-container');
        const statusArea = document.getElementById('status-area');

        // Focus input on load
        commandInput.focus();

        // Handle command submission
        commandInput.addEventListener('keydown', function(e) {
            if (e.key === 'Enter') {
                const command = commandInput.value.trim();
                if (command === 'exit') {
                    fetch('/quit', { method: 'POST' });
                    return;
                }
                if (command) {
                    submitCommand(command);
                }
            } else if (e.key === 'ArrowUp') {
                e.preventDefault();
                navigateHistory(-1);
            } else if (e.key === 'ArrowDown') {
                e.preventDefault();
                navigateHistory(1);
            } else if (e.key === 'Escape') {
                commandInput.value = '';
                historyIndex = 0;
                originalInput = '';
            }
        });

        function navigateHistory(direction) {
            if (direction === -1 && historyIndex < commandHistory.length) {
                if (historyIndex === 0) {
                    originalInput = commandInput.value;
                }
                historyIndex++;
                commandInput.value = commandHistory[commandHistory.length - historyIndex] || '';
            } else if (direction === 1 && historyIndex > 0) {
                historyIndex--;
                if (historyIndex === 0) {
                    commandInput.value = originalInput;
                } else {
                    commandInput.value = commandHistory[commandHistory.length - historyIndex] || '';
                }
            }
        }

        function submitCommand(command) {
            // Add to local history
            commandHistory.push(command);
            historyIndex = 0;
            originalInput = '';

            // Clear input
            commandInput.value = '';

            // Show status
            statusArea.textContent = 'Processing...';

            // Submit to server
            fetch('/command', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ command: command })
            })
            .then(response => response.json())
            .then(data => {
                statusArea.textContent = '';
                updateHistory();
            })
            .catch(error => {
                console.error('Error:', error);
                statusArea.textContent = 'Error: ' + error.message;
            });
        }

        function updateHistory() {
            fetch('/history')
            .then(response => response.json())
            .then(data => {
                displayHistory(data.history);
            })
            .catch(error => {
                console.error('Error fetching history:', error);
            });
        }

        function displayHistory(history) {
            historyContainer.innerHTML = '';
            history.forEach(item => {
                const historyItem = document.createElement('div');
                historyItem.className = 'history-item';

                const commandLine = document.createElement('div');
                commandLine.className = 'command-line';
                commandLine.textContent = 'tcode> ' + item.command;
                historyItem.appendChild(commandLine);

                if (item.result) {
                    const resultLine = document.createElement('div');
                    resultLine.className = item.result.includes('Error') ? 'result-line error-line' : 'result-line';
                    resultLine.textContent = item.result;
                    historyItem.appendChild(resultLine);
                }

                historyContainer.appendChild(historyItem);
            });

            // Scroll to bottom
            const contentArea = document.getElementById('content-area');
            contentArea.scrollTop = contentArea.scrollHeight;
        }

        // Initial history load
        updateHistory();

        // Periodic updates
        setInterval(updateHistory, 1000);
    </script>
</body>
</html>")

(defun make-web-ui ()
  "Create a new web UI instance."
  (make-instance 'web-ui))

(defun run-web-ui (ui port)
  "Start the web server for the UI."
  (setf *web-ui-instance* ui)
  (setf *server-running* t)

  ;; Create a single repl-context for the web UI
  (setf *web-repl-context* (make-repl-context :mutex (make-lock-with-logging "web-repl-mutex")))

  (let ((server-socket (usocket:socket-listen "127.0.0.1" port :reuse-address t :backlog 5)))
    (setf (server-socket ui) server-socket)
    (format t "tcode web server starting on port ~D~%" port)
    (force-output)

    (unwind-protect
        (loop while *server-running*
              do (when (usocket:wait-for-input server-socket :timeout 1)
                   (let ((client-socket (usocket:socket-accept server-socket)))
                     (bt:make-thread
                      (lambda () (handle-client ui client-socket))
                      :name "tcode-client-handler"))))
      (usocket:socket-close server-socket))))

(defun handle-client (ui client-socket)
  "Handle a single client connection."
  (let ((stream (usocket:socket-stream client-socket)))
    (unwind-protect
        (handler-case
            (let* ((request-line (read-line stream))
                   (parts (split-string request-line #\Space))
                   (method (first parts))
                   (path (second parts))
                   (headers (make-hash-table :test 'equal)))

              ;; Read headers
              (loop for line = (read-line stream nil)
                    until (or (null line) (string= line "") (string= line (string #\Return)))
                    do (let ((colon-pos (position #\: line)))
                         (when colon-pos
                           (let ((header-name (string-trim " " (subseq line 0 colon-pos)))
                                 (header-value (string-trim " " (subseq line (1+ colon-pos)))))
                             (setf (gethash (string-upcase header-name) headers) header-value)))))

              (cond
                ((and (string= method "GET") (string= path "/"))
                 (send-html-response stream *html-template*))

                ((and (string= method "POST") (string= path "/command"))
                 (handle-command-request stream ui headers))

                ((and (string= method "GET") (string= path "/history"))
                 (handle-history-request stream))

                ((and (string= method "POST") (string= path "/quit"))
                 (send-json-response stream "{\"status\": \"ok\"}")
                 (quit-tcode))

                (t (send-404-response stream))))

          (error (e)
            (format t "Client error: ~A~%" e)
            (send-500-response stream)))

      (ignore-errors (usocket:socket-close client-socket)))))

(defun handle-command-request (stream ui headers)
  "Handle command submission."
  (declare (ignore ui))
  (log-info "handle-command-request")
  (let* ((content-length (or (parse-integer (or (gethash "CONTENT-LENGTH" headers) "0") :junk-allowed t) 0))
         (body (make-string content-length))
         (json-data nil)
         (command nil))

    (when (> content-length 0)
      (read-sequence body stream)
      (setf json-data (jsown:parse body))
      (setf command (jsown:val json-data "command")))

    (unless command
      (log-info "Command not found: ~A" body))
    
    (when command
      ;; Log the submitted command
      (log-command command)
      ;; Use the global web repl-context
      (submit-command command *web-repl-context*))

    (send-json-response stream "{\"status\": \"ok\"}")))

(defun handle-history-request (stream)
  "Handle history request."
  (let* ((history-data (if *web-repl-context*
                           (bt:with-lock-held ((repl-context-mutex *web-repl-context*))
                             (repl-context-history *web-repl-context*))
                           '()))
         (json-history (jsown:new-js
                         ("history" (mapcar (lambda (item)
                                              (jsown:new-js
                                                ("command" (history-item-command item))
                                                ("result" (format nil "~A" (history-item-result item)))))
                                            (reverse history-data))))))
    (send-json-response stream (jsown:to-json json-history))))


(defun send-html-response (stream html)
  "Send HTML response."
  (format stream "HTTP/1.1 200 OK~C~C" #\Return #\Newline)
  (format stream "Content-Type: text/html; charset=utf-8~C~C" #\Return #\Newline)
  (format stream "Content-Length: ~D~C~C" (length (babel:string-to-octets html :encoding :utf-8)) #\Return #\Newline)
  (format stream "~C~C" #\Return #\Newline)
  (write-string html stream)
  (force-output stream))

(defun send-json-response (stream json)
  "Send JSON response."
  (format stream "HTTP/1.1 200 OK~C~C" #\Return #\Newline)
  (format stream "Content-Type: application/json~C~C" #\Return #\Newline)
  (format stream "Content-Length: ~D~C~C" (length (babel:string-to-octets json :encoding :utf-8)) #\Return #\Newline)
  (format stream "~C~C" #\Return #\Newline)
  (write-string json stream)
  (force-output stream))

(defun send-404-response (stream)
  "Send 404 response."
  (let ((body "Not Found"))
    (format stream "HTTP/1.1 404 Not Found~C~C" #\Return #\Newline)
    (format stream "Content-Type: text/plain~C~C" #\Return #\Newline)
    (format stream "Content-Length: ~D~C~C" (length body) #\Return #\Newline)
    (format stream "~C~C" #\Return #\Newline)
    (write-string body stream)
    (force-output stream)))

(defun send-500-response (stream)
  "Send 500 response."
  (let ((body "Internal Server Error"))
    (format stream "HTTP/1.1 500 Internal Server Error~C~C" #\Return #\Newline)
    (format stream "Content-Type: text/plain~C~C" #\Return #\Newline)
    (format stream "Content-Length: ~D~C~C" (length body) #\Return #\Newline)
    (format stream "~C~C" #\Return #\Newline)
    (write-string body stream)
    (force-output stream)))


(defun run-webview (&key title url width height)
  "Launch a webview window pointing to the given URL."
  (let ((w (webview:webview-create 0 (cffi:null-pointer))))
    (unwind-protect
         (progn
           (webview:webview-set-title w title)
           (webview:webview-set-size w width height 0)
           (webview:webview-navigate w url)
           (webview:webview-run w))
      (webview:webview-destroy w))))

(defun find-available-port (&optional (start-port 8080))
  "Find an available port starting from the given port."
  (handler-case
      (let ((test-socket (usocket:socket-listen "127.0.0.1" start-port :reuse-address t)))
        (usocket:socket-close test-socket)
        start-port)
    (usocket:address-in-use-error ()
      (find-available-port (1+ start-port)))))
