(in-package :tcode)

(defclass web-ui ()
  ((server-socket :initform nil :accessor server-socket)
   (client-connections :initform nil :accessor client-connections))
  (:documentation "A web UI implementation for tcode using a basic webserver."))

(defvar *web-ui-instance* nil)
(defvar *server-running* nil)
(defvar *web-repl-context* nil)
(defvar *sse-clients* nil)

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
        // DOM Reconciliation Functions (borrowed from TLE)
        function jsonToDOM(jsonNode) {
            if (typeof jsonNode === 'string') {
                // Text node
                return document.createTextNode(jsonNode);
            } else if (jsonNode && jsonNode.type === 'element') {
                // Element node
                const element = document.createElement(jsonNode.tag);

                // Set attributes
                if (jsonNode.attributes) {
                    for (const [key, value] of Object.entries(jsonNode.attributes)) {
                        element.setAttribute(key, value);
                    }
                }

                // Add children
                if (jsonNode.children) {
                    for (const child of jsonNode.children) {
                        element.appendChild(jsonToDOM(child));
                    }
                }

                return element;
            }
            // Fallback for unknown types
            return document.createTextNode(String(jsonNode));
        }

        function updateDOMFromJSON(domParent, jsonNode) {
            // Clear existing content if JSON represents a single child
            if (typeof jsonNode === 'string' || (jsonNode && jsonNode.type === 'element')) {
                // Single node - replace all children
                while (domParent.firstChild) {
                    domParent.removeChild(domParent.firstChild);
                }
                domParent.appendChild(jsonToDOM(jsonNode));
            } else if (Array.isArray(jsonNode)) {
                // Array of nodes - reconcile children
                reconcileChildren(domParent, jsonNode);
            }
        }

        function reconcileChildren(domParent, newChildrenJSON) {
            const currentChildren = Array.from(domParent.childNodes);

            // Reconcile each position
            for (let i = 0; i < Math.max(currentChildren.length, newChildrenJSON.length); i++) {
                const currentChild = currentChildren[i];
                const newChildJSON = newChildrenJSON[i];

                if (!newChildJSON) {
                    // Remove extra existing children
                    if (currentChild) {
                        domParent.removeChild(currentChild);
                    }
                } else if (!currentChild) {
                    // Add new child
                    domParent.appendChild(jsonToDOM(newChildJSON));
                } else {
                    // Reconcile existing child
                    if (!nodesEqual(currentChild, newChildJSON)) {
                        // Replace if they're different
                        const newChild = jsonToDOM(newChildJSON);
                        domParent.replaceChild(newChild, currentChild);
                    } else if (currentChild.nodeType === Node.ELEMENT_NODE && newChildJSON.type === 'element') {
                        // Recursively reconcile element children
                        reconcileElement(currentChild, newChildJSON);
                    }
                }
            }
        }

        function reconcileElement(domElement, jsonElement) {
            // Update attributes
            const currentAttrs = new Set();
            for (const attr of domElement.attributes) {
                currentAttrs.add(attr.name);
                if (jsonElement.attributes[attr.name] !== attr.value) {
                    domElement.setAttribute(attr.name, jsonElement.attributes[attr.name] || '');
                }
            }

            // Add new attributes
            if (jsonElement.attributes) {
                for (const [key, value] of Object.entries(jsonElement.attributes)) {
                    if (!currentAttrs.has(key)) {
                        domElement.setAttribute(key, value);
                    }
                }
            }

            // Remove old attributes not in new
            for (const attr of Array.from(domElement.attributes)) {
                if (!jsonElement.attributes || !(attr.name in jsonElement.attributes)) {
                    domElement.removeAttribute(attr.name);
                }
            }

            // Reconcile children
            if (jsonElement.children) {
                reconcileChildren(domElement, jsonElement.children);
            }
        }

        function nodesEqual(domNode, jsonNode) {
            if (domNode.nodeType === Node.TEXT_NODE) {
                return typeof jsonNode === 'string' && domNode.textContent === jsonNode;
            } else if (domNode.nodeType === Node.ELEMENT_NODE) {
                return jsonNode && jsonNode.type === 'element' &&
                       domNode.tagName.toLowerCase() === jsonNode.tag;
            }
            return false;
        }

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
                // History will be updated via SSE
            })
            .catch(error => {
                console.error('Error:', error);
                statusArea.textContent = 'Error: ' + error.message;
            });
        }

        function displayHistory(historyDOM) {
            const contentArea = document.getElementById('content-area');
            const wasAtBottom = contentArea.scrollTop >= contentArea.scrollHeight - contentArea.clientHeight - 10;

            // Use DOM reconciliation instead of innerHTML replacement
            updateDOMFromJSON(historyContainer, historyDOM);

            // Only scroll to bottom if user was already at bottom (auto-scroll new content)
            if (wasAtBottom) {
                contentArea.scrollTop = contentArea.scrollHeight;
            }
        }

        function updateLastHistoryEntry(command, result) {
            const contentArea = document.getElementById('content-area');
            const wasAtBottom = contentArea.scrollTop >= contentArea.scrollHeight - contentArea.clientHeight - 10;

            // Find the last history item by looking for the last element with class 'history-item'
            const historyItems = historyContainer.getElementsByClassName('history-item');

            console.log('Updating last history entry, found', historyItems.length, 'history items');
            console.log('Command:', command, 'Result length:', result.length);

            if (historyItems.length > 0) {
                const lastItem = historyItems[historyItems.length - 1];

                // Find the result line within the last item
                let resultLine = lastItem.querySelector('.result-line');
                if (resultLine) {
                    // Update the result text
                    resultLine.textContent = result;
                    // Update class based on content (error detection)
                    resultLine.className = result.includes('Error') ? 'result-line error-line' : 'result-line';
                    console.log('Updated existing result line');
                } else {
                    // If no result line exists, create one
                    const newResultLine = document.createElement('div');
                    newResultLine.className = result.includes('Error') ? 'result-line error-line' : 'result-line';
                    newResultLine.textContent = result;
                    lastItem.appendChild(newResultLine);
                    console.log('Created new result line');
                }

                // Only scroll to bottom if user was already at bottom (auto-scroll new content)
                if (wasAtBottom) {
                    contentArea.scrollTop = contentArea.scrollHeight;
                }
            } else {
                console.log('No history items found for incremental update');
            }
        }

        // Set up Server-Sent Events for real-time updates
        const eventSource = new EventSource('/events');

        eventSource.onmessage = function(event) {
            try {
                const data = JSON.parse(event.data);
                console.log('Received SSE message:', data.type);
                if (data.type === 'history-update') {
                    console.log('Processing history-update');
                    displayHistory(data.content);
                } else if (data.type === 'incremental-update') {
                    console.log('Processing incremental-update for command:', data.command);
                    updateLastHistoryEntry(data.command, data.result);
                }
            } catch (error) {
                console.error('Error parsing SSE data:', error);
            }
        };

        eventSource.onerror = function(event) {
            console.error('SSE connection error:', event);
            // Optionally implement reconnection logic here
        };

        // Clean up EventSource when page unloads
        window.addEventListener('beforeunload', function() {
            eventSource.close();
        });
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

                ((and (string= method "GET") (string= path "/events"))
                 (handle-sse-request stream))

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
      (submit-command command *web-repl-context*)
      ;; Broadcast history update to all SSE clients
      (broadcast-history-update))

    (send-json-response stream "{\"status\": \"ok\"}")))

(defun format-usage-info (usage)
  "Format usage information for display"
  (when usage
    (let ((prompt-tokens (jsown:val usage "prompt_tokens"))
          (completion-tokens (jsown:val usage "completion_tokens"))
          (total-tokens (jsown:val usage "total_tokens"))
          (cost (jsown:val usage "cost")))
      (if cost
          (format nil "~A/~A tokens (~A total) - $~,2f"
                  (or prompt-tokens "?")
                  (or completion-tokens "?")
                  (or total-tokens "?")
                  cost)
          (format nil "~A/~A tokens (~A total)"
                  (or prompt-tokens "?")
                  (or completion-tokens "?")
                  (or total-tokens "?"))))))

(defun render-history-as-dom ()
  "Render history as DOM structure."
  (let* ((history-data (if *web-repl-context*
                           (bt:with-lock-held ((repl-context-mutex *web-repl-context*))
                             (repl-context-history *web-repl-context*))
                           '()))
         (history-items (mapcar (lambda (item)
                                  (div :class "history-item"
                                       :children (append
                                                  (list
                                                   (div :class "command-line"
                                                        :children (list (text (format nil "tcode> ~A" (history-item-command item))))))
                                                  (when (history-item-result item)
                                                    (list
                                                     (div :class (if (search "Error" (format nil "~A" (history-item-result item)))
                                                                     "result-line error-line"
                                                                     "result-line")
                                                          :children (list (text (format nil "~A" (history-item-result item)))))))
                                                  (when (history-item-usage item)
                                                    (list
                                                     (div :class "usage-line"
                                                          :style "text-align: right; color: #666; font-size: 0.8em; margin-top: 4px;"
                                                          :children (list (text (format-usage-info (history-item-usage item))))))))))
                                (reverse history-data))))
    (div :children history-items)))

(defun handle-history-request (stream)
  "Handle history request with DOM reconciliation."
  (let* ((history-dom (render-history-as-dom))
         (json-dom (dom-to-json history-dom))
         (response (jsown:new-js ("type" "history-update") ("content" json-dom))))
    (send-json-response stream (jsown:to-json response))))

(defun handle-sse-request (stream)
  "Handle Server-Sent Events connection."
  (format stream "HTTP/1.1 200 OK~C~C" #\Return #\Newline)
  (format stream "Content-Type: text/event-stream~C~C" #\Return #\Newline)
  (format stream "Cache-Control: no-cache~C~C" #\Return #\Newline)
  (format stream "Connection: keep-alive~C~C" #\Return #\Newline)
  (format stream "Access-Control-Allow-Origin: *~C~C" #\Return #\Newline)
  (format stream "~C~C" #\Return #\Newline)
  (force-output stream)

  ;; Add this stream to the list of SSE clients
  (push stream *sse-clients*)

  ;; Send initial history to the new client
  (send-sse-history-update stream)

  ;; Keep the connection open by handling client disconnect
  (handler-case
      (loop
        ;; Send a keep-alive comment every 30 seconds
        (sleep 30)
        (format stream ": keep-alive~C~C" #\Return #\Newline)
        (force-output stream))
    (error ()
      ;; Client disconnected, remove from list
      (setf *sse-clients* (remove stream *sse-clients*)))))

(defun send-sse-history-update (stream)
  "Send history update to a specific SSE client."
  (handler-case
      (let* ((history-dom (render-history-as-dom))
             (json-dom (dom-to-json history-dom))
             (data (jsown:to-json (jsown:new-js ("type" "history-update") ("content" json-dom)))))
        (format stream "data: ~A~C~C~C~C" data #\Return #\Newline #\Return #\Newline)
        (force-output stream))
    (error ()
      ;; Client disconnected, remove from list
      (setf *sse-clients* (remove stream *sse-clients*)))))

(defun broadcast-history-update ()
  "Broadcast history update to all connected SSE clients."
  (let ((disconnected-clients '()))
    (dolist (client *sse-clients*)
      (handler-case
          (send-sse-history-update client)
        (error ()
          ;; Mark client as disconnected
          (push client disconnected-clients))))
    ;; Remove disconnected clients
    (dolist (client disconnected-clients)
      (setf *sse-clients* (remove client *sse-clients*)))))

(defun send-sse-incremental-update (stream command result)
  "Send incremental update for the last history entry to a specific SSE client."
  (handler-case
      (let* ((data (jsown:to-json (jsown:new-js ("type" "incremental-update")
                                                ("command" command)
                                                ("result" result)))))
        (format stream "data: ~A~C~C~C~C" data #\Return #\Newline #\Return #\Newline)
        (force-output stream))
    (error ()
      ;; Client disconnected, remove from list
      (setf *sse-clients* (remove stream *sse-clients*)))))

(defun broadcast-incremental-update (command result)
  "Broadcast incremental update for the last history entry to all connected SSE clients."
  (let ((disconnected-clients '()))
    (dolist (client *sse-clients*)
      (handler-case
          (send-sse-incremental-update client command result)
        (error ()
          ;; Mark client as disconnected
          (push client disconnected-clients))))
    ;; Remove disconnected clients
    (dolist (client disconnected-clients)
      (setf *sse-clients* (remove client *sse-clients*)))))

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
