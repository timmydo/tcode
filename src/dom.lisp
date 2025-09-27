(in-package :tcode)

(defstruct dom-element
  "A DOM element with tag, attributes, and children"
  tag
  attributes
  children)

(defstruct dom-text
  "A DOM text node"
  content)

(defun dom (tag &key attributes children)
  "Create a DOM element with given tag, attributes, and children"
  (make-dom-element
   :tag tag
   :attributes (or attributes '())
   :children (or children '())))

(defun text (content)
  "Create a text node with the given content"
  (make-dom-text :content content))

(defun attr (name value)
  "Create an attribute key-value pair"
  (cons name value))

(defun div (&key class id style onclick data-attributes children)
  "Helper function to create a div element"
  (let ((attributes '()))
    (when class (push (attr "class" class) attributes))
    (when id (push (attr "id" id) attributes))
    (when style (push (attr "style" style) attributes))
    (when onclick (push (attr "onclick" onclick) attributes))
    (when data-attributes
      (dolist (data-attr data-attributes)
        (push (attr (format nil "data-~A" (car data-attr)) (cdr data-attr)) attributes)))
    (dom "div" :attributes (reverse attributes) :children children)))

(defun span (&key class style data-attributes children)
  "Helper function to create a span element"
  (let ((attributes '()))
    (when class (push (attr "class" class) attributes))
    (when style (push (attr "style" style) attributes))
    (when data-attributes
      (dolist (data-attr data-attributes)
        (push (attr (format nil "data-~A" (car data-attr)) (cdr data-attr)) attributes)))
    (dom "span" :attributes (reverse attributes) :children children)))

(defun escape-html-text (text)
  "Escape HTML special characters in text"
  (let ((result ""))
    (loop for char across text do
      (setf result
            (concatenate 'string result
                         (case char
                           (#\< "&lt;")
                           (#\> "&gt;")
                           (#\& "&amp;")
                           (#\" "&quot;")
                           (t (string char))))))
    result))

(defun dom-to-json (dom-node)
  "Convert a DOM node to JSON-serializable structure"
  (cond
    ((dom-text-p dom-node)
     ;; Text nodes are represented as strings
     (dom-text-content dom-node))
    ((dom-element-p dom-node)
     ;; Elements are represented as objects with type, tag, attributes, and children
     (let ((tag (dom-element-tag dom-node))
           (attributes (dom-element-attributes dom-node))
           (children (dom-element-children dom-node)))
       (jsown:new-js
         ("type" "element")
         ("tag" tag)
         ("attributes" (if attributes
                          (let ((attrs (jsown:new-js)))
                            (dolist (attr attributes)
                              (setf (jsown:val attrs (car attr)) (format nil "~A" (cdr attr))))
                            attrs)
                          (jsown:new-js)))
         ("children" (mapcar #'dom-to-json children)))))
    (t
     ;; Fallback for other types - convert to string
     (format nil "~A" dom-node))))