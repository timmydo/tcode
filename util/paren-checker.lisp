#!/usr/bin/env -S sbcl --script
;;;; paren-checker.lisp - Check for mismatched parentheses in Common Lisp source files

(defpackage :paren-checker
  (:use :common-lisp)
  (:export #:check-parens
           #:check-parens-file
           #:paren-error
           #:paren-error-position
           #:paren-error-expected
           #:paren-error-found
           #:paren-error-line
           #:paren-error-column
           #:main))

(in-package :paren-checker)

(define-condition paren-error (error)
  ((position :initarg :position :reader paren-error-position)
   (expected :initarg :expected :reader paren-error-expected)
   (found :initarg :found :reader paren-error-found)
   (line :initarg :line :reader paren-error-line)
   (column :initarg :column :reader paren-error-column))
  (:report (lambda (condition stream)
             (format stream "Parentheses mismatch at line ~A, column ~A: expected ~A, found ~A"
                     (paren-error-line condition)
                     (paren-error-column condition)
                     (paren-error-expected condition)
                     (paren-error-found condition)))))

(defun opening-paren-p (char)
  (member char '(#\( #\[ #\{)))

(defun closing-paren-p (char)
  (member char '(#\) #\] #\})))

(defun matching-paren (char)
  (case char
    (#\( #\))
    (#\[ #\])
    (#\{ #\})
    (#\) #\()
    (#\] #\[)
    (#\} #\{)))

(defun in-string-p (text position)
  "Check if position is inside a string literal"
  (let ((quote-count 0)
        (escape-next nil))
    (loop for i from 0 below position
          for char = (char text i)
          do (cond
               (escape-next
                (setf escape-next nil))
               ((char= char #\\)
                (setf escape-next t))
               ((char= char #\")
                (incf quote-count))))
    (oddp quote-count)))

(defun in-comment-p (text position)
  "Check if position is inside a line comment"
  (let ((line-start (or (position #\Newline text :end position :from-end t) -1)))
    (and (> position line-start)
         (let ((semicolon-pos (position #\; text :start (1+ line-start) :end position)))
           (and semicolon-pos
                (not (in-string-p text semicolon-pos)))))))

(defun calculate-line-column (text position)
  "Calculate line and column numbers for a position in text"
  (let ((line 1)
        (column 0))
    (loop for i from 0 below (min position (length text))
          for char = (char text i)
          do (if (char= char #\Newline)
                 (progn
                   (incf line)
                   (setf column 0))
                 (incf column)))
    (values line (1+ column))))

(defun check-parens (text)
  "Check parentheses matching in a string of Common Lisp code.
Returns T if all parentheses match, otherwise signals a PAREN-ERROR."
  (let ((stack '())
        (position 0)
        (in-string nil)
        (in-comment nil)
        (escape-next nil))
    (loop for char across text
          do (progn
               (cond
                 (escape-next
                  ;; Skip escaped character
                  (setf escape-next nil))
                 ((char= char #\\)
                  ;; Next character is escaped
                  (setf escape-next t))
                 ((char= char #\")
                  ;; Toggle string state
                  (setf in-string (not in-string)))
                 ((char= char #\Newline)
                  ;; End of line comment
                  (setf in-comment nil))
                 ((char= char #\;)
                  ;; Start of comment (only if not in string)
                  (unless in-string
                    (setf in-comment t)))
                 ((and (not in-string) (not in-comment))
                  ;; Process parentheses only if not in string or comment
                  (cond
                    ((opening-paren-p char)
                     (push (list char position) stack))
                    ((closing-paren-p char)
                     (if (null stack)
                         (multiple-value-bind (line column)
                             (calculate-line-column text position)
                           (error 'paren-error
                                  :position position
                                  :expected "opening parenthesis"
                                  :found char
                                  :line line
                                  :column column))
                         (let* ((top (pop stack))
                                (opening-char (first top))
                                (expected-closing (matching-paren opening-char)))
                           (unless (char= char expected-closing)
                             (multiple-value-bind (line column)
                                 (calculate-line-column text position)
                               (error 'paren-error
                                      :position position
                                      :expected expected-closing
                                      :found char
                                      :line line
                                      :column column)))))))))
               (incf position)))
    (when stack
      (let* ((unclosed (first stack))
             (unclosed-pos (second unclosed))
             (unclosed-char (first unclosed)))
        (multiple-value-bind (line column)
            (calculate-line-column text unclosed-pos)
          (error 'paren-error
                 :position unclosed-pos
                 :expected (matching-paren unclosed-char)
                 :found "end of file"
                 :line line
                 :column column))))
    t))

(defun check-parens-file (filename)
  "Check parentheses matching in a Common Lisp source file.
Returns T if all parentheses match, otherwise signals a PAREN-ERROR."
  (with-open-file (stream filename :direction :input)
    (let* ((file-size (file-length stream))
           (content (make-string file-size)))
      (when (> file-size 0)
        (read-sequence content stream))
      (check-parens content))))

(defun main (&optional filename)
  "Main function for command-line usage"
  (if filename
      (handler-case
          (progn
            (check-parens-file filename)
            (format t "No parentheses errors found in ~A~%" filename)
            0)
        (paren-error (e)
          (format t "Error in ~A: ~A~%" filename e)
          1)
        (error (e)
          (format t "Error reading file ~A: ~A~%" filename e)
          1))
      (progn
        (format t "Usage: paren-checker.lisp filename.lisp~%")
        1)))

;; Script entry point when run directly
(when (and (boundp 'sb-ext:*posix-argv*) sb-ext:*posix-argv*)
  (let ((filename (second sb-ext:*posix-argv*)))
    (sb-ext:exit :code (main filename))))