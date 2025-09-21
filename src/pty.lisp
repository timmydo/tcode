(in-package :tcode)

;;; PTY (Pseudo Terminal) interface using CFFI
;;; Provides proper terminal semantics for shell interaction


;; Constants
(defconstant +o-rdwr+ #x02)
(defconstant +o-noctty+ #x100)
(defconstant +tiocswinsz+ #x5414)

;; Structure for terminal window size
(cffi:defcstruct winsize
  (ws_row :unsigned-short)
  (ws_col :unsigned-short)
  (ws_xpixel :unsigned-short)
  (ws_ypixel :unsigned-short))

;; CFFI bindings for PTY system calls
(cffi:defcfun ("posix_openpt" %posix-openpt) :int
  (flags :int))

(cffi:defcfun ("grantpt" %grantpt) :int
  (fd :int))

(cffi:defcfun ("unlockpt" %unlockpt) :int
  (fd :int))

(cffi:defcfun ("ptsname" %ptsname) :string
  (fd :int))

(cffi:defcfun ("open" %open) :int
  (pathname :string)
  (flags :int))

(cffi:defcfun ("close" %close) :int
  (fd :int))

(cffi:defcfun ("read" %read) :long
  (fd :int)
  (buf :pointer)
  (count :size))

(cffi:defcfun ("write" %write) :long
  (fd :int)
  (buf :pointer)
  (count :size))

(cffi:defcfun ("fork" %fork) :int)

(cffi:defcfun ("setsid" %setsid) :int)

(cffi:defcfun ("ioctl" %ioctl) :int
  (fd :int)
  (cmd :unsigned-long)
  (arg :pointer))

(cffi:defcfun ("dup2" %dup2) :int
  (oldfd :int)
  (newfd :int))

(cffi:defcfun ("execvp" %execvp) :int
  (file :string)
  (argv :pointer))

(cffi:defcfun ("waitpid" %waitpid) :int
  (pid :int)
  (status :pointer)
  (options :int))

(cffi:defcfun ("kill" %kill) :int
  (pid :int)
  (sig :int))

;; PTY class for managing pseudo terminals
(defclass pty ()
  ((master-fd :accessor pty-master-fd :initform -1)
   (slave-fd :accessor pty-slave-fd :initform -1)
   (slave-name :accessor pty-slave-name :initform nil)
   (child-pid :accessor pty-child-pid :initform -1)
   (width :accessor pty-width :initform 80 :initarg :width)
   (height :accessor pty-height :initform 24 :initarg :height)
   (running :accessor pty-running-p :initform nil))
  (:documentation "A pseudo terminal interface"))

(defun make-pty (&key (width 80) (height 24))
  "Create a new PTY instance"
  (make-instance 'pty :width width :height height))

(defun open-pty (pty)
  "Open a PTY master/slave pair"
  (let ((master-fd (%posix-openpt (logior +o-rdwr+ +o-noctty+))))
    (when (< master-fd 0)
      (error "Failed to create PTY master"))
    
    (when (< (%grantpt master-fd) 0)
      (%close master-fd)
      (error "Failed to grant PTY"))
    
    (when (< (%unlockpt master-fd) 0)
      (%close master-fd)
      (error "Failed to unlock PTY"))
    
    (let ((slave-name (%ptsname master-fd)))
      (unless slave-name
        (%close master-fd)
        (error "Failed to get PTY slave name"))
      
      (setf (pty-master-fd pty) master-fd)
      (setf (pty-slave-name pty) slave-name)
      pty)))

(defun close-pty (pty)
  "Close PTY file descriptors"
  (when (>= (pty-master-fd pty) 0)
    (%close (pty-master-fd pty))
    (setf (pty-master-fd pty) -1))
  (when (>= (pty-slave-fd pty) 0)
    (%close (pty-slave-fd pty))
    (setf (pty-slave-fd pty) -1))
  (setf (pty-running-p pty) nil))

(defun set-pty-window-size (pty width height)
  "Set the window size for the PTY"
  (when (>= (pty-master-fd pty) 0)
    (cffi:with-foreign-object (ws '(:struct winsize))
      (setf (cffi:foreign-slot-value ws '(:struct winsize) 'ws_row) height)
      (setf (cffi:foreign-slot-value ws '(:struct winsize) 'ws_col) width)
      (setf (cffi:foreign-slot-value ws '(:struct winsize) 'ws_xpixel) 0)
      (setf (cffi:foreign-slot-value ws '(:struct winsize) 'ws_ypixel) 0)
      (%ioctl (pty-master-fd pty) +tiocswinsz+ ws))
    (setf (pty-width pty) width)
    (setf (pty-height pty) height)))

(defun pty-read-bytes (pty buffer size)
  "Read bytes from PTY master"
  (when (>= (pty-master-fd pty) 0)
    (%read (pty-master-fd pty) buffer size)))

(defun pty-write-bytes (pty buffer size)
  "Write bytes to PTY master"
  (when (>= (pty-master-fd pty) 0)
    (%write (pty-master-fd pty) buffer size)))

(defun pty-write-string (pty string)
  "Write a string to PTY master"
  (let* ((bytes (babel:string-to-octets string :encoding :utf-8))
         (len (length bytes)))
    (cffi:with-foreign-object (buffer :unsigned-char len)
      (loop for i from 0 below len
            do (setf (cffi:mem-aref buffer :unsigned-char i) (aref bytes i)))
      (pty-write-bytes pty buffer len))))

(defun spawn-process-in-pty (pty shell-command)
  "Spawn a shell process in the PTY slave"
  (unless (pty-slave-name pty)
    (error "PTY not properly initialized"))
  
  (let ((pid (%fork)))
    (cond
      ((< pid 0)
       (error "Failed to fork process"))
      
      ((= pid 0)
       ;; Child process
       (handler-case
           (progn
             ;; Create new session
             (%setsid)
             
             ;; Open slave PTY
             (let ((slave-fd (%open (pty-slave-name pty) +o-rdwr+)))
               (when (< slave-fd 0)
                 (error "Failed to open PTY slave"))
               
               ;; Redirect stdin, stdout, stderr to slave PTY
               (%dup2 slave-fd 0)  ; stdin
               (%dup2 slave-fd 1)  ; stdout
               (%dup2 slave-fd 2)  ; stderr
               
               ;; Close original slave fd if different
               (when (> slave-fd 2)
                 (%close slave-fd))
               
               ;; Close master fd in child
               (when (>= (pty-master-fd pty) 0)
                 (%close (pty-master-fd pty)))
               
               ;; Set up environment
               (cffi:with-foreign-objects ((argv :pointer 3))
                 (cffi:with-foreign-strings ((shell-str shell-command)
                                            (interactive-str "-i")
                                            (null-str ""))
                   (setf (cffi:mem-aref argv :pointer 0) shell-str)
                   (setf (cffi:mem-aref argv :pointer 1) interactive-str)
                   (setf (cffi:mem-aref argv :pointer 2) (cffi:null-pointer))
                   (%execvp shell-command argv)))))
         (error (e)
           (format t "Child process error: ~A~%" e)
           (cffi:foreign-funcall "exit" :int 1 :void))))
      
      (t
       ;; Parent process
       (setf (pty-child-pid pty) pid)
       (setf (pty-running-p pty) t)
       
       ;; Set initial window size
       (set-pty-window-size pty (pty-width pty) (pty-height pty))
       
       pty))))

(defun pty-process-running-p (pty)
  "Check if the PTY child process is still running"
  (when (and (pty-running-p pty) (> (pty-child-pid pty) 0))
    (cffi:with-foreign-object (status :int)
      (let ((result (%waitpid (pty-child-pid pty) status 1))) ; WNOHANG = 1
        (cond
          ((= result 0) t)  ; Process still running
          ((> result 0)     ; Process exited
           (setf (pty-running-p pty) nil)
           nil)
          (t t))))))      ; Error, assume still running

(defun kill-pty-process (pty &optional (signal 15))
  "Kill the PTY child process (default SIGTERM)"
  (when (and (pty-running-p pty) (> (pty-child-pid pty) 0))
    (%kill (pty-child-pid pty) signal)
    (setf (pty-running-p pty) nil)))

(defun cleanup-pty (pty)
  "Clean up PTY resources"
  (kill-pty-process pty)
  (close-pty pty))
