(in-package :tcode)

;;; PTY (Pseudo Terminal) interface using CFFI
;;; Provides proper terminal semantics for shell interaction


;; Constants
(defconstant +o-rdwr+ #x02)
(defconstant +o-noctty+ #x100)
(defconstant +tiocswinsz+ #x5414)
(defconstant +tiocgwinsz+ #x5413)

;; Structure for terminal window size
(cffi:defcstruct winsize
  (ws_row :unsigned-short)
  (ws_col :unsigned-short)
  (ws_xpixel :unsigned-short)
  (ws_ypixel :unsigned-short))

;; Terminal control constants
(defconstant +TCGETS+ #x5401)
(defconstant +TCSETS+ #x5402)
(defconstant +ICANON+ #x0002)
(defconstant +ECHO+ #x0008)
(defconstant +VMIN+ 6)
(defconstant +VTIME+ 5)

;; Terminal structure for termios
(cffi:defcstruct termios
  (c_iflag :uint32)
  (c_oflag :uint32)
  (c_cflag :uint32)
  (c_lflag :uint32)
  (c_line :uint8)
  (c_cc :uint8 :count 32))

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

;; Terminal control functions for curses-like interface
(defun get-terminal-attributes (fd)
  "Get terminal attributes for the given file descriptor"
  (cffi:with-foreign-object (termios-ptr '(:struct termios))
    (when (>= (%ioctl fd +TCGETS+ termios-ptr) 0)
      termios-ptr)))

(defun set-terminal-attributes (fd termios-ptr)
  "Set terminal attributes for the given file descriptor"
  (%ioctl fd +TCSETS+ termios-ptr))

(defvar *original-termios* nil "Storage for original terminal settings")

(defun enable-raw-mode (fd)
  "Enable raw mode on terminal (disable canonical mode and echo)"
  (unless *original-termios*
    (setf *original-termios* (cffi:foreign-alloc '(:struct termios))))

  (cffi:with-foreign-object (new-termios '(:struct termios))
    (when (>= (%ioctl fd +TCGETS+ *original-termios*) 0)
      ;; Copy original to new
      (cffi:foreign-slot-value *original-termios* '(:struct termios) 'c_iflag)
      (setf (cffi:foreign-slot-value new-termios '(:struct termios) 'c_iflag)
            (cffi:foreign-slot-value *original-termios* '(:struct termios) 'c_iflag))
      (setf (cffi:foreign-slot-value new-termios '(:struct termios) 'c_oflag)
            (cffi:foreign-slot-value *original-termios* '(:struct termios) 'c_oflag))
      (setf (cffi:foreign-slot-value new-termios '(:struct termios) 'c_cflag)
            (cffi:foreign-slot-value *original-termios* '(:struct termios) 'c_cflag))
      (setf (cffi:foreign-slot-value new-termios '(:struct termios) 'c_lflag)
            (cffi:foreign-slot-value *original-termios* '(:struct termios) 'c_lflag))
      (setf (cffi:foreign-slot-value new-termios '(:struct termios) 'c_line)
            (cffi:foreign-slot-value *original-termios* '(:struct termios) 'c_line))

      ;; Copy control characters
      (loop for i from 0 below 32 do
        (setf (cffi:mem-aref (cffi:foreign-slot-pointer new-termios '(:struct termios) 'c_cc) :uint8 i)
              (cffi:mem-aref (cffi:foreign-slot-pointer *original-termios* '(:struct termios) 'c_cc) :uint8 i)))

      (let ((lflag (cffi:foreign-slot-value new-termios '(:struct termios) 'c_lflag)))
        ;; Disable canonical mode and echo
        (setf (cffi:foreign-slot-value new-termios '(:struct termios) 'c_lflag)
              (logand lflag (lognot (logior +ICANON+ +ECHO+))))
        ;; Set VMIN=1, VTIME=0 for immediate character read
        (setf (cffi:mem-aref (cffi:foreign-slot-pointer new-termios '(:struct termios) 'c_cc) :uint8 +VMIN+) 1)
        (setf (cffi:mem-aref (cffi:foreign-slot-pointer new-termios '(:struct termios) 'c_cc) :uint8 +VTIME+) 0)
        (set-terminal-attributes fd new-termios)
        t))))

(defun disable-raw-mode (fd &optional dummy)
  "Restore original terminal attributes"
  (declare (ignore dummy))
  (when *original-termios*
    (set-terminal-attributes fd *original-termios*)
    (cffi:foreign-free *original-termios*)
    (setf *original-termios* nil)))

(defun read-char-raw ()
  "Read a single character from stdin without waiting for newline"
  (cffi:with-foreign-object (buffer :unsigned-char 1)
    (block read-char-block
      (handler-case
          (loop
            (let ((bytes-read (%read 0 buffer 1)))
              (when (> bytes-read 0)
                (return-from read-char-block (code-char (cffi:mem-aref buffer :unsigned-char 0))))))
        (sb-sys:interactive-interrupt ()
          ;; Convert interrupt signal to Ctrl-C character for proper handling
          (return-from read-char-block (code-char 3)))))))

(defun read-escape-sequence ()
  "Read the remainder of an escape sequence after ESC has been read"
  (cffi:with-foreign-object (buffer :unsigned-char 1)
    (let ((sequence "")
          (timeout-counter 0)
          (max-timeout 10)) ; Small timeout to distinguish standalone ESC from escape sequences
      (loop
        (let ((bytes-read (%read 0 buffer 1)))
          (cond
            ((> bytes-read 0)
             (let ((char (code-char (cffi:mem-aref buffer :unsigned-char 0))))
               (setf sequence (concatenate 'string sequence (string char)))
               (setf timeout-counter 0)
               ;; Check if we have a complete sequence
               (cond
                 ;; Page Up: ESC[5~
                 ((string= sequence "[5~") (return sequence))
                 ;; Page Down: ESC[6~
                 ((string= sequence "[6~") (return sequence))
                 ;; Arrow keys: ESC[A, ESC[B, ESC[C, ESC[D
                 ((and (>= (length sequence) 2)
                       (char= (char sequence 0) #\[)
                       (member (char sequence 1) '(#\A #\B #\C #\D)))
                  (return sequence))
                 ;; If sequence gets too long, assume it's not what we're looking for
                 ((> (length sequence) 10) (return sequence)))))
            (t
             ;; No more data available - increment timeout
             (incf timeout-counter)
             (when (> timeout-counter max-timeout)
               ;; Timeout reached - likely standalone ESC
               (return sequence))
             ;; Small delay before next check
             (sleep 0.001))))))))

;; Terminal output functions
(defun move-cursor (row col)
  "Move cursor to specified position (1-based)"
  (format t "~C[~D;~DH" #\Escape row col)
  (force-output))

(defun clear-screen ()
  "Clear the entire screen"
  (format t "~C[2J" #\Escape)
  (move-cursor 1 1))

(defun clear-line ()
  "Clear the current line"
  (format t "~C[2K" #\Escape)
  (force-output))

(defun save-cursor ()
  "Save current cursor position"
  (format t "~C[s" #\Escape)
  (force-output))

(defun restore-cursor ()
  "Restore saved cursor position"
  (format t "~C[u" #\Escape)
  (force-output))

(defun set-color (fg &optional bg)
  "Set foreground and optionally background color"
  (if bg
      (format t "~C[~D;~Dm" #\Escape (+ 30 fg) (+ 40 bg))
      (format t "~C[~Dm" #\Escape (+ 30 fg)))
  (force-output))

(defun reset-color ()
  "Reset colors to default"
  (format t "~C[0m" #\Escape)
  (force-output))

(defun get-terminal-size ()
  "Get the current terminal size as (rows columns)"
  (cffi:with-foreign-object (ws '(:struct winsize))
    (if (>= (%ioctl 1 +tiocgwinsz+ ws) 0)  ; stdout fd = 1
        (values (cffi:foreign-slot-value ws '(:struct winsize) 'ws_row)
                (cffi:foreign-slot-value ws '(:struct winsize) 'ws_col))
        (values 24 80))))  ; fallback to default size
