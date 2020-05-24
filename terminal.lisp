(defpackage #:eu.turtleware.charming-clim
  (:use #:cl)
  (:export #:start-display))
(in-package #:eu.turtleware.charming-clim)

;; gcc raw-mode.c -shared -o raw-mode.so
;; (cffi:load-foreign-library "/path/to/raw-mode.so")

(cffi:defcfun (enable-raw "enable_raw")
    :pointer)

(cffi:defcfun (disable-raw "disable_raw")
    :void
  (handler :pointer))

(defvar *console-io* *terminal-io*)


(defun put (&rest args)
  "Put raw string on a console"
  (format *console-io* "~{~a~}" args)
  (finish-output *console-io*))

(defun esc (&rest args)
  "Escape sequence"
  (apply #'put #\esc args))

(defun csi (&rest args)
  "Control sequence introducer"
  (apply #'esc #\[ args))

(defun sgr (&rest args)
  "Select Graphic Rendition"
  (apply #'csi (append args '("m"))))


(defun reset-console ()
  "Clears the screen, attributes, cursor position etc."
  (esc "c"))

(defun clear-console (&optional (mode 2))
  "Erase in display"
  ;; Defined modes:
  ;; 0 - clear from cursor to the end of the display
  ;; 1 - clear from cursor to the start of the display
  ;; 2 - clear entire display
  (csi mode "J"))

(defun clear-line (&optional (mode 2))
  "Erase in line"
  ;; Defined modes:
  ;; 0 - clear from cursor to the end of the line
  ;; 1 - clear from cursor to the start of the line
  ;; 2 - clear entire line
  (csi mode "K"))

(defun set-foreground-color (r g b)
  (sgr "38;2;" r ";" g ";" b))

(defun set-background-color (r g b)
  (sgr "48;2;" r ";" g ";" b))

(defun save-cursor-position ()
  (csi "s"))

(defun restore-cursor-position ()
  (csi "u"))

(defun set-cursor-position (row col)
  (cond ((and row col)    (csi row ";" col "H"))
        ((not (null row)) (csi row ";H"))
        ((not (null col)) (csi ";" col "H"))))

(defmacro with-cursor-position ((row col) &body body)
  `(progn
     (save-cursor-position)
     (set-cursor-position ,row ,col)
     (unwind-protect (progn ,@body)
       (restore-cursor-position))))

(defun (setf cursor-visibility) (visiblep)
  (if visiblep
      (csi "?" 2 5 "h")
      (csi "?" 2 5 "l")))


(defmacro with-console (opts &body body)
  (declare (ignore opts))
  (let ((handler (gensym)))
    `(let ((,handler (enable-raw)))
       (unwind-protect (progn ,@body)
         (disable-raw ,handler)))))

(declaim (notinline show-screen))
(defun show-screen ()
  (set-cursor-position (1+ (random 24))
                       (1+ (random 80)))
  (if (zerop (random 2))
      (put "+")
      (put "-")))

(defparameter *conf*
  (list :sleep 1/60
        :cursorp nil
        :foreground '(#xff #xa0 #xa0)
        :background '(#x00 #x22 #x22)))

(defun start-display ()
  (swank:create-server)
  (with-console ()
    (loop with conf
          with seconds
          do (unless (equalp conf *conf*)
               (setf conf (copy-list *conf*))
               (destructuring-bind (&key sleep cursorp foreground background)
                   conf
                 (setf seconds sleep)
                 (reset-console)
                 (setf (cursor-visibility) cursorp)
                 (apply #'set-background-color background)
                 (apply #'set-foreground-color foreground)
                 (clear-console)))
             (sleep seconds)
             (show-screen))))
