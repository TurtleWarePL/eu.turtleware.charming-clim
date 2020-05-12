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

(defmacro with-console (opts &body body)
  (declare (ignore opts))
  (let ((handler (gensym)))
    `(let ((,handler (enable-raw)))
       (unwind-protect (progn ,@body)
         (disable-raw ,handler)))))

(declaim (notinline show-screen))
(defun show-screen ()
  (format *console-io* "~acHello World!" #\esc))

(defun start-display ()
  (swank:create-server)
  (with-console ()
    (loop (sleep 1)
          (show-screen))))
