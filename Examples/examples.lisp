(defpackage #:eu.turtleware.charming-clim.examples
  (:use #:cl)
  (:local-nicknames (#:ax #:alexandria)
                    (#:l0 #:eu.turtleware.charming-clim/l0)
                    (#:l1 #:eu.turtleware.charming-clim/l1))
  (:export #:start))

(in-package #:eu.turtleware.charming-clim.examples)

(defvar *examples* (make-hash-table))

(defun register-example (name function)
  (setf (gethash name *examples*) function))

(defun start (&optional example)
  (flet ((run-example (name)
           (ax:if-let ((example (gethash name *examples*)))
             (funcall example)
             (warn "Example ~s does not exist.~%" name))))
    (if example
        (run-example example)
        (loop (format t "Examples:~%~{~s~%~}> "
                      (ax:hash-table-keys *examples*))
              (finish-output)
              (let ((key (read)))
                (if (eq key :quit)
                    (return)
                    (run-example key)))))))
