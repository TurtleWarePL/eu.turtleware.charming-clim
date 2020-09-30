(defpackage #:eu.turtleware.charming-clim.examples
  (:use #:cl)
  (:local-nicknames (#:ax #:alexandria)
                    (#:l0 #:eu.turtleware.charming-clim/l0)
                    (#:l1 #:eu.turtleware.charming-clim/l1)
                    (#:fm #:eu.turtleware.charming-clim))
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


;;; common functions

(defconstant +white+  #xffffffff)
(defconstant +black+  #x000000ff)
(defconstant +grey0+  #x888888ff)
(defconstant +grey1+  #x444444ff)
(defconstant +grey2+  #xbbbbbbff)
(defconstant +purple+ #xff88ffff)
(defconstant +blue+   #x8888ffff)

;;; l1 helper functions
(defun draw-text (string row col &key (align :left))
  (flet ((do-it (row col)
           (l1:out (:row row :col col) string)))
    (ecase align
      (:left   (do-it row col))
      (:center (do-it row (- col (truncate (length string) 2))))
      (:right  (do-it row (- col (length string)))))))

(defun draw-peep (row col &optional (centerp t))
  (unless centerp
    (incf row 3)
    (incf col 6))
  (loop for drow from -3 upto +3
        do (loop for dcol from -3 upto +3
                 do (if (and (<= (+ (abs dcol) (abs drow)) 3)
                             (>= (+ (abs dcol) (abs drow)) 2))
                        (l1:out (:row (+ row drow)
                                 :col (+ col (* 2 dcol)))
                                (ax:random-elt '("HX" "30" "42")))
                        (l1:out (:row (+ row drow)
                                 :col (+ col (* 2 dcol)))
                                "  ")))))
