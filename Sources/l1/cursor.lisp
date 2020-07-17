(in-package #:eu.turtleware.charming-clim)

(defclass cursor ()
  ((cvp :initarg :cvp :accessor cvp :documentation "Cursor visible?")
   (row :initarg :row :accessor row :documentation "Cursor row")
   (col :initarg :col :accessor col :documentation "Cursor col")
   (fgc :initarg :fgc :accessor fgc :documentation "Foreground color")
   (bgc :initarg :bgc :accessor bgc :documentation "Background color"))
  (:default-initargs :cvp nil :fgc nil :bgc nil :row nil :col nil))

(defmethod initialize-instance :after
    ((instance cursor) &rest args &key fgc bgc row col cvp)
  (declare (ignore args))
  (set-cursor-visibility cvp)
  (set-cursor-position row col)
  (set-foreground-color fgc)
  (set-foreground-color bgc))

(defmethod (setf cvp) :before (cvp (cur cursor))
  (unless (eql cvp (cvp cur))
    (set-cursor-visibility cvp)))

(defun update-cursor-position (cursor row col)
  (setf (slot-value cursor 'row) row
        (slot-value cursor 'col) col))

(defsetf cursor-position (cursor) (row col)
  `(let ((crow (row ,cursor))
         (ccol (col ,cursor)))
     (cond ((not (or (eql crow ,row)
                     (eql ccol ,col)))
            (set-cursor-position ,row ,col))
           ((not (eql crow ,row))
            (set-cursor-position ,row ccol))
           ((not (eql ccol ,col))
            (set-cursor-position crow ,col)))
     (update-cursor-position ,cursor ,row ,col)
     (values ,row ,col)))

(defmethod (setf fgc) :before (fgc (cur cursor))
  (unless (eql fgc (fgc cur))
    (set-foreground-color fgc)))

(defmethod (setf bgc) :before (bgc (cur cursor))
  (unless (eql bgc (bgc cur))
    (set-background-color bgc)))

(defsetf cursor-colors (cursor) (fgc bgc)
  `(progn (setf (fgc ,cursor) ,fgc
                (bgc ,cursor) ,bgc)
          (values ,fgc ,bgc)))
