(in-package #:eu.turtleware.charming-clim)

;;; Cursor protocol:

(defgeneric change-cursor-visiblep (cursor visiblep)
  (:documentation "Changes the cursor visibility."))

(defgeneric change-cursor-position (cursor row col)
  (:documentation "Changes the cursor position."))

(defgeneric change-cursor-pen (cursor &rest args
                               &key fgc bgc
                                 intensity underline italicized
                                 crossout blink inverse invisible)
  (:documentation "Changes the cursor pen properties."))

(defgeneric cursor-visiblep (cursor)
  (:documentation "Returns a flag whether the cursor is visible."))

(defgeneric cursor-position (cursor)
  (:documentation "Returns two values: current ROW and COL."))

(defgeneric cursor-pen (cursor)
  (:documentation "Returns a plist with the pen properties."))


;;; This is a mixin class for the pen properties.
(defclass drawing-style-mixin ()
  ((fgc :initarg :fgc :accessor fgc :documentation "Foreground color")
   (bgc :initarg :bgc :accessor bgc :documentation "Background color")
   (txt :initarg :txt :accessor txt :documentation "Text properties"))
  (:default-initargs :fgc #x222222ff :bgc #xddddddff
                     :txt '(:intensity :normal
                            :underline :none
                            :italicized nil
                            :crossout   nil
                            :blink      nil
                            :inverse    nil
                            :invisible  nil)))

(defmethod change-cursor-pen ((pen drawing-style-mixin)
                              &rest args)
  (alexandria:when-let ((fgc (getf args :fgc)))
    (setf (fgc pen) fgc)
    (remf args :fgc))
  (alexandria:when-let ((bgc (getf args :bgc)))
    (setf (bgc pen) bgc)
    (remf args :bgc))
  (loop with txt = (txt pen)
        for (arg val) on args by #'cddr
        do (unless (eql val (getf txt arg))
             (setf (getf txt arg) val))
        finally
           (setf (txt pen) txt)))

(defmethod cursor-pen ((pen drawing-style-mixin))
  (list* :fgc (fgc pen) :bgc (bgc pen) (copy-list (txt pen))))

(defclass cursor (drawing-style-mixin)
  ((cvp :initarg :cvp :reader cvp :writer set-cvp)
   (row :initarg :row :reader row :writer set-row)
   (col :initarg :col :reader col :writer set-col))
  (:default-initargs :cvp nil
                     :row 1
                     :col 1))

(defmethod change-cursor-visiblep ((cur cursor) visiblep)
  (set-cvp visiblep cur))

(defmethod change-cursor-position ((cur cursor) row col)
  (set-row row cur)
  (set-col col cur))


(defclass tcursor (cursor) ()
  (:documentation "The terminal cursor."))

(defmethod initialize-instance :after
    ((instance tcursor) &rest args
     &key row col cvp fgc bgc txt)
  (declare (ignore args))
  (set-cursor-visibility cvp)
  (set-cursor-position row col)
  (set-foreground-color fgc)
  (set-background-color bgc)
  (apply #'set-text-style txt))

(defmethod change-cursor-visiblep :before ((cur tcursor) visiblep)
  (unless (eql visiblep (cvp cur))
    (set-cursor-visibility visiblep)))

(defmethod change-cursor-position :before ((cur tcursor) row col)
  (let ((set-row-p (not (eql (row cur) row)))
        (set-col-p (not (eql (col cur) col))))
    (cond ((and set-row-p set-col-p)
           (set-cursor-position row col))
          (set-row-p
           (set-cursor-position row nil))
          (set-col-p
           (set-cursor-position nil col)))))

(defmethod change-cursor-pen ((pen drawing-style-mixin)
                              &rest args)
  (alexandria:when-let ((fgc (getf args :fgc)))
    (unless (eql fgc (fgc pen))
      (set-foreground-color fgc)
      (setf (fgc pen) fgc))
    (remf args :fgc))
  (alexandria:when-let ((bgc (getf args :bgc)))
    (unless (eql bgc (bgc pen))
      (set-background-color bgc)
      (setf (bgc pen) bgc))
    (remf args :bgc))
  (loop with txt = (txt pen)
        for (arg val) on args by #'cddr
        unless (eql val (getf txt arg))
          do (setf (getf txt arg) val)
          and collect arg into dif
          and collect val into dif
        finally
           (setf (txt pen) txt)
           (apply #'set-text-style dif)))


(defclass pointer (cursor) ()
  (:documentation "The terminal pointer."))

(defmethod initialize-instance :after
    ((instance pointer) &rest args)
  (set-mouse-tracking t))
