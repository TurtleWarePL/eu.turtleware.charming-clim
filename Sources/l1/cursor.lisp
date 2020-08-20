(in-package #:eu.turtleware.charming-clim)

;;; Cursor protocol:

(defgeneric change-cursor-enabledp (cursor enabledp)
  (:documentation "Changes the cursor state."))

(defgeneric change-cursor-visiblep (cursor visiblep)
  (:documentation "Changes the cursor visibility."))

(defgeneric change-cursor-position (cursor row col)
  (:documentation "Changes the cursor position."))

(defgeneric change-cursor-data (cursor data)
  (:documentation "Changes the cursor data."))

(defgeneric change-cursor-inks (cursor fgc bgc)
  (:documentation "Changes the cursor colors."))

(defgeneric change-cursor-text (cursor &rest args
                                &key
                                  intensity underline italicized
                                  crossout blink inverse invisible)
  (:documentation "Changes the cursor text properties."))

(defgeneric cursor-enabledp (cursor)
  (:documentation "Returns a flag whether the cursor is enabled."))

(defgeneric cursor-visiblep (cursor)
  (:documentation "Returns a flag whether the cursor is visible."))

(defgeneric cursor-position (cursor)
  (:documentation "Returns two values: current ROW and COL."))

(defgeneric cursor-data (cursor)
  (:documentation "Returns the cursor data."))

(defgeneric cursor-inks (cursor)
  (:documentation "Returns the cursor foreground and background colors."))

(defgeneric cursor-text (cursor)
  (:documentation "Returns a plist with the text properties."))

;;; This is a mixin class for the pen properties.
(defclass drawing-style-mixin ()
  ((fgc :initarg :fgc :accessor fgc :documentation "Foreground color")
   (bgc :initarg :bgc :accessor bgc :documentation "Background color")
   (txt :initarg :txt :accessor txt :documentation "Text properties"
        :reader cursor-text))
  (:default-initargs :fgc #x222222ff :bgc #xddddddff
                     :txt '(:intensity :normal
                            :underline :none
                            :italicized nil
                            :crossout   nil
                            :blink      nil
                            :inverse    nil
                            :invisible  nil)))

(defmethod change-cursor-inks ((pen drawing-style-mixin) fgc bgc)
  (when fgc (setf (fgc pen) fgc))
  (when bgc (setf (bgc pen) bgc)))

(defmethod cursor-inks ((pen drawing-style-mixin))
  (values (fgc pen) (bgc pen)))

(defmethod change-cursor-text ((pen drawing-style-mixin) &rest args)
  (loop with txt = (txt pen)
        for (arg val) on args by #'cddr
        do (unless (eql val (getf txt arg))
             (setf (getf txt arg) val))
        finally
           (setf (txt pen) txt)))

(defclass cursor (drawing-style-mixin)
  ((cep :initarg :cep :reader cep :writer set-cep :reader cursor-enabledp)
   (cvp :initarg :cvp :reader cvp :writer set-cvp :reader cursor-visiblep)
   (row :initarg :row :reader row :writer set-row)
   (col :initarg :col :reader col :writer set-col)
   (obj :initarg :obj :reader obj :writer set-obj :reader cursor-data))
  (:default-initargs :cep t
                     :cvp nil
                     :row 1
                     :col 1))

(defmethod change-cursor-enabledp ((cur cursor) enabledp)
  (set-cep enabledp cur))

(defmethod change-cursor-visiblep ((cur cursor) visiblep)
  (set-cvp visiblep cur))

(defmethod change-cursor-position ((cur cursor) row col)
  (set-row row cur)
  (set-col col cur))

(defmethod cursor-position ((cur cursor))
  (values (row cur) (col cur)))

(defmethod change-cursor-data ((cur cursor) data)
  (set-obj data cur))


(defclass tcursor (cursor) ()
  (:documentation "The terminal cursor."))

(defmethod initialize-instance :after
    ((instance tcursor) &rest args
     &key row col cep cvp fgc bgc txt)
  (declare (ignore args))
  (set-cursor-position row col)
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
    (when (or set-row-p set-col-p)
      (set-cursor-position row col))))

(defmethod change-cursor-inks ((pen tcursor) fgc bgc)
  (when fgc
    (unless (eql fgc (fgc pen))
      (set-foreground-color fgc)
      (setf (fgc pen) fgc)))
  (when bgc
    (unless (eql bgc (bgc pen))
      (set-background-color bgc)
      (setf (bgc pen) bgc))))

(defmethod change-cursor-text ((pen tcursor) &rest args)
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
  (:documentation "The pointer.")
  (:default-initargs :fgc #xff8888ff :bgc #x88ffffff))

(defmethod initialize-instance :after
    ((instance pointer) &rest args)
  (set-mouse-tracking (cursor-enabledp instance)))

(defmethod change-cursor-enabledp :before ((cur pointer) enabledp)
  (unless (eq enabledp (cursor-enabledp instance))
    (set-mouse-tracking enabledp)))

(defclass vpointer (cursor) ()
  (:documentation "The virtual pointer."))
