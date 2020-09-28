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

(defgeneric change-cursor-text (cursor txt)
  (:documentation "Changes the cursor text style."))

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
  (:documentation "Returns the cursor text style."))

(defclass cursor (drawing-style-mixin)
  ((cep :initarg :cep :reader cep :writer set-cep :reader cursor-enabledp)
   (cvp :initarg :cvp :reader cvp :writer set-cvp :reader cursor-visiblep)
   (row :initarg :row :reader row :writer set-row)
   (col :initarg :col :reader col :writer set-col)
   (obj :initarg :obj :reader obj :writer set-obj :reader cursor-data))
  (:default-initargs :cep t
                     :cvp nil
                     :row 1
                     :col 1
                     :obj nil))

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

(defmethod change-cursor-inks ((pen cursor) fgc bgc)
  (when fgc (setf (fgc pen) fgc))
  (when bgc (setf (bgc pen) bgc)))

(defmethod cursor-inks ((pen cursor))
  (values (fgc pen) (bgc pen)))

(defmethod change-cursor-text ((pen cursor) txt)
  (setf (txt pen) (fuze-text-style txt (txt pen))))

(defmethod cursor-text ((pen cursor))
  (txt pen))

;;; Helper functions

(defun update-pen (cursor &key row col fgc bgc txt)
  (change-cursor-position cursor row col)
  (change-cursor-inks cursor fgc bgc)
  (change-cursor-text cursor txt))

(defun update-pen* (cursor src)
  (update-pen cursor :txt (txt src)
                     :row (row src) :col (col src)
                     :fgc (fgc src) :bgc (bgc src)))

(defun return-pen (cursor)
  (multiple-value-bind (row col) (cursor-position cursor)
    (multiple-value-bind (fgc bgc) (cursor-inks cursor)
      (list :row row :col col :fgc fgc :bgc bgc :txt (cursor-text cursor)))))

(defmacro with-modified-pen ((cursor cursor-args) &body body)
  (alexandria:with-gensyms (old-row old-col old-fgc old-bgc old-txt)
    (alexandria:once-only (cursor)
      `(multiple-value-bind (,old-row ,old-col) (cursor-position ,cursor)
         (multiple-value-bind (,old-fgc ,old-bgc) (cursor-inks ,cursor)
           (let ((,old-txt (cursor-text ,cursor)))
             (apply #'update-pen ,cursor ,cursor-args)
             (unwind-protect (progn ,@body)
               (update-pen ,cursor
                           :row ,old-row :col ,old-col
                           :fgc ,old-fgc :bgc ,old-bgc
                           :txt ,old-txt))))))))


(defclass tcursor (cursor) ()
  (:documentation "The terminal cursor."))

(defmethod initialize-instance :after
    ((instance tcursor) &rest args
     &key row col cep cvp fgc bgc txt)
  (declare (ignore args cep))
  (set-cursor-position row col)
  (set-cursor-visibility cvp)
  (set-cursor-position row col)
  (set-foreground-color fgc)
  (set-background-color bgc)
  (set-text-style txt))

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

;;; something wrong here!(!)[!]
(defmethod change-cursor-text ((pen tcursor) txt)
  (let ((diff (text-style-diff txt (txt pen))))
    (setf (txt pen) (fuze-text-style txt (txt pen)))
    (set-text-style diff)))


;;; Slots ROW and COL coalasce from both superclasses.
(defclass %pointer (cursor)
  ()
  (:documentation "A pointer."))


(defclass pointer (%pointer)
  ()
  (:documentation "The physical pointer.")
  (:default-initargs :fgc #xff0000ff :bgc #x00000000))

(defmethod initialize-instance :after
    ((instance pointer) &rest args)
  (declare (ignore args))
  (set-mouse-tracking (cursor-enabledp instance)))

(defmethod change-cursor-enabledp :before ((cur pointer) enabledp)
  (unless (eq enabledp (cursor-enabledp cur))
    (set-mouse-tracking enabledp)))


(defclass vpointer (%pointer)
  ((toggled-btn :initarg :toggled-btn :accessor toggled-btn))
  (:documentation "The virtual pointer.")
  (:default-initargs :fgc #x00ff00ff :bgc #x22222200
                     :toggled-btn :none))

(defmethod initialize-instance :after
    ((instance vpointer) &rest args)
  (declare (ignore args))
  (change-cursor-data instance
                      (make-instance 'pointer-event :pointer instance)))
