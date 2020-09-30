(in-package #:eu.turtleware.charming-clim)

(defclass surface (bbox output-buffer)
  ((sink :initarg :sink :accessor sink :documentation "Flush destination")
   (row0 :initarg :row0 :accessor row0 :documentation "Scroll row offset")
   (col0 :initarg :col0 :accessor col0 :documentation "Scroll col offset"))
  (:default-initargs :row0 0 :col0 0 :sink *buffer*))

(defun offset (surface)
  (values (row0 surface) (col0 surface)))

(defmethod direct-cursor ((buffer surface) &aux (sink (sink buffer)))
  (ecase (mode sink)
    ((:buf :wrt)
     (buffer-cursor sink))
    (:dir
     (direct-cursor sink))))

(defmethod initialize-instance :after
    ((buf surface) &key data rows cols r1 c1 r2 c2)
  (destructuring-bind (d0 d1) (array-dimensions data)
    (unless rows
      (if (not (zerop d0))
          (setf rows d0)
          (setf rows (1+ (- r2 r1))))
      (setf (rows buf) rows))
    (unless cols
      (if (not (zerop d1))
          (setf cols d1)
          (setf cols (1+ (- c2 c1))))
      (setf (cols buf) cols)))
  (let ((clip (clip buf)))
    (setf (r2 clip) rows
          (c2 clip) cols))
  (adjust-array (data buf) (list rows cols) :initial-element nil))

;;; update cursor!
(defmethod put-cell ((buf surface) str
                     &rest cursor-args
                     &key row col &allow-other-keys
                     &aux (sink (sink buf)))
  (remf cursor-args :row)
  (remf cursor-args :col)
  (multiple-value-bind (r1 c1 r2 c2) (bbox buf)
    (multiple-value-bind (row0 col0) (offset buf)
      (iterate-cells (chr crow ccol wrap-p)
          (buf row col (string str))
        (when (inside-p buf row col)
          (let ((vrow (- (+ r1 row) row0 1))
                (vcol (- (+ c1 col) col0 1)))
            (when (and (<= r1 vrow r2)
                       (<= c1 vcol c2))
              (apply #'set-cell sink chr :row vrow :col vcol cursor-args))))))))

(defmethod flush-output ((buffer surface) &rest args &key force)
  (declare (ignore args))
  (loop for row from 1 upto (rows buffer)
        do (loop for col from 1 upto (cols buffer)
                 for cell = (get-cell buffer row col)
                 when (or force (dirty-p cell))
                   do (put-cell buffer (chr cell) :row row
                                                  :col col
                                                  :fgc (fgc cell)
                                                  :bgc (bgc cell))
                      (setf (dirty-p cell) nil))))

(defun move-to-row (buf row0)
  (let* ((rows (rows buf))
         (height (1+ (- (r2 buf) (r1 buf))))
         (vrow1 (- 1    row0))
         (vrow2 (- rows row0)))
    (when (if (> height rows)
              (and (<= 1 vrow1 height)
                   (<= 1 vrow2 height))
              (and (<= vrow1 1)
                   (>= vrow2 height)))
      (setf (row0 buf) row0))))

(defun move-to-col (buf col0)
  (let* ((cols (cols buf))
         (width (1+ (- (c2 buf) (c1 buf))))
         (vcol1 (- 1    col0))
         (vcol2 (- cols col0)))
    (when (if (> width cols)
              (and (<= 1 vcol1 width)
                   (<= 1 vcol2 width))
              (and (<= vcol1 1)
                   (>= vcol2 width)))
      (setf (col0 buf) col0))))

#+ (or) ;; naive version
(defun scroll-buffer (buf row-dx col-dx)
  (incf (row0 buf) row-dx)
  (incf (col0 buf) col-dx))

(defun scroll-buffer (buf row-dx col-dx)
  (flet ((quantity (screen-size buffer-size dx)
           (if (alexandria:xor (> screen-size buffer-size)
                               (minusp dx))
               0
               (- buffer-size screen-size))))
    (unless (zerop row-dx)
      (let ((height (1+ (- (r2 buf) (r1 buf)))))
        (or (move-to-row buf (+ (row0 buf) row-dx))
            (setf (row0 buf)
                  (quantity height (rows buf) row-dx)))))
    (unless (zerop col-dx)
      (let ((width (1+ (- (c2 buf) (c1 buf)))))
        (or (move-to-col buf (+ (col0 buf) col-dx))
            (setf (col0 buf)
                  (quantity width (cols buf) col-dx)))))))

(defun move-buffer (buf row-dx col-dx)
  (incf (r1 buf) row-dx)
  (incf (r2 buf) row-dx)
  (incf (c1 buf) col-dx)
  (incf (c2 buf) col-dx))

(defun reshape-buffer (buf r1 c1 r2 c2)
  (setf (r1 buf) r1)
  (setf (c1 buf) c1)
  (setf (r2 buf) r2)
  (setf (c2 buf) c2)
  (scroll-buffer buf 0 0))
