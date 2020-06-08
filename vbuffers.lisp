(in-package #:eu.turtleware.charming-clim)

(defgeneric get-cell (buffer row col))
(defgeneric put-cell (buffer row col))
(defgeneric set-cell (buffer row col ch fg bg))
(defgeneric inside-p (buffer row col))
(defgeneric flush-buffer (buffer &key r1 c1 r2 c2 force))

(defclass vbuffer ()
  ((fgc  :initarg :fgc  :accessor fgc  :documentation "Foregorund color")
   (bgc  :initarg :bgc  :accessor bgc  :documentation "Background color")
   (row  :initarg :row  :accessor row  :documentation "Current row")
   (col  :initarg :col  :accessor col  :documentation "Current col")
   (clip :initarg :clip :accessor clip :documentation "Clipping object")
   (data :initarg :data :accessor data :documentation "Data buffer")
   (rows :initarg :rows :accessor rows :documentation "Buffer number of rows")
   (cols :initarg :cols :accessor cols :documentation "Buffer number of cols"))
  (:default-initargs :fgc #xffa0a0
                     :bgc #x222222
                     :row 1
                     :col 1
                     :clip (make-instance 'vclip)))

(defclass vcell ()
  ((ch :initarg :ch :accessor ch)
   (fg :initarg :fg :accessor fg)
   (bg :initarg :bg :accessor bg)
   (dirty-p :initarg :dirty-p :accessor dirty-p))
  (:default-initargs :ch #\space
                     :fg (fgc *buffer*)
                     :bg (bgc *buffer*)
                     :dirty-p t))

(defmethod get-cell ((buf vbuffer) row col)
  (let ((data (data buf))
        (i0 (1- row))
        (i1 (1- col)))
    (or (aref data i0 i1)
        (setf (aref data i0 i1) (make-instance 'vcell)))))

(defmethod put-cell ((buf vbuffer) row col)
  (let ((cell (get-cell buf row col)))
    (setf (dirty-p cell) nil)))

(defmethod set-cell ((buf vbuffer) row col ch fg bg)
  (let ((cell (get-cell buf row col)))
    (unless (and (eql (ch cell) ch)
                 (eql (fg cell) fg)
                 (eql (bg cell) bg))
      (setf (ch cell) ch
            (fg cell) fg
            (bg cell) bg
            (dirty-p cell) t))))

(defclass vclip ()
  ((r1 :initarg :r1 :accessor r1)
   (c1 :initarg :c1 :accessor c1)
   (r2 :initarg :r2 :accessor r2)
   (c2 :initarg :c2 :accessor c2)
   (fn :initarg :fn :accessor fn))
  (:default-initargs :r1 1
                     :c1 1
                     :r2 24
                     :c2 80
                     :fn (constantly t)))

(defmethod inside-p ((buffer vbuffer) row col)
  (let ((clip (clip buffer)))
    (and (<= (r1 clip) row (r2 clip))
         (<= (c1 clip) col (c2 clip))
         (funcall (fn clip) row col))))

(defmacro with-clipping ((buffer &key r1 c1 r2 c2 fn) &body body)
  (alexandria:with-gensyms (row1 col1 row2 col2 fun)
    `(let ((clip (clip ,buffer)))
       (let (,@(when r1 `((,row1 (r1 clip))))
             ,@(when c1 `((,col1 (c1 clip))))
             ,@(when r2 `((,row2 (r2 clip))))
             ,@(when c2 `((,col2 (c2 clip))))
             ,@(when fn `((,fun  (fn clip)))))
         (unwind-protect
              (progn ,@(when r1 `((setf (r1 clip) (max ,row1 ,r1))))
                     ,@(when c1 `((setf (c1 clip) (max ,col1 ,c1))))
                     ,@(when r2 `((setf (r2 clip) (min ,row2 ,r2))))
                     ,@(when c2 `((setf (c2 clip) (min ,col2 ,c2))))
                     ,@(when fn `((setf (fn clip)
                                        (lambda (row col)
                                          (and (funcall ,fun row col)
                                               (funcall ,fn row col))))))
                     ,@body)
           ,@(when r1 `((setf (r1 clip) ,row1)))
           ,@(when c1 `((setf (c1 clip) ,col1)))
           ,@(when r2 `((setf (r2 clip) ,row2)))
           ,@(when c2 `((setf (c2 clip) ,col2)))
           ,@(when fn `((setf (fn clip) ,fn))))))))


(defvar *buffer*)

(defmacro with-buffer ((object) &body body)
  `(let ((*buffer* ,object))
     ,@body))

(defun clear-rectangle (r1 c1 r2 c2)
  (loop with buf = *buffer*
        with fgc = (fgc buf)
        with bgc = (bgc buf)
        with max-row = (min r2 (rows buf))
        with max-col = (min c2 (cols buf))
        for row from r1 upto max-row
        do (loop for col from c1 upto max-col
                 do (set-cell buf row col #\space fgc bgc))))

(defmacro out ((&key row col fgc bgc) object)
  `(let ((str (princ-to-string ,object)))
     (assert (null (find #\newline str)))
     (let* ((buf *buffer*)
            (row (or ,row (row buf)))
            (col (or ,col (col buf)))
            (fgc (or ,fgc (fgc buf)))
            (bgc (or ,bgc (bgc buf))))
       (loop for col from col
             for ch across str
             when (inside-p buf row col)
               do (set-cell buf row col ch fgc bgc)))))

(defmacro ctl (&rest operations)
  `(progn
     ,@(loop for op in operations
             collect (destructuring-bind (name &rest args) op
                       (ecase name
                         (:clr `(clear-rectangle ,@args))
                         (:fgc `(setf (fgc *buffer*) ,@args))
                         (:bgc `(setf (bgc *buffer*) ,@args))
                         (:cvp `(setf (cvp *buffer*) ,@args))
                         (:ptr `(setf (ptr *buffer*) ,@args))
                         (:row `(setf (row *buffer*) ,@args))
                         (:col `(setf (col *buffer*) ,@args))
                         (:ffb `(flush-buffer *buffer* :force t))
                         (:fls `(flush-buffer *buffer* :force nil)))))))
