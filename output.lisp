(in-package #:eu.turtleware.charming-clim)

(defgeneric get-cell (buffer row col))
(defgeneric put-cell (buffer row col ch fg bg))
(defgeneric set-cell (buffer row col ch fg bg))
(defgeneric inside-p (buffer row col))
(defgeneric flush-buffer (buffer &key r1 c1 r2 c2 force))

(defgeneric fgc (buffer))
(defgeneric (setf fgc) (fgc buffer)
  (:argument-precedence-order buffer fgc))

(defgeneric bgc (buffer))
(defgeneric (setf bgc) (bgc buffer)
  (:argument-precedence-order buffer bgc))

(defgeneric row (buffer))
(defgeneric (setf row) (row buffer)
  (:argument-precedence-order buffer row))

(defgeneric col (buffer))
(defgeneric (setf col) (col buffer)
  (:argument-precedence-order buffer col))

(defgeneric rows (buffer))
(defgeneric cols (buffer))

(defgeneric inside-p (buffer row col))

(defgeneric invoke-with-clipping (buffer continuation
                                  &rest opts
                                  &key r1 c1 r2 c2 fn))

(defmacro with-clipping ((buffer &rest opts) &body body)
  (let ((fn (gensym)))
    `(flet ((,fn () ,@body))
       (declare (dynamic-extent (function ,fn)))
       (invoke-with-clipping ,buffer (function ,fn) ,@opts))))

(defmacro letf (bindings &body body)
  (loop for (place value) in bindings
        for old-val = (gensym)
        collect `(,old-val ,place)      into saves
        collect `(setf ,place ,value)   into store
        collect `(setf ,place ,old-val) into restore
        finally (return `(let (,@saves)
                           (unwind-protect (progn ,@store ,@body)
                             ,@restore)))))


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
                         (:rnd `(setf (rend *buffer*) ,@args))
                         (:ffb `(flush-buffer *buffer* :force t))
                         (:fls `(flush-buffer *buffer* :force nil)))))))


(defclass clip ()
  ((r1 :initarg :r1 :accessor r1)
   (c1 :initarg :c1 :accessor c1)
   (r2 :initarg :r2 :accessor r2)
   (c2 :initarg :c2 :accessor c2)
   (fn :initarg :fn :accessor fn))
  (:default-initargs :r1 1 :c1 1 :r2 24 :c2 80
                     :fn (constantly t)))

(defclass cell ()
  ((ch :initarg :ch :accessor ch)
   (fg :initarg :fg :accessor fg)
   (bg :initarg :bg :accessor bg)
   (dirty-p :initarg :dirty-p :accessor dirty-p))
  (:default-initargs :ch #\space
                     :fg (fgc *buffer*)
                     :bg (bgc *buffer*)
                     :dirty-p t))

(defclass buffer ()
  ((fgc :initarg :fgc :accessor fgc :documentation "Foregorund color")
   (bgc :initarg :bgc :accessor bgc :documentation "Background color")
   (row :initarg :row :accessor row :documentation "Current row")
   (col :initarg :col :accessor col :documentation "Current col")
   (rend :initarg :rend :accessor rend :documentation "Rendering mode")
   (clip :initarg :clip :accessor clip :documentation "Clipping object")
   (data :initarg :data :accessor data :documentation "Data buffer")
   (rows :initarg :rows :accessor rows :documentation "Buffer number of rows")
   (cols :initarg :cols :accessor cols :documentation "Buffer number of cols"))
  (:default-initargs :fgc #xffa0a0
                     :bgc #x222222
                     :row 1
                     :col 1
                     :rend :buf
                     :clip (make-instance 'clip)))

(defmethod inside-p ((buffer buffer) row col)
  (let* ((clip (clip buffer))
         (row1 (or (r1 clip) )))
    (and (<= (r1 clip) row (r2 clip))
         (<= (c1 clip) col (c2 clip))
         (funcall (fn clip) row col))))

(defmethod invoke-with-clipping ((buffer buffer) cont &key r1 c1 r2 c2 fn)
  (let ((clip (clip buffer)))
    (letf (((r1 clip) (or r1 (r1 clip)))
           ((c1 clip) (or c1 (c1 clip)))
           ((r2 clip) (or r2 (r2 clip)))
           ((c2 clip) (or c2 (c2 clip)))
           ((fn clip) (if (null fn)
                          (fn clip)
                          (let ((old-fn (fn clip)))
                            (lambda (row col)
                              (and (funcall fn row col)
                                   (funcall old-fn row col)))))))
      (funcall cont))))

(defmethod put-cell ((buffer buffer) row col ch fg bg)
  (declare (ignore row col ch fg bg))
  (warn "put-cell: default method does nothing!"))

(defmethod get-cell ((buf buffer) row col)
  (let ((data (data buf))
        (i0 (1- row))
        (i1 (1- col)))
    (if (array-in-bounds-p data i0 i1)
        (or (aref data i0 i1)
            (setf (aref data i0 i1) (make-instance 'cell)))
        (make-instance 'cell))))

(defmethod set-cell ((buf buffer) row col ch fg bg)
  (let* ((cell (get-cell buf row col))
         (clean (and (eql ch (ch cell))
                     (eql fg (fg cell))
                     (eql bg (bg cell)))))
    (flet ((set-cell ()
             (unless clean
               (setf (ch cell) ch
                     (fg cell) fg
                     (bg cell) bg)))
           (put-cell ()
             (put-cell buf row col ch fg bg)))
      (ecase (rend buf)
        (:buf
         (set-cell)
         ;; Unchanged cell retains its dirtiness. Changed cell is
         ;; always dirty.
         (unless clean
           (setf (dirty-p cell) t)))
        (:dir
         (put-cell)
         (setf (dirty-p cell) (not clean)))
        (:bth
         (set-cell)
         (put-cell)
         (setf (dirty-p cell) nil))))))
