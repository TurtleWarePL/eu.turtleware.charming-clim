(in-package #:eu.turtleware.charming-clim)

(defgeneric flush-output (buffer &rest args))
(defgeneric put-cell (buffer row col str fg bg))
(defgeneric set-cell (buffer row col str fg bg))

(defgeneric rend (buffer))
(defgeneric (setf rend) (mode buffer)
  (:argument-precedence-order buffer mode))

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
  `(let ((*buffer* ,object)) ,@body))

(defmacro out ((&key row col fgc bgc) object)
  `(let ((buf *buffer*)
         (str (princ-to-string ,object)))
     (set-cell buf ,row ,col str ,fgc ,bgc)))

(defmacro ctl (&rest operations)
  `(let* ((buf *buffer*)
          (pen (bcur buf)))
     (declare (ignorable buf pen))
     ,@(loop for op in operations
             collect (destructuring-bind (name &rest args) op
                       (ecase name
                         (:pen `(change-cursor-pen pen ,@args))
                         (:pos `(change-cursor-position pen ,@args))
                         (:rnd `(setf (rend buf) ,@args))
                         (:clr `(clear-rectangle ,@args))
                         (:fls `(flush-output buf ,@args)))))))

(defun clear-rectangle (r1 c1 r2 c2)
  (loop with str = (make-string (1+ (- c2 c1)) :initial-element #\space)
        for r from r1 upto r2
        do (out (:row r :col c1) str)))


(defclass bbox ()
  ((r1 :initarg :r1 :accessor r1)
   (c1 :initarg :c1 :accessor c1)
   (r2 :initarg :r2 :accessor r2)
   (c2 :initarg :c2 :accessor c2)))

(defclass clip (bbox)
  ((fn :initarg :fn :accessor fn))
  (:default-initargs :r1 1 :c1 1 :r2 24 :c2 80
                     :fn (constantly t)))

(defclass cell ()
  ((ch :initarg :ch :accessor ch)
   (fg :initarg :fg :accessor fg)
   (bg :initarg :bg :accessor bg)
   (dirty-p :initarg :dirty-p :accessor dirty-p))
  (:default-initargs :ch #\space
                     :fg (fgc (bcur *buffer*))
                     :bg (bgc (bcur *buffer*))
                     :dirty-p t))

(defclass output-buffer ()
  ((bcur :initarg :bcur :accessor bcur :documentation "Buffer cursor")
   (rend :initarg :rend :accessor rend :documentation "Rendering mode")
   (clip :initarg :clip :accessor clip :documentation "Clipping object")
   (data :initarg :data :accessor data :documentation "Data buffer")
   (rows :initarg :rows :accessor rows :documentation "Buffer number of rows")
   (cols :initarg :cols :accessor cols :documentation "Buffer number of cols"))
  (:default-initargs :bcur (make-instance 'cursor)
                     :rend :buf
                     :data (make-array (list 0 0) :adjustable t)
                     :clip (make-instance 'clip)))

(defmacro iterate-cells ((ch crow ccol wrap)
                         (buf row col str)
                         &body body)
  (alexandria:with-gensyms (cols rows)
    `(loop with ,rows = (rows ,buf)
           with ,cols = (cols ,buf)
           with ,crow = ,row
           with ,ccol = ,col
           with ,wrap = nil
           for ,ch across ,str
           do (progn ,@body)
              (setf ,wrap nil)
           if (eql ,ch #\newline)
             do (setf ,ccol 1
                      ,wrap t)
                (if (= ,crow ,rows)
                    (setf ,crow 1)
                    (incf ,crow 1))
           else
             do (if (= ,ccol ,cols)
                    (setf ,ccol 1
                          ,crow (1+ ,crow)
                          ,wrap t)
                    (incf ,ccol))
           finally (return (values ,crow ,ccol)))))

(defmethod flush-output ((buffer output-buffer) &rest args)
  (declare (ignore buffer args))
  #|whoosh|#)

(defmethod put-cell ((buffer output-buffer) row col str fg bg)
  (warn "put-cell: default method does nothing!"))

(defun get-cell (buf row col)
  (let ((data (data buf))
        (i0 (1- row))
        (i1 (1- col)))
    (if (array-in-bounds-p data i0 i1)
        (or (aref data i0 i1)
            (setf (aref data i0 i1) (make-instance 'cell)))
        (load-time-value
         (make-instance 'cell :ch #\space :fg #xffffff00 :bg #x00000000)))))

(defmethod set-cell ((buf output-buffer) row col str fgc bgc)
  (let ((pen (bcur buf))
        (rendering-mode (rend buf))
        (row (or row (row pen)))
        (col (or col (col pen))))
    (iterate-cells (ch crow ccol wrap-p)
        (buf row col (string str))
      (when (inside-p buf crow ccol)
        (let* ((cell (get-cell buf crow ccol))
               (clean (and (not (dirty-p cell))
                           (eql ch (ch cell))
                           (eql fgc (fg cell))
                           (eql bgc (bg cell)))))
          (unless clean
            (setf (ch cell) ch
                  (fg cell) (or fgc (fgc pen))
                  (bg cell) (or bgc (bgc pen))))
          (setf (dirty-p cell)
                (and (not clean)
                     (not (eq rendering-mode :wrt)))))))
    (when (member rendering-mode '(:dir :wrt))
      (put-cell buf row col str fgc bgc))))

(defmethod inside-p ((buffer output-buffer) row col)
  (let ((clip (clip buffer)))
    (and (<= (r1 clip) row (r2 clip))
         (<= (c1 clip) col (c2 clip))
         (funcall (fn clip) row col))))

(defmethod invoke-with-clipping
    ((buffer output-buffer) cont &key r1 c1 r2 c2 fn)
  (let ((clip (clip buffer)))
    (let ((old-r1 (r1 clip))
          (old-c1 (c1 clip))
          (old-r2 (r2 clip))
          (old-c2 (c2 clip))
          (old-fn (fn clip)))
      (setf (r1 clip) (max (or r1 old-r1) old-r1)
            (c1 clip) (max (or c1 old-c1) old-c1)
            (r2 clip) (min (or r2 old-r2) old-r2)
            (c2 clip) (min (or c2 old-c2) old-c2)
            (fn clip) (if (null fn)
                          old-fn
                          (lambda (row col)
                            (and (funcall fn row col)
                                 (funcall old-fn row col)))))
      (unwind-protect (funcall cont)
        (setf (r1 clip) old-r1
              (c1 clip) old-c1
              (r2 clip) old-r2
              (c2 clip) old-c2
              (fn clip) old-fn)))))
