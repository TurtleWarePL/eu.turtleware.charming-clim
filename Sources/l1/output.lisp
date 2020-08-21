(in-package #:eu.turtleware.charming-clim)

(defgeneric buffer-cursor (buffer))
(defgeneric direct-cursor (buffer))

(defgeneric flush-output (buffer &rest args))
(defgeneric put-cell (buffer str &rest cursor-args))
(defgeneric set-cell (buffer str &rest cursor-args))

(defgeneric mode (buffer))
(defgeneric (setf mode) (mode buffer)
  (:argument-precedence-order buffer mode))

(defgeneric bbox (object))
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

(defmacro out ((&rest cursor-args) object)
  `(set-cell *buffer* (princ-to-string ,object) ,@cursor-args))

(defmacro ctl (&rest operations)
  (let ((operations
          (loop for op in operations
                collect (destructuring-bind (name &rest args) op
                          (ecase name
                            (:txt `(change-cursor-text pen ,@args))
                            (:ink `(change-cursor-inks pen ,@args))
                            (:pos `(change-cursor-position pen ,@args))
                            (:clr `(clear-rectangle ,@args))
                            (:fls `(flush-output buf ,@args)))))))
    `(let ((buf *buffer*))
       (ecase (mode buf)
         ((:buf :wrt)
          (let ((pen (buffer-cursor buf)))
            (declare (ignorable pen))
            ,@operations))
         (:dir
          (let ((pen (direct-cursor buf)))
            (declare (ignorable pen))
            ,@operations))))))

(defun clear-rectangle (r1 c1 r2 c2)
  (loop with str = (make-string (1+ (- c2 c1)) :initial-element #\space)
        for r from r1 upto r2
        do (out (:row r :col c1) str)))


(defclass bbox ()
  ((r1 :initarg :r1 :accessor r1)
   (c1 :initarg :c1 :accessor c1)
   (r2 :initarg :r2 :accessor r2)
   (c2 :initarg :c2 :accessor c2)))

(defmethod bbox ((o bbox))
  (values (r1 o) (c1 o) (r2 o) (c2 o)))

(defclass clip (bbox)
  ((fn :initarg :fn :accessor fn))
  (:default-initargs :r1 1 :c1 1 :r2 24 :c2 80
                     :fn (constantly t)))

(defclass cell (drawing-style-mixin)
  ((chr :initarg :chr :accessor chr)
   (dirty-p :initarg :dirty-p :accessor dirty-p))
  (:default-initargs :chr #\space
                     :fgc (fgc (bcur *buffer*))
                     :bgc (bgc (bcur *buffer*))
                     :dirty-p t))

(defclass output-buffer ()
  ((bcur :initarg :bcur :accessor bcur :documentation "Buffer's cursor"
         :reader buffer-cursor)
   (mode :initarg :mode :accessor mode :documentation "Rendering mode")
   (clip :initarg :clip :accessor clip :documentation "Clipping object")
   (data :initarg :data :accessor data :documentation "Data buffer")
   (rows :initarg :rows :accessor rows :documentation "Buffer number of rows")
   (cols :initarg :cols :accessor cols :documentation "Buffer number of cols"))
  (:default-initargs :fgc #x000000ff
                     :bgc #x88ff88ff
                     :mode :buf
                     :data (make-array (list 0 0) :adjustable t)
                     :clip (make-instance 'clip)))

(defmethod initialize-instance :after ((buf output-buffer)
                                       &key bcur bgc fgc)
  (unless bcur
    (setf (bcur buf) (make-instance 'cursor :fgc fgc :bgc bgc))))

(defmethod bbox ((o output-buffer))
  (values 1 1 (rows o) (cols o)))

(defmacro iterate-cells ((chr crow ccol wrap)
                         (buf row col str)
                         &body body)
  (alexandria:with-gensyms (cols rows)
    `(loop with ,rows = (rows ,buf)
           with ,cols = (cols ,buf)
           with ,crow = ,row
           with ,ccol = ,col
           with ,wrap = nil
           for ,chr across ,str
           do (progn ,@body)
              (setf ,wrap nil)
           if (eql ,chr #\newline)
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

(defun get-cell (buf row col)
  (let ((data (data buf))
        (i0 (1- row))
        (i1 (1- col)))
    (if (array-in-bounds-p data i0 i1)
        (or (aref data i0 i1)
            (setf (aref data i0 i1) (make-instance 'cell)))
        (load-time-value
         (make-instance 'cell :chr #\space :fgc #xffffff00 :bgc #x00000000)))))

;;; All calls to this function iterate over the buffer and update the cell
;;; "dirty" status. Depending on the buffer's mode different things happen:
;;;
;;; - direct :: the cell's dirty status is updated
;;; - buffer :: whole cell is updated
;;; - write-through :: whole cell is updated (and marked as not dirty)
;;;
;;; When the mode is either direct or write-through then the function put-cell
;;; is called with the same buffer and string, cursor-args are merged with the
;;; buffer's cursor and passed along.
;;;
;;; If someone wants to perform direct writes without updating the internal
;;; buffer, then put-cell could be used, however then flush-output may not
;;; recognize dirty regions and the force flag may be necessary.
(defmethod set-cell ((buf output-buffer) str
                     &rest cursor-args)
  (let* ((bcur (bcur buf))
         (mode (mode buf)))
    (apply #'update-pen bcur cursor-args)
    (let ((row (row bcur))
          (col (col bcur))
          (fgc (fgc bcur))
          (bgc (bgc bcur)))
     (iterate-cells (chr crow ccol wrap-p)
         (buf row col (string str))
       (when (inside-p buf crow ccol)
         (let* ((cell (get-cell buf crow ccol))
                (clean (or (eq mode :wrt)
                           (and (not (dirty-p cell))
                                (eql chr (chr cell))
                                (eql fgc (fgc cell))
                                (eql bgc (bgc cell))))))
           (setf (dirty-p cell) (not clean))
           (unless (eq mode :dir)
             (setf (chr cell) chr
                   (fgc cell) fgc
                   (bgc cell) bgc))))))
    (when (member mode '(:dir :wrt))
      (apply #'put-cell buf str (return-pen bcur)))))

(defmethod put-cell ((buffer output-buffer) str &rest cursor-args)
  (warn "put-cell: default method does nothing!"))

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
