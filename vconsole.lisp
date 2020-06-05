(in-package #:eu.turtleware.charming-clim)

(defvar *console*)
(defgeneric flush-buffer (buffer r1 c1 r2 c2))

(defvar *row1* '(1))
(defvar *col1* '(1))
(defvar *row2* '(24))
(defvar *col2* '(80))
(defvar *fun* (list (constantly t)))

(defmacro with-clipping ((&key fun row1 col1 row2 col2) &body body)
  `(let (,@(when row1 `((*row1* (cons (max (car *row1*) ,row1) *row1*))))
         ,@(when col1 `((*col1* (cons (max (car *col1*) ,col1) *col1*))))
         ,@(when row2 `((*row2* (cons (min (car *row2*) ,row2) *row1*))))
         ,@(when col2 `((*col2* (cons (min (car *col2*) ,col2) *col2*))))
         ,@(when fun  `((*fun*  (cons (let ((old (car *fun*)))
                                        (lambda (row col)
                                          (and (funcall ,fun row col)
                                               (funcall old row col))))
                                      *fun*)))))
     ,@body))

(defun inside (row col)
  (and (<= (car *row1*) row (car *row2*))
       (<= (car *col1*) col (car *col2*))
       (funcall (car *fun*) row col)))

(defmacro out ((&key row col fgc bgc) object)
  "Put an object on a console"
  `(let ((str (princ-to-string ,object)))
     (assert (null (find #\newline str)))
     (let ((row (or ,row (row *console*)))
           (col (or ,col (col *console*)))
           (fgc (or ,fgc (fgc *console*)))
           (bgc (or ,bgc (bgc *console*)))
           (data (data *console*)))
       (loop for col from col
             for ch across str
             when (inside row col)
               do (set-cell (ref data row col) ch fgc bgc)))))

(defmacro ctl (&rest operations)
  `(progn
     ,@(loop for op in operations
             collect (destructuring-bind (name &rest args) op
                       (ecase name
                         (:clr `(clear-rectangle ,@args))
                         (:fgc `(setf (fgc *console*) ,@args))
                         (:bgc `(setf (bgc *console*) ,@args))
                         (:cvp `(setf (cvp *console*) ,@args))
                         (:ptr `(setf (ptr *console*) ,@args))
                         (:row `(setf (row *console*) ,@args))
                         (:col `(setf (col *console*) ,@args))
                         (:fls `(progn
                                  (update-console-dimensions)
                                  (flush-buffer *console*
                                                1
                                                1
                                                (rows *console*)
                                                (cols *console*)))))))))

(defun clear-rectangle (r1 c1 r2 c2)
  (loop with buf = (data *console*)
        with fgc = (fgc *console*)
        with bgc = (bgc *console*)
        with max-row = (min r2 (array-dimension buf 0))
        with max-col = (min c2 (array-dimension buf 1))
        for row from r1 upto max-row
        do (loop for col from c1 upto max-col
                 do (set-cell (ref buf row col) #\space fgc bgc))))

(defun get-cursor-position ()
  (request-cursor-position)
  (finish-output *console-io*)
  (handler-case (loop (read-input))
    (cursor-position-report (c)
      (values (row c) (col c)))))

(defun update-console-dimensions ()
  (with-cursor-position ((expt 2 16) (expt 2 16))
    (multiple-value-bind (rows cols)
        (get-cursor-position)
      (setf (rows *console*) rows)
      (setf (cols *console*) cols)
      (destructuring-bind (ar ac) (array-dimensions (data *console*))
        (when (or (> rows ar) (> cols ac))
          (adjust-array (data *console*)
                        (list rows cols)
                        :initial-element nil)))
      (setf *row2* (list rows))
      (setf *col2* (list cols)))))

(defclass vconsole ()
  ((ios :initarg :ios :accessor ios :documentation "Console I/O stream.")
   (fgc :initarg :fgc :accessor fgc :documentation "Foregorund color.")
   (bgc :initarg :bgc :accessor bgc :documentation "Background color.")
   (cvp :initarg :cvp :accessor cvp :documentation "Cursor visibility.")
   (ptr :initarg :ptr :accessor ptr :documentation "Pointer tracking.")
   (fps :initarg :fps :accessor fps :documentation "Desired framerate.")
   (hnd               :accessor hnd :documentation "Terminal handler.")
   (row :initarg :row :accessor row :documentation "Cursor row.")
   (col :initarg :col :accessor col :documentation "Cursor col.")
   (data :accessor data             :documentation "Screen data buffer.")
   (rows :accessor rows             :documentation "Terminal number of rows.")
   (cols :accessor cols             :documentation "Terminal number of cols."))
  (:default-initargs :ios (error "I/O stream must be specified.")
                     :fgc #xffa0a0
                     :bgc #x222222
                     :row 1
                     :col 1
                     :cvp nil
                     :ptr t
                     :fps 10))

(defmethod initialize-instance :after
    ((instance vconsole) &key fgc bgc pos cvp ptr)
  (setf (hnd instance) (init-console))
  (set-foreground-color fgc)
  (set-background-color bgc)
  (set-cursor-position (car pos) (cdr pos))
  (set-cursor-visibility cvp)
  (set-mouse-tracking ptr)
  (setf (data instance) (make-array (list 0 0) :adjustable t))
  (let ((*console* instance))
    (update-console-dimensions)))

(defclass vcell ()
  ((ch :initarg :ch :accessor ch)
   (fg :initarg :fg :accessor fg)
   (bg :initarg :bg :accessor bg))
  (:default-initargs :ch #\space
                     :fg (fgc *console*)
                     :bg (bgc *console*)))

(defun ref (data row col
            &aux (i0 (1- row)) (i1 (1- col)))
  (or (aref data i0 i1)
      (setf (aref data i0 i1) (make-instance 'vcell))))

(defun set-cell (cell ch fg bg)
  (setf (ch cell) ch
        (fg cell) fg
        (bg cell) bg))

(defmethod flush-buffer ((buffer vconsole) r1 c1 r2 c2)
  (set-cursor-position r1 c1)
  (loop with data = (data *console*)
        with max-row = (min r2 (rows buffer))
        with max-col = (min c2 (cols buffer))
        for row from r1 upto max-row
        do (loop with last-fg = nil
                 with last-bg = nil
                 for col from c1 upto max-col
                 for cell = (ref data row col)
                 do (let ((ch (ch cell))
                          (fg (fg cell))
                          (bg (bg cell)))
                      (unless (eql last-fg fg)
                        (set-foreground-color fg)
                        (setf last-fg fg))
                      (unless (eql last-bg bg)
                        (set-background-color bg)
                        (setf last-bg bg))
                      (put ch)))
        finally (finish-output *console-io*)))

(defmacro with-console ((&rest args
                         &key ios fgc bgc cvp fps &allow-other-keys)
                        &body body)
  (declare (ignore fgc bgc cvp fps))
  `(let* ((*console-io* ,ios)
          (*console* (make-instance 'vconsole ,@args)))
     (unwind-protect (progn ,@body)
       (close-console (hnd *console*)))))
