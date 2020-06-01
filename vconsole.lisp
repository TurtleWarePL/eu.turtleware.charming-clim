(in-package #:eu.turtleware.charming-clim)

(defvar *console*)

(defmacro letf (bindings &body body)
  (loop for (place value) in bindings
        for old-val = (gensym)
        collect `(,old-val ,place)      into saves
        collect `(setf ,place ,value)   into store
        collect `(setf ,place ,old-val) into restore
        finally (return `(let (,@saves)
                           (unwind-protect (progn ,@store ,@body)
                             ,@restore)))))

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
     (letf (((row *console*) (or ,row (row *console*)))
            ((col *console*) (or ,col (col *console*)))
            ,@(when fgc `(((fgc *console*) ,fgc)))
            ,@(when bgc `(((bgc *console*) ,bgc))))
       (let ((row (row *console*))
             (col (col *console*)))
         (loop for c from col
               for s across str
               if (inside row c)
                 do (put s)
               else
                 do (cursor-right))))))

(defmacro ctl (&rest operations)
  `(progn
     ,@(loop for op in operations
             collect (destructuring-bind (name &rest args) op
                       (ecase name
                         (:clr `(clear-rectangle ,@args))
                         (:fgc `(setf (fgc *console*) (list ,@args)))
                         (:bgc `(setf (bgc *console*) (list ,@args)))
                         (:cvp `(setf (cvp *console*) ,@args))
                         (:ptr `(setf (ptr *console*) ,@args))
                         (:row `(setf (row *console*) ,@args))
                         (:col `(setf (col *console*) ,@args)))))))

(defun clear-rectangle (r1 c1 r2 c2)
  (with-cursor-position (r1 c1)
    (loop with str = (make-string (1+ (- c2 c1)) :initial-element #\space)
          for r from r1 upto r2
          do (set-cursor-position r c1)
             (put str))))

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
   (rows :accessor rows             :documentation "Terminal number of rows.")
   (cols :accessor cols             :documentation "Terminal number of cols."))
  (:default-initargs :ios (error "I/O stream must be specified.")
                     :fgc '(#xff #xa0 #xa0)
                     :bgc '(#x22 #x22 #x22)
                     :row 1
                     :col 1
                     :cvp nil
                     :ptr t
                     :fps 10))

(defmethod initialize-instance :after
    ((instance vconsole) &key fgc bgc pos cvp ptr)
  (setf (hnd instance) (init-console))
  (apply #'set-foreground-color fgc)
  (apply #'set-background-color bgc)
  (set-cursor-position (car pos) (cdr pos))
  (set-cursor-visibility cvp)
  (set-mouse-tracking ptr)
  (let ((*console* instance))
    (update-console-dimensions)))


(defmacro with-console ((&rest args
                         &key ios fgc bgc cvp fps &allow-other-keys)
                        &body body)
  (declare (ignore fgc bgc cvp fps))
  `(let* ((*console-io* ,ios)
          (*console* (make-instance 'vconsole ,@args)))
     (unwind-protect (progn ,@body)
       (close-console (hnd *console*)))))
