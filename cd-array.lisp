(defpackage #:eu.turtleware.hacks.array*
  (:use)
  (:export #:array*
           ;; Constructors
           #:make-array #:adjust-array
           ;; Predicates
           #:arrayp
           #:array-in-bounds-p
           #:adjustable-array-p
           #:array-has-fill-pointer-p
           ;; Accessors
           #:row-major-aref #:aref
           ;; Readers
           #:array-dimensions #:array-dimension #:array-rank
           #:array-element-type #:array-displacement
           #:array-total-size #:array-row-major-index))

(defpackage #:eu.turtleware.hacks.array*.implementation
  (:use #:cl #:eu.turtleware.hacks.array*)
  (:shadowing-import-from #:eu.turtleware.hacks.array*
                          #:array*
                          ;; Constructors
                          #:make-array #:adjust-array
                          ;; Predicates
                          #:arrayp
                          #:array-in-bounds-p
                          #:adjustable-array-p
                          #:array-has-fill-pointer-p
                          ;; Accessors
                          #:row-major-aref #:aref
                          ;; Readers
                          #:array-dimensions #:array-dimension #:array-rank
                          #:array-element-type #:array-displacement
                          #:array-total-size #:array-row-major-index))
(in-package #:eu.turtleware.hacks.array*.implementation)

(defclass array* ()
  ((array :initarg :array :accessor %array)
   (start :initarg :start :accessor %start)
   (fillp :initarg :fillp :accessor %fillp)
   (inner :initarg :inner :accessor %inner)))

(defun check-conformal-args
    (dimensions initial-element initial-contents
     fill-pointer displaced-to displaced-index-offset)
  (cond ((and (not displaced-to) displaced-index-offset)
         (error "Can't specify ~s without ~s."
                :displaced-index-offset :displaced-to))
        ((and displaced-to (or initial-element initial-contents))
         (error "~s and ~s are mutually exclusive with ~s."
                :initial-element :initial-contents :displaced-to))
        ((and (consp fill-pointer)
              (/= (length fill-pointer) (length dimensions)))
         (error "~s must have the same length as DIMENSIONS."
                :fill-pointer))
        ((and (consp displaced-index-offset)
              (/= (length displaced-index-offset) (length dimensions)))
         (error "~s must have the same length as DIMENSIONS."
                :displaced-index-offset))))

(defun fix-displacement (dimensions displaced-index-offset fill-pointer)
  ;; Correct the FILL-POINTER and the DISPLACED-INDEX-OFFSET. Both
  ;; should be expressed in the destination array indexes.
  (cond ((and (atom fill-pointer)
              (atom displaced-index-offset))
         (setf displaced-index-offset
               (make-list (length dimensions) :initial-element 0))
         (setf fill-pointer dimensions))
        ((atom fill-pointer)
         (setf fill-pointer (mapcar #'+ displaced-index-offset dimensions)))
        ((atom displaced-index-offset)
         (setf displaced-index-offset (mapcar #'- fill-pointer dimensions)))
        (t
         (setf fill-pointer (mapcar #'+ displaced-index-offset fill-pointer))))
  (values displaced-index-offset fill-pointer))

(defun check-indexes (dimensions displaced-index-offset fill-pointer)
  (every #'<=
         (make-list (length dimensions) :initial-element 0)
         displaced-index-offset
         fill-pointer
         (mapcar #'+ displaced-index-offset dimensions)))

(defun get-real-subscripts (array &rest subscripts)
  (loop for sub in subscripts
        for off in (%start array)
        for flp in (%fillp array)
        for ind = (+ sub off)
        if (and (>= ind off) (< ind flp))
          collect ind into subs
        else
          do (error "Invalid index.")
        finally
           (return subs)))

(defun row-major-index-to-subscripts (array index)
  (loop with ind = index
        with sub
        for rem on (array-dimensions array)
        do (multiple-value-setq (sub ind)
             (truncate ind (reduce #'* (cdr rem))))
        collect sub))

(defmacro define-wrapper (name (array-var &rest args) &body body)
  (let ((cl-name (find-symbol (symbol-name name) (find-package 'cl))))
    `(defgeneric ,name (,array-var ,@args)
       (:method ((,array-var cl:array) ,@args)
         (,cl-name ,array-var ,@args))
       (:method ((,array-var array*) ,@args)
         ,@body))))


(defgeneric arrayp (array)
  (:method (array) nil)
  (:method ((array cl:array)) t)
  (:method ((array array*)) t))

(define-wrapper adjustable-array-p (array)
  t)

(define-wrapper array-has-fill-pointer-p (array)
  t)

(defgeneric array-in-bounds-p (array &rest subscripts)
  (:method ((array cl:array) &rest subscripts)
    (apply #'cl:array-in-bounds-p array subscripts))
  (:method ((array array*) &rest subscripts)
    (loop for start in (%start array)
          for fillp in (%fillp array)
          for len = (- fillp start)
          for sub in subscripts
          unless (typep sub `(integer 0 ,len))
            do (return-from array-in-bounds-p nil)
          finally (return t))))

(define-wrapper array-element-type (array)
  (array-element-type (%array array)))

(define-wrapper array-rank (array)
  (length (%start array)))

(define-wrapper array-dimensions (array)
  (mapcar #'- (%fillp array) (%start array)))

(define-wrapper array-dimension (array axis-number)
  (- (nth (%fillp array) axis-number)
     (nth (%start array) axis-number)))

(define-wrapper array-total-size (array)
  (reduce #'* (array-dimensions array)))

(define-wrapper array-displacement (array)
  (values (%array array)
          (%start array)
          (%fillp array)))n

(defgeneric aref (array &rest subscripts)
  (:method ((array cl:array) &rest subscripts)
    (apply #'cl:aref array subscripts))
  (:method ((array array*) &rest subscripts)
    (apply #'cl:aref
           (%array array)
           (apply #'get-real-subscripts array subscripts))))

(defgeneric (setf aref) (new-value array &rest subscripts)
  (:argument-precedence-order array new-value)
  (:method (new-value (array cl:array) &rest subscripts)
    (apply #'(setf cl:aref) new-value array subscripts))
  (:method (new-value (array array*) &rest subscripts)
    (apply #'(setf aref)
           new-value
           (%array array)
           (apply #'get-real-subscripts array subscripts))))

(defgeneric array-row-major-index (array &rest subscripts)
  (:method ((array cl:array) &rest subscripts)
    (apply #'cl:array-row-major-index array subscripts))
  (:method ((array array*) &rest subscripts)
    ;; Q: Can we do better?; A: Of course we can!
    ;; Q: Why won't we?;     A: Too much hassle!
    (loop for rem on (array-dimensions array)
          for sub in subscripts
          summing (* sub (reduce #'* (cdr rem))))))

(define-wrapper row-major-aref (array index)
  (apply #'aref array (row-major-index-to-subscripts array index)))

(defgeneric (setf row-major-aref) (new-value array index)
  (:argument-precedence-order array index new-value)
  (:method (new-value (array cl:array) index)
    (setf (cl:row-major-aref array index) new-value))
  (:method (new-value (array array*) index)
    (apply #'(setf aref) new-value array
           (row-major-index-to-subscripts array index))))

;;; Like CL:MAKE-ARRAY except that it allows FILL-POINTER and
;;; DISPLACED-INDEX-OFFSET to be lists. In that case it creates a
;;; wrapper instance which is conformally displaced onto the array.
;;; When the fill pointer is specified, but no DISPLACED-TO, then we
;;; create an array of the requested size. Effective start and fillp
;;; are stored (that is valid subscripts of the destination array).
(defun make-array (dimensions &rest args
                   &key
                     (element-type t)
                     initial-element
                     initial-contents
                     adjustable
                     fill-pointer
                     displaced-to
                     displaced-index-offset)
  (declare (ignore element-type adjustable))
  (when (and (atom displaced-index-offset)
             (atom fill-pointer)
             (not (typep displaced-to 'array)))
    (return-from make-array
      (apply #'cl:make-array dimensions args)))
  (check-conformal-args dimensions initial-element initial-contents
                        fill-pointer displaced-to displaced-index-offset)
  (when (null displaced-to)
    ;; implies that D-I-O is NIL and that F-P is CONS
    (remf args :fill-pointer)
    (return-from make-array
      (make-instance 'array*
                     :array (apply #'cl:make-array dimensions args)
                     :start (make-list (length dimensions) :initial-element 0)
                     :fillp fill-pointer
                     :inner t)))
  (multiple-value-setq (displaced-index-offset fill-pointer)
    (fix-displacement dimensions displaced-index-offset fill-pointer))
  ;; Assert the indice correctness.
  (if (and (check-indexes dimensions displaced-index-offset fill-pointer)
           (every #'<= fill-pointer (array-dimensions displaced-to)))
      (make-instance 'array*
                     :array displaced-to
                     :start displaced-index-offset
                     :fillp fill-pointer
                     :inner nil)
      (error "Invalid FILL-POINTER or DISPLACED-INDEX-OFFSET specification.")))

;;; Works similar to CL:ADJUST-ARRAY.
(defun adjust-array (array dimensions &rest args
                     &key
                       element-type
                       initial-element
                       initial-contents
                       fill-pointer
                       displaced-to
                       displaced-index-offset)
  (declare (ignore element-type))
  (etypecase array
    (cl:array
     (apply #'adjust-array array dimensions args))
    (array*
     (when (or (not (%inner array)) displaced-to)
       (let ((arr (apply #'make-array array dimensions args)))
         (if (typep arr 'array*)
             (setf (%array array) (%array arr)
                   (%start array) (%start arr)
                   (%fillp array) (%fillp arr)
                   (%inner array) nil)
             (setf (%array array) arr
                   (%start array) (make-list (length dimensions) :initial-element 0)
                   (%fillp array) (array-dimensions arr)
                   (%inner array) t))))
     (check-conformal-args dimensions initial-element initial-contents
                           fill-pointer displaced-to displaced-index-offset)
     (setf displaced-to (%inner array))
     (multiple-value-setq (displaced-index-offset fill-pointer)
       (fix-displacement dimensions displaced-index-offset fill-pointer))
     ;; Assert the indice correctness.
     (unless (check-indexes dimensions displaced-index-offset fill-pointer)
       (error "Invalid FILL-POINTER or DISPLACED-INDEX-OFFSET specification."))
     (unless (every #'<= fill-pointer displaced-to)
       (remf args fill-pointer)
       (remf args displaced-index-offset)
       (setf displaced-to (apply #'adjust-array displaced-to fill-pointer args)))
     (setf (%array array) displaced-to
           (%start array) displaced-index-offset
           (%fillp array) fill-pointer)
     array)))
