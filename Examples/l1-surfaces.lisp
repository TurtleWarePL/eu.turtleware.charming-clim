(in-package #:eu.turtleware.charming-clim.examples)

;;; This example shows how to achieve a stable framerate and manage surfaces.
;;;
;;; Features:
;;;
;;; - a custom implementation of the console
;;; - two moveable and resizable surfaces
;;; - keyboard events are redirected to a focused surface (if any)
;;; - pointer events are redirected to a surface below the cursor

(defclass window (l1:surface)
  ((title
    :initarg :title
    :accessor title)))

(defclass surface-manager (l1:console)
  ((surfaces
    :accessor surfaces)
   (focused
    :accessor focused))
  (:default-initargs
   :fgc +white+
   :bgc +black+))

(defmethod initialize-instance :after ((object surface-manager) &rest args)
  (declare (ignore args))
  (setf (surfaces object)
        (list (make-instance 'window
                             :title "Window 1"
                             :sink object
                             :fgc +purple+
                             :bgc +grey1+
                             :r1 3 :c1 6 :r2 9 :c2 19)
              (make-instance 'window
                             :title "Window 2"
                             :sink object
                             :fgc +black+
                             :bgc +grey2+
                             :r1 13 :c1 10 :r2 19 :c2 23)))
  (setf (focused object) nil))

(defun draw-decorations (surface)
  (multiple-value-bind (r1 c1 r2 c2) (l1:bbox surface)
    (l1:with-buffer ((l1:sink surface))
      (l1:ctl (:ink #x0000ff00 #xffff8800))
      (let* ((length (+ (- c2 c1) 5))
             (hbar (make-string length :initial-element #\space)))
        (l1:out (:row (1- r1) :col (- c1 2)) hbar)
        (draw-text (title surface) (1- r1) (floor (+ c2 c1 1) 2) :align :center)
        (l1:out (:row (1+ r2) :col (- c1 2)) hbar)
        (l1:out (:row (1+ r2) :col (1+ c2)) "30"))
      (loop for r from r1 upto r2
            do (l1:out (:row r :col (- c1 2) :bgc #xffff8800) "  ")
               (l1:out (:row r :col (+ c2 1) :bgc #xffff8800) "  ")))))

(defgeneric handle-repaint (object)
  (:method (object) nil))

(defmethod handle-repaint ((object surface-manager))
  (mapc #'handle-repaint (surfaces object))
  (l1:flush-output object))

(defmethod handle-repaint ((object window))
  (l1:with-buffer (object)
    (draw-peep 1 1 nil)
    ;; We need "force" here to reflush the background onto the sink that
    ;; redraws its own background upon C-r. The more robust solution would be
    ;; a protocol which allows signaling damaged regions to children. That
    ;; will be implemented in the l2 module.
    (l1:flush-output object :force t))
  (draw-decorations object))

(defun start-surface-manager ()
  (l1:with-console (:ios *terminal-io* :console-class 'surface-manager)
    (loop with console = l1:*console*
          with flush-time = (floor internal-time-units-per-second 60)
          with next-paint = (+ (get-internal-real-time) flush-time)
          do (when (>= (get-internal-real-time) next-paint)
               (handle-repaint console)
               (setf next-paint (+ (get-internal-real-time) flush-time)))
             (unless (l1:process-next-event)
               (sleep .001)))))

(register-example :surfaces 'start-surface-manager)
