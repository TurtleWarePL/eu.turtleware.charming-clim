(in-package #:eu.turtleware.charming-clim.examples)

;;; This example shows how to achieve a stable framerate and manage surfaces.
;;;
;;; Features:
;;;
;;; - a custom implementation of the console
;;; - multiple moveable and resizable surfaces
;;; - events are redirected to a focused/pressed surface
;;; - multiple pointers may be used simultaneously

(defclass window (l1:surface)
  ((title
    :initarg :title
    :accessor title)
   ;; The hash table keys are pointers which are currently moving the window
   ;; and its values are conses which indicate the pointer "last" position. We
   ;; need the last coordinate to compute the motion difference.
   (moving-pointers
    :initform (make-hash-table :test #'eq)
    :accessor moving-pointers)
   ;; The hash table keys are pointers which currently resize the window. Its
   ;; values are always T.
   (resize-pointers
    :initform (make-hash-table :test #'eq)
    :accessor resize-pointers)))

(defclass surface-manager (l1:console)
  ((surfaces
    :accessor surfaces)
   ;; When a surface is "focused" all keyboard events are delivered to it.
   (focused
    :initform nil
    :accessor focused)
   ;; When a surface is "pressed" all pointer events are delivered to it.
   ;; Multiple surfaces may be pressed by different pointers. Pointer is a key
   ;; and the surface is a value.
   (pressed
    :initform (make-hash-table)
    :accessor pressed))
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
                             :r1 13 :c1 10 :r2 19 :c2 23))))

(defun draw-decorations (surface)
  (multiple-value-bind (r1 c1 r2 c2) (l1:bbox surface)
    (l1:with-buffer ((l1:sink surface))
      (block nil
        (ax:maphash-values (lambda (s)
                             (when (eq surface s)
                               (l1:ctl (:ink #x8800ff00 #xff888800))
                               (return)))
                           (pressed l1:*console*))
        (l1:ctl (:ink #x8800ff00 #xffff8800)))
      (let* ((length (+ (- c2 c1) 5))
             (hbar (make-string length :initial-element #\space))
             (title (title surface)))
        (l1:out (:row (1- r1) :col (- c1 2)) hbar)
        (l1:out (:row (1- r1) :col (- c1 2)) title)
        (when (eq surface (focused l1:*console*))
          (l1:out (:row (1- r1)
                   :col (+ (- c1 2) (length title))
                   :txt '(:blink t)) "█"))
        (l1:out (:row (1+ r2) :col (- c1 2)) hbar)
        (l1:out (:row (1+ r2) :col (+ c2 2)) "/"))
      (loop for r from r1 upto r2
            do (l1:out (:row r :col (- c1 2)) "  ")
               (l1:out (:row r :col (+ c2 1)) "  ")))))

;;; A function of the same name (and similar purpose) will be introduced for
;;; display lists in the l2 module.
(defgeneric handle-repaint (object)
  (:method (object) nil))

(defmethod handle-repaint ((object surface-manager))
  (l1:ctl (:ink #x00000000 #x00000000)
          (:clr 1 1 (l0:rows object) (l0:cols object)))
  (mapc #'handle-repaint (reverse (surfaces object)))
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

(defun inside-p (surface row col &optional decorations)
  (multiple-value-bind (r1 c1 r2 c2)
      (l1:bbox surface)
    ;; +/- 1/2 is for decorations.
    (cond ((and (<= r1 row r2)
                (<= c1 col c2))
           :surface)
          ((null decorations)
           nil)
          ((and (= row (+ r2 1))
                (= col (+ c2 2)))
           :resize)
          ((and (<= (- r1 1) row (+ r2 1))
                (<= (- c1 2) col (+ c2 2)))
           :move)
          (t
           nil))))

;;; Our event handling is quite simplistic. The pointer events left button
;;; press and left button release are used to set the pressed window, and the
;;; pointer event left button press is used to set the focused window. When
;;; they are made above an empty area, then nil is assigned to the appropriate
;;; field. When a window is pressed, then all pointer events are redirected to
;;; that window. When a window is focused, then all keyboard events are
;;; redirected to that window.
(defmethod l1:handle-event ((object surface-manager) (event l0:pointer-event)
                            &aux
                              (ptr (fm::pointer event))
                              (state (l0:state event))
                              (button (l0:btn event)))
  (when (and (eq button :left)
             (eq state :release))
    (ax:when-let ((pressed (gethash ptr (pressed object))))
      (l1:handle-event pressed event)
      (remhash ptr (pressed object)))
    (return-from l1:handle-event))
  (ax:if-let ((surface (gethash ptr (pressed object))))
    ;; Event coordinates are specified in the console coordinate system.
    (l1:handle-event surface event)
    ;; If we end up in the "else" branch, then no surface is pressed. Our
    ;; handling is quite simplistic for that it only distributes events and
    ;; manages focus.
    (when (and (eq button :left)
               (eq state :press))
      (let ((row (l0:row event))
            (col (l0:col event)))
        (ax:if-let ((sur (loop for surface in (surfaces object)
                               when (inside-p surface row col t)
                                 do (return surface))))
          (progn
            (setf (gethash ptr (pressed object)) sur)
            (setf (focused object) sur)
            ;; cycle to the top
            (let ((surfaces (surfaces object)))
              (unless (eq sur (first surfaces))
                (setf (surfaces object)
                      (list* sur (remove sur surfaces)))))
            (l1:handle-event sur event))
          ;; The button was pressed over an empty area.
          (remhash ptr (pressed object)))))))

(defmethod l1:handle-event ((object surface-manager) (event l0:keyboard-event))
  ;; We want to retain global console gestures for keyboard (C-r etc).
  (call-next-method)
  (ax:when-let ((surface (focused object)))
    (l1:handle-event surface event)))

(defmethod l1:handle-event ((object window) (event l0:keyboard-event)))

(defmethod l1:handle-event ((object window) (event l0:pointer-event))
  (unless (eq (l0:btn event) :left)
    (return-from l1:handle-event))
  (let ((ptr (fm::pointer event))
        (row (l0:row event))
        (col (l0:col event))
        (mpt (moving-pointers object))
        (rpt (resize-pointers object)))
    (ecase (l0:state event)
      (:press
       (case (inside-p object row col t)
         (:move   (setf (gethash ptr mpt) (cons row col)))
         (:resize (setf (gethash ptr rpt) t))))
      (:release
       (remhash ptr mpt)
       (remhash ptr rpt))
      (:motion
       (if (gethash ptr rpt)
           (fm::reshape-buffer object
                               (fm::r1 object)
                               (fm::c1 object)
                               (- row 1)
                               (- col 2))
           (ax:when-let ((pos (gethash ptr mpt)))
             (if (zerop (hash-table-count rpt))
                 (fm::move-buffer object
                                  (- row (car pos))
                                  (- col (cdr pos)))
                 ;; Don't move the bottom right corner during resize.
                 (fm::reshape-buffer object
                                     row
                                     col
                                     (fm::r2 object)
                                     (fm::c2 object)))
             (setf (car pos) row
                   (cdr pos) col)))))))

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
