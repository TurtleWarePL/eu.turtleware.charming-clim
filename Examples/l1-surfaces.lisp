(in-package #:eu.turtleware.charming-clim.examples)

;;; This example shows how to achieve a stable framerate and manage surfaces.
;;;
;;; Features:
;;;
;;; - a custom implementation of the console
;;; - multiple moveable and resizable surfaces
;;; - events are redirected to a focused/pressed surface
;;; - multiple pointers may be used simultaneously
;;; - example text buffer cursor implementation

;;; A function of the same name (and similar purpose) will be introduced for
;;; display lists in the l2 module.
(defgeneric handle-repaint (object)
  (:method (object) nil))

;;; This is a helper function which does two things: it reshapes the surface
;;; and resizes the buffer.
(defun resize-surface (buf r1 c1 r2 c2)
  (l1:reshape-surface buf r1 c1 r2 c2)
  (l1:resize-buffer buf (+ (- r2 r1) 1) (+ (- c2 c1) 1)))

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
                             :frame 'peep-frame
                             :fgc +purple+
                             :bgc +grey1+
                             :r1 2 :c1 4 :r2 10 :c2 21)
              (make-instance 'window
                             :title "Window 2"
                             :sink object
                             :frame 'text-frame
                             :fgc +black+
                             :bgc +grey2+
                             :r1 13 :c1 10 :r2 21 :c2 40))))

;;; A helper function.
(defun inside-p (surface row col)
  (multiple-value-bind (r1 c1 r2 c2)
      (l1:bbox surface)
    (cond ((not (and (<= r1 row r2)
                     (<= c1 col c2)))
           nil)
          ((and (= row r2)
                (= col c2))
           :resize)
          ((or (= row r1)
               (= row r2)
               (<= col (+ c1 1))
               (>= col (- c2 1)))
           :move)
          (t
           t))))

(defmethod handle-repaint ((object surface-manager))
  (mapc (lambda (surface)
          (handle-repaint surface))
        (reverse (surfaces object)))
  (l1:flush-output object))

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
                               when (inside-p surface row col)
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

;; resizing window should also resize the frame (to limit the surface)
(defclass window (l1:surface)
  ((title
    :initform (ax:required-argument :title)
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
    :accessor resize-pointers)
   ;; Frame is the actual application.
   (frame
    :initform nil
    :initarg :frame
    :accessor frame)))

;;; This function is responsible for reshaping the window's surface and
;;; adjusting the underlying frame accordingly.
(defun resize-window (win r1 c1 r2 c2)
  (multiple-value-bind (or1 oc1 or2 oc2) (l1:bbox win)
    (l1:with-buffer ((l1:sink win))
      (l1:ctl (:clr or1 oc1 or2 oc2))))
  (resize-surface win r1 c1 r2 c2)
  (ax:when-let ((frame (frame win)))
    (resize-surface frame 2 3 (- (l1:rows win) 1) (- (l1:cols win) 2))))

(defmethod initialize-instance :after ((object window) &rest args
                                       &key frame fgc bgc r1 c1 r2 c2)
  (declare (ignore args))
  ;; The frame mode is direct, because we are not interested in buffering
  ;; output that will be buffered (again) in the window.
  (when frame
    (setf (frame object)
          (let ((rows (+ (- r2 r1) 1))
                (cols (+ (- c2 c1) 1)))
            (make-instance frame :sink object :mode :dir
                                 :fgc fgc :bgc bgc
                                 :r1 2 :c1 3
                                 :r2 (- rows 1) :c2 (- cols 2))))))

(defmethod handle-repaint ((object window))
  (flet ((draw-decorations (surface)
           (let ((rows (l0:rows surface))
                 (cols (l0:cols surface))
                 (border-fgc #x8800ff00)
                 (border-bgc #xffff8800))
             (block nil
               (ax:maphash-values (lambda (s)
                                    (when (eq surface s)
                                      (setf border-fgc #x8800ff00)
                                      (setf border-bgc #xff888800)
                                      (return)))
                                  (pressed l1:*console*)))
             (let ((title (title surface)))
               (l1:ctl (:ink border-fgc border-bgc)
                       (:clr 1 1 rows cols)
                       (:ink #x8800ff00 #x22222200)
                       (:clr 2 3 (- rows 1) (- cols 2))
                       (:ink border-fgc border-bgc))
               (l1:out (:row 1 :col 1) title)
               (l1:out (:row rows :col cols) "/")))))
    (l1:with-buffer (object)
      (draw-decorations object)))
  (handle-repaint (frame object))
  ;; We need "force" here to reflush the background onto the sink that
  ;; redraws its own background upon C-r. The more robust solution would be
  ;; a protocol which allows signaling damaged regions to children. That
  ;; will be implemented in the l2 module.
  (l1:flush-output object :force t))

(defmethod l1:handle-event ((object window) (event l0:keyboard-event))
  (l1:handle-event (frame object) event))

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
       (case (inside-p object row col)
         (:move   (setf (gethash ptr mpt) (cons row col)))
         (:resize (setf (gethash ptr rpt) t))))
      (:release
       (remhash ptr mpt)
       (remhash ptr rpt))
      (:motion
       (multiple-value-bind (r1 c1 r2 c2) (l1:bbox object)
        (if (gethash ptr rpt)
            (resize-window object r1 c1 row col)
            (ax:when-let ((pos (gethash ptr mpt)))
              (if (zerop (hash-table-count rpt))
                  (let ((rdx (- row (car pos)))
                        (cdx (- col (cdr pos))))
                    (resize-window object
                                   (+ r1 rdx)
                                   (+ c1 cdx)
                                   (+ r2 rdx)
                                   (+ c2 cdx)))
                  ;; Don't move the bottom right corner during resize.
                  (resize-window object row col r2 c2))
              (setf (car pos) row
                    (cdr pos) col))))))))


(defclass peep-frame (l1:surface) ())

(defmethod handle-repaint ((object peep-frame))
  (l1:with-buffer (object)
    (draw-peep 1 1 nil)))


(defclass text-frame (l1:surface)
  ((text-buffer
    :initform "Hello world"
    :accessor text-buffer)))

(defmethod handle-repaint ((object text-frame)
                           &aux (text (text-buffer object)))
  (l1:with-buffer (object)
    (l1:ctl (:clr 1 1 (l0:rows object) (l0:cols object)))
    (l1:out (:row 1) text)
    ;; Our "surface manager" is only capable of "focusing" top-level surfaces,
    ;; so we check whether the sink (we assume that it is a top-level window)
    ;; is focused and only then the prompt is printed.
    (when (eq (l1:sink object) (focused l1:*console*))
      ;; out should accept now row/col, in that case buffer-cursor position
      ;; should be left "at the end" of the last output.
      (multiple-value-bind (mod rem)
          (truncate (length text) (l0:cols object))
        (l1:out (:row (+ mod 1) :col (+ rem 1) :txt '(:blink t)) "_")))))

(defmethod l1:handle-event ((object text-frame) (event l0:keyboard-event))
  (setf (text-buffer object) (format nil "~a" event)))

;; (defclass text-cursor (fm::cursor) ())

;; (defmethod l1:handle-event ((client text-cursor) (event l0:keyboard-event))
;;   (fm::key-case event
;;     (:backsp (decf ))))


(defun start-surface-manager ()
  (l1:with-console (:ios *terminal-io* :console-class 'surface-manager)
    (loop with console = l1:*console*
          with flush-time = (floor internal-time-units-per-second 60)
          with next-paint = (+ (get-internal-real-time) flush-time)
            initially
               (l1:ctl (:ink #xffffff00 #x00000000)
                       (:clr 1 1 (l0:rows console) (l0:cols console)))
          do (when (>= (get-internal-real-time) next-paint)
               (handle-repaint console)
               (setf next-paint (+ (get-internal-real-time) flush-time)))
             (unless (l1:process-next-event)
               (sleep .001)))))

(register-example :surfaces 'start-surface-manager)
