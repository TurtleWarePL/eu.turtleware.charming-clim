(in-package #:eu.turtleware.charming-clim)

(defclass frame-manager ()
  ((frames :initarg :frames :accessor frames :documentation "All frames.")
   (active :initarg :active :accessor active :documentation "Active frame."))
  (:default-initargs :frames nil :active nil))

(defun handle-event (fm event)
  (flet ((reset ()
           (setf (frames fm) nil
                 (active fm) nil)
           (update-console-dimensions)
           (clear-rectangle 1 1 (rows *console*) (cols *console*))))
    (cond ((keyp event #\Q :c)
           (cl-user::quit))
          ((keyp event #\R :c)
           (reset))
          ((keyp event #\N :c)
           (reset)
           (setf (frames fm)
                 (list (make-noise-frame 4  2 12 20)
                       (make-noise-frame 6 12 14 30)
                       (make-animation-frame 4 36 10 78 20)
                       (make-report-frame 18 10 23 20))))
          ((keyp event #\M :c)
           (reset)
           (setf (frames fm)
                 (list (make-noise-frame 2
                                         1
                                         (rows *console*)
                                         (cols *console*)))))
          ((keyp event #\U :c)
           (alexandria:if-let ((cur (active fm)))
             (let* ((fms (frames fm))
                    (pos (position cur fms))
                    (new (mod (1+ pos) (length fms))))
               (setf (active fm) (elt fms new)))
             (setf (active fm) (first (frames fm)))))
          ((keyp event #\I :c)
           (setf (active fm) nil))
          ((keyp event :f5)
           (ctl (:ffb)))
          ((keyp event :key-up)
           (alexandria:when-let ((frame (active fm)))
             (scroll-buffer frame -1 0)))
          ((keyp event :key-left)
           (alexandria:when-let ((frame (active fm)))
             (scroll-buffer frame 0 -1)))
          ((keyp event :key-down)
           (alexandria:when-let ((frame (active fm)))
             (scroll-buffer frame 1 0)))
          ((keyp event :key-right)
           (alexandria:when-let ((frame (active fm)))
             (scroll-buffer frame 0 1)))
          ((keyp event :page-up)
           (alexandria:when-let ((frame (active fm)))
             (scroll-buffer frame -4 0)))
          ((keyp event :page-down)
           (alexandria:when-let ((frame (active fm)))
             (scroll-buffer frame 4 0))))))

(defun render-window (frame activep)
  (multiple-value-bind (wr1 wc1 wr2 wc2) (fsz frame)
    (ctl (:bgc (if activep #x444488 #x111111))
         (:fgc (if activep #xffffff #xbbbbbb)))
    (clear-rectangle wr1 wc1 wr2 (1+ wc2))
    (let ((col (1+ wc2)))
      (out (:row wr1 :col col :fgc #xff2244) "x")
      (out (:row (+ wr1 1) :col col) "o")
      (out (:row (+ wr1 2) :col col) ">")
      (out (:row (- wr2 0) :col col) "/"))
    (render-frame frame)))

(defun display-screen (fm)
  (let ((active (active fm)))
    (dolist (frame (frames fm))
      (unless (eq frame active)
        (render-window frame nil)))
    (when active
      (render-window active t))))

(defun start-display ()
  (swank:create-server)
  (with-console (:ios *terminal-io*)
    (clear-console)
    (loop with fm = (make-instance 'frame-manager)
          with count = 0
          with fps = :|Good!.00|
          for rows = (rows *console*)
          for cols = (cols *console*)
          for start = (get-internal-real-time)
          for *count* = 0
          do (loop for ch = (read-input)
                   until (null ch)
                   do (handle-event fm ch))
          do (update-console-dimensions)
          do (display-screen fm)
             (ctl (:fgc #x222222)
                  (:bgc #xffffff))
             (let* ((status (format nil "Rows ~3d, Cols ~3d, FPS ~8,2f, chars ~8d"
                                    (1- rows) cols fps count))
                    (len (length status)))
               (ctl (:clr 1 (min (1+ len) cols) 1 cols))
               (out (:col 1 :row 1) status))
             (ctl (:fgc #xffa0a0)
                  (:bgc #x222222))
          do (ctl (:fls))
             (setf count *count*)
             (let* ((stop (get-internal-real-time))
                    (delta (/ (- stop start) internal-time-units-per-second)))
               (if (zerop delta)
                   (setf fps :|Good!.00|)
                   (setf fps (/ 1.0 delta)))))))



(defclass frame (surface)
  ((rfn :initarg :rfn :accessor rfn :documentation "Rendering function."))
  (:default-initargs :vbuf *console*))

(defun fsz (frame)
  (values (r1 frame) (c1 frame) (r2 frame) (c2 frame)))

(defun render-frame (frame)
  (with-buffer (frame)
    (funcall (rfn frame) frame)
    (ctl (:fls))))

(defun make-noise-frame (r1 c1 r2 c2)
  (flet ((make-noise-renderer (color)
           (lambda (frame)
             (loop for row from 1 upto (rows frame)
                   do (loop for col from 1 upto (cols frame)
                            do (out (:row row
                                     :col col
                                     :bgc (alexandria:random-elt
                                           `(#x000000 #x080808))
                                     :fgc color)
                                    (alexandria:random-elt '("+" "-")))))))
         (random-color ()
           (random (1+ #xffffff))))
    (make-instance 'frame
                   :rfn (make-noise-renderer (random-color))
                   :r1 r1 :c1 c1 :r2 r2 :c2 c2
                   :rows (1+ (- r2 r1))
                   :cols (1+ (- c2 c1)))))

(defun make-animation-frame (r1 c1 r2 c2 speed)
  (let* ((last-time (get-internal-real-time))
         (dc 1)
         (cols (1+ (- c2 c1)))
         (rows (1+ (- r2 r1)))
         (current-row (1+ (truncate rows 2)))
         (current-col 2))
    (flet ((draw-square ()
             (ctl (:bgc #x444400)
                  (:fgc #xffbb00)
                  (:clr 1 1 rows cols))
             (let* ((now (get-internal-real-time))
                    (delta (- now last-time))
                    (seconds (/ delta internal-time-units-per-second)))
               (incf current-col (* seconds speed dc))
               (setf last-time now))
             (cond ((>= (+ current-col 2) cols)
                    (setf dc -1))
                   ((<= (- current-col 2) 1)
                    (setf dc +1)))
             (setf current-col
                   (alexandria:clamp current-col 2 (- cols 2)))
             (loop with row = current-row
                   with col = (round current-col)
                   for r from (1- row) upto (1+ row)
                   do (loop for c from (- col 2) upto (+ col 2)
                            do (out (:row r :col c) "#")))))
      (make-instance 'frame
                     :rfn (lambda (frame)
                            (declare (ignore frame))
                            (draw-square))
                     :r1 r1 :c1 c1 :r2 r2 :c2 c2
                     :rows rows
                     :cols cols))))

(defun make-report-frame (r1 c1 r2 c2 &optional (rows 10))
  (flet ((reporter (frame)
           (declare (ignore frame))
           (let ((str "I'd like to report an event here!"))
             (ctl (:bgc #x000000))
             (clear-rectangle 1 1 rows 50)
             (loop for row from 1 upto rows
                   for id from 0
                   for string = (format nil "XXX ~d/~d: ~a" id (1- rows) str)
                   do (out (:row row :col 1 :fgc #xff8888) string)))))
    (make-instance 'frame
                   :rfn #'reporter :vbuf *console*
                   :r1 r1 :c1 c1 :r2 r2 :c2 c2
                   ;; :row0 3
                   ;; :col0 2
                   :rows rows
                   :cols 50)))
