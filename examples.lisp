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
           (if (active fm)
               (setf (active fm) nil)
               (alexandria:when-let ((frs (frames fm)))
                 (setf (active fm) (alexandria:random-elt frs))))))))

(defun render-window (frame)
  (destructuring-bind (wr1 wc1 wr2 wc2) (fsz frame)
    (declare (ignore wc1))
    (when (= wr2 (first *row2*))
      (return-from render-window
        (render-frame frame)))
    (ctl (:bgc #x11 #x11 #x11)
         (:fgc #xbb #xbb #xbb))
    (let ((col (1+ wc2)))
      (out (:row wr1 :col col :fgc '(#xff #x22 #x44)) "x")
      (out (:row (+ wr1 1) :col col) "o")
      (out (:row (+ wr1 2) :col col) ">")
      (loop for row from (+ wr1 3) upto wr2
            do (out (:row row :col col) " "))
      (out (:row (- wr2 0) :col col) "/"))
    (ctl (:bgc #x22 #x22 #x22)
         (:fgc #xbb #xbb #xbb))
    (render-frame frame)))

(defun display-screen (fm)
  (alexandria:if-let ((frame (active fm)))
    (render-window frame)
    (dolist (frame (frames fm))
      (render-window frame))))

(defun start-display ()
  (swank:create-server)
  (with-console (:ios *terminal-io*)
    (clear-console)
    (loop with fm = (make-instance 'frame-manager)
          with count = 0
          with fps = :|We are that good!|
          for rows = (rows *console*)
          for cols = (cols *console*)
          for start = (get-internal-real-time)
          for *count* = 0
          do (loop for ch = (read-input)
                   until (null ch)
                   do (handle-event fm ch))
          do (display-screen fm)
             (ctl (:fgc #x22 #x22 #x22)
                  (:bgc #xbb #xbb #xbb))
             (let* ((status (format nil "Rows ~3d, Cols ~3d, FPS ~8,2f, chars ~8d"
                                    (1- rows) cols fps count))
                    (len (length status)))
               (ctl (:clr 1 (min (1+ len) cols) 1 cols))
               (out (:col 1 :row 1) status))
             (ctl (:fgc #xff #xa0 #xa0)
                  (:bgc #x22 #x22 #x22))
          do (ctl (:fls))
             (setf count *count*)
             (let* ((stop (get-internal-real-time))
                    (delta (/ (- stop start) internal-time-units-per-second)))
               (if (zerop delta)
                   (setf fps :|We are that good!|)
                   (setf fps (/ 1.0 delta)))))))



(defclass frame ()
  ((rfn :initarg :rfn :accessor rfn :documentation "Rendering function.")
   (fsz :initarg :fsz :accessor fsz :documentation "Frame dimensions.")))

(defun render-frame (frame)
  (destructuring-bind (r1 c1 r2 c2) (fsz frame)
    (with-clipping (:row1 r1 :col1 c1 :row2 r2 :col2 c2)
      (funcall (rfn frame) frame))))

(defun make-noise-frame (r1 c1 r2 c2)
  (flet ((make-noise-renderer (color)
           (lambda (frame)
             (destructuring-bind (r1 c1 r2 c2) (fsz frame)
               (loop for row from r1 upto r2
                     do (loop for col from c1 upto c2
                              do (out (:row row
                                       :col col
                                       :bgc (alexandria:random-elt
                                             `((0 0 0) (8 8 8)))
                                       :fgc color)
                                      (alexandria:random-elt '("+" "-")))))))))
    (make-instance 'frame
                   :rfn (make-noise-renderer
                         (list (alexandria:random-elt '(#x22 #x88 #xff))
                               (alexandria:random-elt '(#x22 #x88 #xff))
                               (alexandria:random-elt '(#x22 #x88 #xff))))
                   :fsz (list r1 c1 r2 c2))))

(defun make-animation-frame (r1 c1 r2 c2 speed)
  (let ((last-time (get-internal-real-time))
        (dc 1)
        (current-row (truncate (+ r1 r2) 2))
        (current-col (+ c1 2)))
    (flet ((draw-square ()
             (ctl (:bgc #x44 #x44 #x00)
                  (:fgc #xff #xbb #x00)
                  (:clr r1 c1 r2 c2))
             (let* ((now (get-internal-real-time))
                    (delta (- now last-time))
                    (seconds (/ delta internal-time-units-per-second)))
               (incf current-col (* seconds speed dc))
               (setf last-time now))
             (cond ((>= (+ current-col 2) c2)
                    (setf dc -1))
                   ((<= (- current-col 2) c1)
                    (setf dc +1)))
             (setf current-col
                   (alexandria:clamp current-col (+ c1 2) (- c2 2)))
             (loop with row = current-row
                   with col = (round current-col)
                   for r from (1- row) upto (1+ row)
                   do (loop for c from (- col 2) upto (+ col 2)
                            do (out (:row r :col c) "#")))))
      (make-instance 'frame
                     :rfn (lambda (frame)
                            (declare (ignore frame))
                            (draw-square))
                     :fsz (list r1 c1 r2 c2)))))

(defun make-report-frame (r1 c1 r2 c2)
  (flet ((reporter (frame)
           (declare (ignore frame))
           (let ((str "I would like to report an event here!"))
             (loop with rows = (+ (- r2 r1) 3)
                   with col = (- c1 2)
                   for row from (1- r1) upto (1+ r2)
                   for id = (- row r1 -2)
                   for string = (format nil "XXX ~d/~d: ~a" id rows str)
                   do (out (:row row :col col :fgc '(#xff #x88 #x88)) string)))))
    (make-instance 'frame
                   :rfn #'reporter
                   :fsz (list r1 c1 r2 c2))))
