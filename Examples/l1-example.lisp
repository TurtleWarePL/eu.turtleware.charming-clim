(in-package #:eu.turtleware.charming-clim.examples)

(defclass frame-manager (l1:console)
  ((frames :initarg :frames :accessor frames :documentation "All frames"))
  (:default-initargs :frames nil))

(defclass frame (l1:surface)
  ())

(defclass noise-demo (frame)
  ())

(defgeneric handle-repaint (object))

(defmethod handle-repaint :around (object)
  (l1:with-buffer (object)
    (call-next-method)
    (l1:ctl (:fls :force t))))

(defmethod handle-repaint ((fm frame-manager))
  (l1:ctl (:clr 1 1
                (eu.turtleware.charming-clim::rows l1:*console*)
                (eu.turtleware.charming-clim::cols l1:*console*))
          (:ink #xbbbbbb00 #x33333300))
  (dolist (frame (frames fm))
    ;; decorations
    (multiple-value-bind (r1 c1 r2 c2) (l1:bbox frame)
      (loop with col = (1+ c2)
            for row from (1+ r1) upto (1- r2)
            do (l1:out (:row row :col col) " ")
            finally (l1:out (:col col :row r1 :fgc #xff224400) "x")
                    (when (or (> (eu.turtleware.charming-clim::rows frame)
                                 (1+ (- r2 r1)))
                              (> (eu.turtleware.charming-clim::cols frame)
                                 (1+ (- c2 c1))))
                      (l1:out (:col col :row (1- r2)) "&"))
                    (l1:out (:col col :row r2) "/")))
    ;; the frame
    (handle-repaint frame))
  (l1:ctl (:ink #xbbbbbb00 #x11111100)))

(defmethod handle-repaint ((frame frame))
  (l1:ctl (:ink #xff0000ff #x8888ffff)
          (:clr 1 1 (l0:rows frame) (l0:cols frame))))

(defmethod handle-repaint ((frame noise-demo))
  (call-next-method)
  (loop for row from 1 upto (eu.turtleware.charming-clim::rows frame)
        do (loop for col from 1 upto (eu.turtleware.charming-clim::cols frame)
                 do (l1:out (:row row
                             :col col
                             ;; this sets everything(!)
                             :bgc (ax:random-elt `(#x00000000 #x08080800))
                             :fgc (ax:random-elt '(#xff444400 #xff664400)))
                            (ax:random-elt '("+" "-"))))))

(defun run-l1 ()
  (let ((lambda-frame
          (make-instance 'noise-demo
                         :rows 12 :cols 40
                         :r1 2 :c1 4 :r2 13 :c2 43))
        (noise-frame
          (make-instance 'frame
                         :rows 12 :cols 40
                         :r1 12 :c1 24 :r2 23 :c2 63)))
    (loop with frames = (list lambda-frame noise-frame)
          with fm = l1:*console*
            initially (setf (frames fm) frames)
          do (l1:process-available-events)
          do (handle-repaint fm)
          do (sleep .1))))

(defun start-l1 ()
  (l1:with-console (:ios *terminal-io*
                    :console-class 'frame-manager)
    (run-l1)))

(register-example :l1-example #'start-l1)
