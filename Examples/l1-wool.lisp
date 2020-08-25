(in-package #:eu.turtleware.charming-clim.examples)

;;; Wool is an interactive generative art joke, enjoy! Inspired by a very
;;; entertaining application "Silk" available at http://weavesilk.com/.

(defconstant +white+  #xffffffff)
(defconstant +black+  #x000000ff)
(defconstant +grey0+  #x888888ff)
(defconstant +grey1+  #x444444ff)
(defconstant +grey2+  #xbbbbbbff)
(defconstant +purple+ #xff88ffff)
(defconstant +blue+   #x8888ffff)

;;; Symmetry is the fool's aesthetics.

(defun draw-text (string row col &key (align :left))
  (flet ((do-it (row col)
           (l1:out (:row row :col col) string)))
    (ecase align
      (:left   (do-it row col))
      (:center (do-it row (- col (truncate (length string) 2))))
      (:right  (do-it row (- col (length string)))))))

(defun draw-peep (row col)
  (loop for drow from -3 upto +3
        do (loop for dcol from -3 upto +3
                 do (if (and (<= (+ (abs dcol) (abs drow)) 3)
                             (>= (+ (abs dcol) (abs drow)) 2))
                        (l1:out (:row (+ row drow)
                                 :col (+ col (* 2 dcol))
                                 :fgc +purple+
                                 :bgc +black+)
                                (ax:random-elt '("HX" "30" "42")))
                        (l1:out (:row (+ row drow)
                                 :col (+ col (* 2 dcol))
                                 :fgc +purple+
                                 :bgc +black+)
                                "  ")))))

(defun draw-wool ()
  (multiple-value-bind (r1 c1 r2 c2)
      (l1:bbox l1:*console*)
    (l1:ctl (:ink +white+ +black+)
            (:clr r1 c1 r2 c2))
    (let ((crow (truncate (+ r1 r2) 2))
          (ccol (truncate (+ c1 c2) 2)))
      (l1:ctl (:txt '(:intensity :bold :underline :single :blink nil)))
      (draw-text "Wool" (- crow 8) ccol :align :center)
      (l1:ctl (:txt '(:intensity :normal :underline :none)))
      (draw-text "Interactive generative ploy" (- crow 6) ccol :align :center)
      (l1:ctl (:ink +purple+ +black+))
      (draw-peep crow ccol)
      (l1:ctl (:ink +white+ +black+))
      (l1:ctl (:txt '(:italicized t :intensity :faint)))
      (draw-text "Draw something." (+ crow 8) ccol :align :center)
      (l1:ctl (:txt '(:italicized nil :intensity :normal)))
      (let* ((cur (eu.turtleware.charming-clim::cur l1:*console*))
             (row (eu.turtleware.charming-clim::row cur))
             (col (eu.turtleware.charming-clim::col cur))
             (drow (abs (- crow row)))
             (dcol (abs (- ccol col))))
        (l1:out (:row (+ crow drow) :col (- ccol dcol) :fgc +blue+) "1")
        (l1:out (:row (+ crow drow) :col (+ ccol dcol) :fgc +blue+) "2")
        (l1:out (:row (- crow drow) :col (- ccol dcol) :fgc +blue+) "3")
        (l1:out (:row (- crow drow) :col (+ ccol dcol) :fgc +blue+) "4"))
      (l1:ctl (:fls :force t))
      (l1:process-next-event t))))

(defun start-wool ()
  (l1:with-console (:ios *terminal-io*)
    (l1:ctl (:ink +white+ +black+))
    (loop (draw-wool))))

(register-example :wool 'start-wool)
