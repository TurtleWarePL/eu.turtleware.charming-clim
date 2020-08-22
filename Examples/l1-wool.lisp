(in-package #:eu.turtleware.charming-clim.examples)

;;; Wool is an interactive generative art joke, enjoy! Inspired by a very
;;; entertaining application "Silk" available at http://weavesilk.com/.

(defconstant +white+  #xffffffff)
(defconstant +black+  #x000000ff)
(defconstant +grey0+  #x888888ff)
(defconstant +grey1+  #x444444ff)
(defconstant +grey2+  #xbbbbbbff)
(defconstant +purple+ #xff88ffff)

;;; Symmetry is the fool's aesthetics.
(defun draw-text (string center-row center-col txt)
  (l1:out (:row center-row
           :col (- center-col (truncate (length string) 2))
           :fgc +white+
           :bgc +black+
           :txt txt)
          string)
  (l1:ctl (:fls)))

(defun draw-peep (row col)
  (loop for drow from -3 upto +3
        do (loop for dcol from -3 upto +3
                 do (if (and (<= (+ (abs dcol) (abs drow)) 3)
                             (>= (+ (abs dcol) (abs drow)) 2))
                        (l1:out (:row (+ row drow)
                                 :col (+ col (* 2 dcol))
                                 :fgc +purple+
                                 :bgc +black+)
                                "HX")
                        (l1:out (:row (+ row drow)
                                 :col (+ col (* 2 dcol))
                                 :fgc +purple+
                                 :bgc +black+)
                                "  ")))))

(defun start-wool ()
  (l1:with-console (:ios *terminal-io*)
    (multiple-value-bind (r1 c1 r2 c2)
        (l1:bbox l1:*console*)
      (let ((center-row (truncate (+ r1 r2) 2))
            (center-col (truncate (+ c1 c2) 2)))
        (draw-text "Wool" (- center-row 8) center-col
                   '(:intensity :bold :underline :single))
        (draw-text "Interactive generative ploy"
                   (- center-row 6) center-col '(:intensity :normal
                                                 :underline :none))
        (draw-peep center-row center-col)
        (draw-text "Draw something."
                   (+ center-row 8) center-col '(:italicized t
                                                 :intensity :faint))
        (l1:process-next-event t)))))

(register-example :wool 'start-wool)
