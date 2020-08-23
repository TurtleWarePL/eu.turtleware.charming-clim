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

(defun draw-text (string row col &key (align :left))
  (flet ((do-it (row col)
           (out (:row row :col col) string)))
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
                                "HX")
                        (l1:out (:row (+ row drow)
                                 :col (+ col (* 2 dcol))
                                 :fgc +purple+
                                 :bgc +black+)
                                "  ")))))

(defun draw-wool ()
  (multiple-value-bind (r1 c1 r2 c2)
      (l1:bbox l1:*console*)
    (let ((crow (truncate (+ r1 r2) 2))
          (ccol (truncate (+ c1 c2) 2)))
      (l1:ctl (:txt '(:intensity :bold :underline :single)))
      (draw-text "Wool" (- crow 8) ccol :align :center)
      (l1:ctl (:txt '(:intensity :normal :underline :none)))
      (draw-text "Interactive generative ploy" (- crow 6) ccol :align :center)
      (l1:ctl (:ink +purple+ +black+))
      (draw-peep crow ccol)
      (l1:ctl (:ink +white+ +black+))
      (l1:ctl (:txt '(:italicized t :intensity :faint)))
      (draw-text "Draw something." (+ crow 8) ccol :align :center)
      (l1:ctl (:txt '(:italicized nil :intensity :normal)))
      (l1:ctl (:fls))
      (l1:process-next-event t))))

(defun start-wool ()
  (l1:with-console (:ios *terminal-io*)
    (l1:ctl (:ink +white+ +black+))
    (loop (draw-wool))))

(register-example :wool 'start-wool)
