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

;;; This function draws "drawing cursors", that is the pointer and its
;;; mirrored versions. It is used both to draw cursors and to modify a buffer,
;;; the rendering mode is specified as the third argument.
(defun draw-cursors (x y mode)
  (let* ((buf l1:*buffer*)
         (cur (fm::ptr buf))
         (crow (/ (1+ (fm::rows buf)) 2))
         (ccol (/ (1+ (fm::cols buf)) 2))
         (row (fm::row cur))
         (col (fm::col cur))
         (drow (- crow row))
         (dcol (- ccol col)))
    ;; Center of the terminal is tricky, it may be in the center or between
    ;; two adjacent cells - that applies separately to both rows and
    ;; columns. When we work with small pixels it could rounded, but pixels of
    ;; a character size a difference is easy to spot. -- jd 2020-08-25
    (fm::letf (((fm::mode buf) mode))
      (l1:out (:row (- crow drow) :col (- ccol dcol) :fgc +blue+) "1")
      (l1:out (:row (- crow drow) :col (+ ccol dcol) :fgc +blue+) "2")
      (l1:out (:row (+ crow drow) :col (- ccol dcol) :fgc +blue+) "3")
      (l1:out (:row (+ crow drow) :col (+ ccol dcol) :fgc +blue+) "4")

      ;; (l1:out (:row (+ crow (truncate dcol 2))
      ;;          :col (- ccol (* drow 2)) :fgc +blue+) "8")
      ;; (l1:out (:row (+ crow (truncate dcol 2))
      ;;          :col (+ ccol (* drow 2)) :fgc +blue+) "9")
      ;; (l1:out (:row (- crow (truncate dcol 2))
      ;;          :col (- ccol (* drow 2)) :fgc +blue+) "0")
      ;; (l1:out (:row (- crow (truncate dcol 2))
      ;;          :col (+ ccol (* drow 2)) :fgc +blue+) "a")
      (finish-output fm::*terminal*))))

(defun draw-wool ()
  (multiple-value-bind (r1 c1 r2 c2)
      (l1:bbox l1:*buffer*)
    (l1:ctl (:ink +white+ +black+))
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
      (l1:ctl (:fls))
      (l1:process-next-event t))))

(defclass wool (l1:console)
  ((n-fold :initarg :n-fold :accessor n-fold)
   (mirror :initarg :mirror :accessor mirror)
   (spiral :initarg :spiral :accessor spiral))
  (:default-initargs :n-fold 1 :mirror :y :spiral nil))

(defmethod fm::flush-output :after ((client wool) &rest args)
  (declare (ignore args))
  (draw-cursors 40 80 :dir))

(defmethod l1:handle-event :after ((client wool) (event l0:pointer-event))
  (when (eq :left (l0:btn event))
    (draw-cursors 40 80 :buf)))    

(defun start-wool ()
  (l1:with-console (:ios *terminal-io* :console-class 'wool)
    (l1:ctl (:ink +white+ +black+))
    (loop (draw-wool))))

(register-example :wool 'start-wool)
