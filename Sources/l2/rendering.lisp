(in-package #:eu.turtleware.charming-clim)

;;; TBD alpha blending

;;; Composing cells is a problematic thing to do. First we need to consider
;;; whether the foreground color of the target cell is used, or whether we
;;; blend only onto the background. In other words, over composition is:
;;;
;;; a) cell-1-bgc cell-1-fgc cell-2-bgc cell-2-fgc
;;; b) cell-1-bgc ---------- cell-2-bgc cell-2-fgc
;;;
;;; Another question is whether we should give the space a special
;;; treatment. For instance:
;;;
;;; - when cell-2-chr is a space and cell-2-bgc is not opaque, then use the
;;;   cell-1-chr with appropriate color for the cell
;;;
;;; - when cell-1-chr is a space, then do not blend cell-1-fgc
;;;
;;; That could certainly give us the "wow" appeal when due, however it doesn't
;;; seem super-useful and is quite irregular. Since the alpha composition in
;;; the console is such a wastful and fun hack, we'll go with the "wow"
;;; approach, which could be summarized with the following pseudocode:
#+ (or)
(let ((bgc (compose cell-1-bgc cell-2-bgc))
      (fgc (cond ((not (char= #\space cell-2-chr))
                  (compose cell-1-bgc cell-2-bgc cell-2-fgc))
                 ((not (char= #\space cell-1-chr))
                  (compose cell-1-bgc cell-1-fgc cell-2-bgc))
                 (t
                  #x00000000)))))

;;; RGB components have premultiplied (associated) alpha[1].
;;; [1] https://en.wikipedia.org/wiki/Alpha_compositing
#+ (or)
(defun blend-colors (target source)
  ;; For example: SOURCE /over/ TARGET
  (flet ((color-components (color)
           "Returns R, G, B and Alpha components of COLOR (in 0-255 range)."
           (values (/ (ldb (byte 8 24) color) 255.0)
                   (/ (ldb (byte 8 16) color) 255.0)
                   (/ (ldb (byte 8  8) color) 255.0)
                   (/ (ldb (byte 8  0) color) 255.0))))
    (multiple-value-bind (r1 g1 b1 a1) (color-components source)
      (when (= a1 255)
        (return-from blend-colors source))
      (multiple-value-bind (r2 g2 b2 a2) (color-components target)
        (let ((r3 (+ r1 (* r2 (- 1.0 a1))))
              (g3 (+ g1 (* g2 (- 1.0 a1))))
              (b3 (+ b1 (* b2 (- 1.0 a1))))
              (a3 (+ a1 (* a2 (- 1.0 a1)))))
          (+ (ash (truncate (* 255 r3)) 24)
             (ash (truncate (* 255 g3)) 16)
             (ash (truncate (* 255 b3)) 8)
             (truncate (* 255 a3))))))))

;;; Drawing functions
(defun draw-text (string row col &key (align :left))
  (flet ((do-it (row col)
           (out (:row row :col col) string)))
    (ecase align
      (:left   (do-it row col))
      (:center (do-it row (- col (truncate (length string) 2))))
      (:right  (do-it row (- col (length string)))))))
