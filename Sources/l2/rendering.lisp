(in-package #:eu.turtleware.charming-clim)

;;; Drawing functions
(defun draw-text (string row col &key (align :left))
  (flet ((do-it (row col)
           (out (:row row :col col) string)))
    (ecase align
      (:left   (do-it row col))
      (:center (do-it row (- col (truncate (length string) 2))))
      (:right  (do-it row (- col (length string)))))))
