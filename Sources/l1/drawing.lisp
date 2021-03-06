(in-package #:eu.turtleware.charming-clim)

;;; For now text-styles are plists and are not extensible. Their API is
;;; defined by two functions: fuze-text-style and make-text-style, where the
;;; former is responsible for creating a complete text style.

(defvar *default-text-style*
  '(:intensity :normal
    :underline :none
    :italicized nil
    :crossout   nil
    :blink      nil
    :inverse    nil
    :invisible  nil))

(defun fuze-text-style (new-text-style &optional (default *default-text-style*))
  "Replaces in TEXT-STYLE-1 components specified with TEXT-STYLE-2."
  (loop with text-style = (copy-list default)
        for (key val) on new-text-style by #'cddr
        do (setf (getf text-style key) val)
        finally (return text-style)))

(defun make-text-style (&rest args
                        &key
                          intensity underline italicized
                          crossout blink inverse invisible
                        &allow-other-keys)
  (declare (ignore intensity underline italicized
                   crossout blink inverse invisible))
  args)

;;; FIXME we assume complete text styles.
(defun text-style-equal (text-style-1 text-style-2)
  (when (eql text-style-1 text-style-2)
    (return-from text-style-equal t))
  (loop for (key1 val1) on text-style-1 by #'cddr
        for (key2 val2) on text-style-2 by #'cddr
        unless (and (eql (getf text-style-2 key1) val1)
                    (eql (getf text-style-1 key2) val2))
          do (return nil)
        finally (return t)))

;;; FIXME we assume complete text styles.
(defun text-style-diff (text-style &optional (default *default-text-style*))
  (when (eql text-style default)
    (return-from text-style-diff nil))
  (loop for (arg val) on text-style by #'cddr
        unless (eql val (getf default arg :undefined))
          collect arg into diff
          and collect val into diff
        finally
           (return diff)))

;;; This is a mixin class for the pen properties. It is mixed to both cursor
;;; and cell so it is possible to easily copy cursor properties to the cell.
(defclass drawing-style-mixin ()
  ((chr :initarg :chr :accessor chr :documentation "Drawing character")
   (fgc :initarg :fgc :accessor fgc :documentation "Foreground color")
   (bgc :initarg :bgc :accessor bgc :documentation "Background color")
   (txt :initarg :txt :accessor txt :documentation "Text style"))
  (:default-initargs :chr #\space :fgc #x222222ff :bgc #xddddddff
                     :txt (copy-list *default-text-style*)))


(defclass cell (drawing-style-mixin)
  ((dirty-p :initarg :dirty-p :accessor dirty-p))
  (:default-initargs :chr #\space
                     :fgc (fgc (bcur *buffer*))
                     :bgc (bgc (bcur *buffer*))
                     :txt (copy-list (txt (bcur *buffer*)))
                     :dirty-p t))

;;; CLIM has a fancy abstraction with uniform compositums, so it is possible
;;; to easily blend whole surfaces when it is possible. We always go on cell
;;; basis to be concise with the abstraction.
(defun write-cell (cell chr fgc bgc txt &key (only-check nil))
  (let ((clean (and (not (dirty-p cell))
                    (eql chr (chr cell))
                    (eql fgc (fgc cell))
                    (eql bgc (bgc cell))
                    (text-style-equal txt (txt cell)))))
    (unless only-check
      (setf (chr cell) chr
            (fgc cell) fgc
            (bgc cell) bgc
            (txt cell) txt))
    (values cell clean)))
