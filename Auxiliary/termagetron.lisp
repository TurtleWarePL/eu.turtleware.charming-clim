(defpackage #:eu.turtleware.termagetron
  (:use #:cl #:alexandria)
  (:local-nicknames
   (#:charm #:eu.turtleware.charming-clim)
   (#:l1 #:eu.turtleware.charming-clim/l1)))
(in-package #:eu.turtleware.termagetron)

(defclass termagetron (l1:console)
  ((players
    :accessor players)
   (grid
    :accessor grid)))

(defclass grid (l1:surface)
  ()
  (:default-initargs :r1 1 :c1 1 :r2 20 :c2 40))

(defmethod initialize-instance :after ((object termagetron) &key)
  (l1:with-output (object)
   (setf (grid object) (make-instance 'grid))))

(defun bgc ()
  (random-elt '(#x010122ff #x010200ff #x010400ff
                #x020200ff #x020400ff #x040400ff)))

(defmethod l1:handle-repaint ((client termagetron) region)
  (declare (ignore region))
  (loop for row from 1 upto 20
        do (loop for col from 1 upto 40
                 do (l1:out (:row row :col col :bgc (bgc)) " ")))
  (l1:ctl (:fls)))

(defun run ()
  (l1:with-console (:ios *terminal-io*
                    :console-class 'termagetron)
    (loop (l1:process-available-events)
          (l1:handle-repaint l1:*console* t))))
