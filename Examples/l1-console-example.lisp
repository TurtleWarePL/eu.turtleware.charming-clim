(in-package #:eu.turtleware.charming-clim.examples)

(defun draw-l1/pointer-widget (row col)
  (let ((pointer (l1:inp :pointer)))
    ))

[] [] []
--------
-- @@ --
@@ -- @@
-- @@ --
[] -- []
[] -- []


(defun draw-l1/console ()
  (let ((last-event  (l1:inp :get/evt))
        (cursor  (l1:inp :cursor))
        (pointer (l1:inp :pointer)))
    (l1:out (:row 1 :col 1) cursor)
    (l1:out (:row 1 :col 1) pointer)))

(defun run-l1/console ()
  (loop do (l1:inp (:prc))
        do (draw-l1/console)
        do (l1:ctl (:fls))))

(defun start-l1/console ()
  (l1:with-console (:ios *terminal-io*)
    (run-l1)))

(register-example :l1-example #'start-l1)
