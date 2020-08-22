(in-package #:eu.turtleware.charming-clim.examples)

(defun get-terminal-size (&aux (l0:*request-terminal-size* t))
  (loop initially (l0:with-cursor-position (32000 32000)
                    (l0:request-cursor-position))
                  (finish-output l0:*terminal*)
        for event = (l0:read-input t)
        when (typep event 'l0:terminal-resize-event)
          do (return-from get-terminal-size
               (values (l0:rows event) (l0:cols event)))))

(defun run-l0 ()
  (multiple-value-bind (rows cols) (get-terminal-size)
    (let* ((row-center (truncate rows 2))
           (slen (length "Hello world!"))
           (col1 (truncate (- cols slen) 2))
           (col2 (+ col1 slen -1))
           (armed nil)
           (hover nil))
      (flet ((render-frame ()
               (l0:reset-terminal)
               (l0:put "Click on \"Hello World!\" to exit (or press q).")
               (l0:set-mouse-tracking t)
               (l0:set-foreground-color #xff44ffff)
               (l0:set-text-style (list :intensity (if armed :bold :normal)
                                        :underline (if hover :single :none)
                                        :italicized t))
               (l0:with-cursor-position (row-center col1)
                 (l0:put "Hello world!"))
               (finish-output l0:*terminal*))
             (exit-frame ()
               (l0:set-mouse-tracking nil)
               (finish-output l0:*terminal*)
               (return-from run-l0))
             (hoverp (event)
               (and (= (l0:row event) row-center)
                    (<= col1 (l0:col event) col2))))
        (loop for event = (progn (render-frame)
                                 (l0:read-input t))
              do (typecase event
                   ((or character l0:keyboard-event)
                    (when (l0:keyp event #\q)
                      (exit-frame)))
                   (l0:pointer-press-event
                    (setf armed (hoverp event)))
                   (l0:pointer-release-event
                    (when (and armed (hoverp event))
                      (exit-frame))
                    (setf armed nil))
                   (l0:pointer-motion-event
                    (setf hover (hoverp event))))))))
  (finish-output l0:*terminal*))

(defun start-l0 ()
  (let* ((l0:*terminal* *terminal-io*)
         (handler (l0:init-terminal)))
    (unwind-protect (run-l0)
      (l0:close-terminal handler))))

(register-example :l0-example #'start-l0)
