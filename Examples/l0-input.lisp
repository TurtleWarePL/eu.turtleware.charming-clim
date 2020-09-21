(in-package #:eu.turtleware.charming-clim.examples)

(register-example
 :l0-input
 (lambda ()
   (let* ((l0:*terminal* *terminal-io*)
          (handler (l0:init-terminal)))
     (unwind-protect (let ((last-event nil))
                       (flet ((render-frame ()
                                (typecase last-event
                                  (l0:keyboard-event
                                   (l0:set-cursor-position 1 1))
                                  (l0:pointer-event
                                   (l0:set-cursor-position 2 1))
                                  (l0:oink-terminal-event
                                   (l0:set-cursor-position 3 1))
                                  (l0:unknown-terminal-event
                                   (l0:set-cursor-position 4 1))
                                  (l0:terminal-event
                                   (l0:set-cursor-position 5 1))
                                  (otherwise
                                   (l0:set-cursor-position 6 1)))
                                (l0:put last-event)
                                (l0:clear-line 0)
                                (finish-output l0:*terminal*)))
                         (l0:reset-terminal)
                         (l0:set-mouse-tracking t)
                         (finish-output l0:*terminal*)
                         (loop (setf last-event (l0:read-input t))
                               (render-frame)
                               (when (l0:keyp last-event #\Q :c)
                                 (return)))))
       (l0:set-mouse-tracking nil)
       (finish-output l0:*terminal*)
       (l0:reset-terminal)
       (l0:close-terminal handler)))))
