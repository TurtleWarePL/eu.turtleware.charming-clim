(in-package #:eu.turtleware.charming-clim)

;;; First we process available events and only after that (when requested)
;;; probe for the terminal size. This is made that way to avoid ambiguity.
(defun process-available-events (&optional update-console-dimensions)
  (finish-output *terminal*)
  (loop while (process-next-event nil))
  (when update-console-dimensions
    (with-cursor-position ((expt 2 16) (expt 2 16))
      (request-cursor-position)
      (finish-output *terminal*))
    ;; Defensive programming: define a deadline for defunct terminals.
    (loop with deadline = (+ (get-universal-time) 2)
          with *request-terminal-size* = t
          for event = (process-next-event nil)
          do (unless event
               (sleep .1))
          until (or (typep event 'terminal-resize-event)
                    (> (get-universal-time) deadline)))))

(defun process-next-event (&optional waitp)
  (finish-output *terminal*)
  (alexandria:when-let ((event (read-input waitp)))
    (handle-event *console* event)
    event))

(defgeneric handle-event (client event)
  (:method (client event)
    (declare (ignore client event))))

(define-condition exit () ())
(defun exit () (signal 'exit))
