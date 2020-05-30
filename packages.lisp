(defpackage #:eu.turtleware.charming-clim.terminal/ll
  (:export #:init-console
           #:close-console
           #:*console-io*

           #:put #:esc #:csi #:sgr
           #:read-input #:keyp

           #:reset-console
           #:clear-console
           #:clear-rectangle
           #:clear-line
           #:set-foreground-color
           #:set-background-color

           #:with-cursor-position
           #:set-cursor-position
           #:save-cursor-position
           #:restore-cursor-position
           #:request-cursor-position

           #:cursor-up
           #:cursor-down
           #:cursor-right
           #:cursor-left

           #:set-cursor-visibility
           #:set-mouse-tracking))

(defpackage #:eu.turtleware.charming-clim.terminal/hl
  (:export #:with-console #:out #:ctl))

(defpackage #:eu.turtleware.charming-clim
  (:use #:common-lisp
        #:eu.turtleware.charming-clim.terminal/ll
        #:eu.turtleware.charming-clim.terminal/hl))
