(defpackage #:eu.turtleware.charming-clim/l0
  (:export #:init-terminal
           #:close-terminal
           #:*terminal*

           #:put #:esc #:csi #:sgr
           #:read-input #:keyp

           #:reset-terminal
           #:clear-terminal

           #:clear-line
           #:set-foreground-color
           #:set-background-color
           #:set-text-style
           #:reset-cursor-attributes

           #:with-cursor-position
           #:set-cursor-position
           #:save-cursor-position
           #:restore-cursor-position

           #:cursor-up
           #:cursor-down
           #:cursor-right
           #:cursor-left

           #:set-cursor-visibility
           #:set-mouse-tracking
           #:set-alt-is-meta

           #:*request-terminal-size*
           #:request-cursor-position

           #:event
           #:terminal-event
           #:unknown-terminal-event #:seq
           #:oink-terminal-event #:err #:res
           #:cursor-position-event #:row #:col
           #:terminal-resize-event #:rows #:cols
           #:keyboard-event #:key #:kch #:mods
           #:pointer-event #:row #:col #:btn #:mods #:state))

(defpackage #:eu.turtleware.charming-clim/l1
  (:use #:eu.turtleware.charming-clim/l0)
  (:export #:with-console #:*console* #:console #:handle-event
           #:process-next-event #:process-available-events #:exit)
  (:export #:with-buffer #:*buffer* #:output-buffer #:out #:ctl
           #:buffer-cursor #:direct-cursor
           #:set-cell #:put-cell #:flush-output
           #:bbox #:rows #:cols #:resize-buffer
           #:with-clipping #:invoke-with-clipping #:inside-p)
  (:export #:surface #:sink #:offset
           #:scroll-surface #:move-surface #:reshape-surface))

(defpackage #:eu.turtleware.charming-clim/l2
  (:use #:eu.turtleware.charming-clim/l0
        #:eu.turtleware.charming-clim/l1)
  (:export))

(defpackage #:eu.turtleware.charming-clim
  (:use #:common-lisp
        #:eu.turtleware.charming-clim/l0
        #:eu.turtleware.charming-clim/l1
        #:eu.turtleware.charming-clim/l2)
  (:local-nicknames (#:ax #:alexandria)))
