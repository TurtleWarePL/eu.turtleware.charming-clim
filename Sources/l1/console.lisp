(in-package #:eu.turtleware.charming-clim)

(defvar *console*)

(defmacro with-console ((&rest args
                         &key
                           ios
                           (console-class ''console)
                         &allow-other-keys)
                        &body body)
  (remf args :console-class)
  `(let ((*terminal* ,ios)
         (console-class ,console-class))
     (loop
       (restart-case
           (let ((*console* (make-instance console-class ,@args)))
             (handler-case (with-buffer (*console*) ,@body)
               (error (e)
                 (set-mouse-tracking nil)
                 (process-available-events)
                 (close-terminal (hnd *console*))
                 (error e))
               (exit (e)
                 (declare (ignore e))
                 (set-mouse-tracking nil)
                 (process-available-events)
                 (close-terminal (hnd *console*))
                 (return))
               (:no-error (&rest values)
                 (declare (ignore values))
                 (set-mouse-tracking nil)
                 (process-available-events)
                 (close-terminal (hnd *console*))
                 (return))))
         (again ()
           :report "Start display again.")
         (change (new-console-class)
           :report "Change the console class."
           :interactive (lambda ()
                          (format *debug-io* "Type the class symbol:~%")
                          (format *debug-io* "~a> '" (package-name *package*))
                          (finish-output *debug-io*)
                          (list (read)))
           (setf console-class new-console-class))))))

(defclass console (output-buffer)
  ((ios :initarg :ios :accessor ios :documentation "Console I/O stream")
   (cur :initarg :cur :accessor cur :documentation "The terminal cursor"
        :reader direct-cursor)
   (ptr :initarg :ptr :accessor ptr :documentation "The pointer cursor")
   (vrt :initarg :vrt :accessor vrt :documentation "The virtual pointer cursor")
   (hnd               :accessor hnd :documentation "Terminal handler"))
  (:default-initargs :ios (error "I/O stream must be specified.")))

(defmethod initialize-instance :after
    ((instance console) &rest args &key ios fgc bgc
     &aux (*terminal* ios) (*console* instance))
  (declare (ignore args))
  (setf (hnd instance) (init-terminal))
  (setf (cur instance) (make-instance 'tcursor :cvp nil :fgc fgc :bgc bgc))
  (setf (ptr instance) (make-instance 'pointer :cvp t :cep t))
  (setf (vrt instance) (make-instance 'vpointer :cvp t :cep nil))
  (set-alt-is-meta t)
  (process-available-events t))

;;; Cursors are showed after flushing output and they are presented in direct
;;; mode, so they don't modify the actual buffered cell. When we move cursor
;;; we want to "see" its previous content.
(defun show-cursors (console)
  (flet ((show-cursor (ptr)
           (when (and (cep ptr) (cvp ptr))
             (let ((row (row ptr))
                   (col (col ptr))
                   (txt (txt ptr))
                   (fgc (fgc ptr))
                   (bgc (bgc ptr)))
               (out (:row row :col col :txt txt :fgc fgc :bgc bgc) ptr)
               (out (:row (1+ row) :col col :txt txt :fgc fgc :bgc bgc)
                    (cursor-data ptr))))))
    (letf (((mode console) :dir))
      (show-cursor (ptr console))
      (show-cursor (vrt console)))))

(defmethod flush-output ((buffer console) &rest args &key force)
  (declare (ignore args))
  (let* ((cursor (cur buffer))
         (last-fgc (fgc cursor))
         (last-bgc (bgc cursor))
         (last-txt (txt cursor))
         (gap 0))
    (set-cursor-position 1 1)
    (iterate-cells (cell crow ccol wrap-p)
        (buffer 1 1 (make-array (* (cols buffer)
                                   (rows buffer))
                                :displaced-to (data buffer)))
      (when wrap-p
        (set-cursor-position crow ccol)
        (setf gap 0))
      (if (and cell (or force (dirty-p cell)))
          (let ((chr (chr cell))
                (fgc (fgc cell))
                (bgc (bgc cell))
                (txt (txt cell)))
            (unless (= fgc last-fgc)
              (set-foreground-color fgc)
              (setf last-fgc fgc))
            (unless (= bgc last-bgc)
              (set-background-color bgc)
              (setf last-bgc bgc))
            (alexandria:when-let ((diff (text-style-diff txt last-txt)))
              (set-text-style diff)
              (setf last-txt txt))
            (when (plusp gap)
              (cursor-right gap)
              (setf gap 0))
            (put chr)
            (setf (dirty-p cell) nil))
          (if force
              (put #\space)
              (incf gap))))
    (set-cursor-position (row cursor) (col cursor))
    (set-foreground-color (fgc cursor))
    (set-background-color (bgc cursor))
    (set-text-style (txt cursor))
    (show-cursors buffer))
  (finish-output *terminal*))

(defmethod put-cell ((buf console) str &rest cursor-args)
  (let* ((cur (cur buf)))
    (apply #'update-pen cur cursor-args)
    (iterate-cells (chr crow ccol wrap-p)
        (buf (row cur) (col cur) (string str))
      (when wrap-p
        (set-cursor-position crow ccol))
      (if (inside-p buf crow ccol)
          (put chr)
          (cursor-right)))
    (set-cursor-position (row cur) (col cur))))

(defmethod handle-event :before ((client console) (event terminal-resize-event))
  (let ((rows (rows event))
        (cols (cols event)))
    (setf (rows client) rows)
    (setf (cols client) cols)
    (setf (r2 (clip client)) rows)
    (setf (c2 (clip client)) cols)
    (let* ((vptr (vrt client))
           (row (alexandria:clamp (row vptr) 1 rows))
           (col (alexandria:clamp (col vptr) 1 cols)))
      (change-cursor-position vptr row col))
    (adjust-array (data client)
                  (list rows cols)
                  :initial-element nil)))

(defmethod handle-event :before ((client console) (event pointer-event))
  (let ((ptr (pointer event))
        (btn (btn event)))
    (change-cursor-data ptr event)
    (change-cursor-position ptr (row event) (col event))))

(defmethod handle-event :before ((client console) (event keyboard-event))
  ;; Handle the virtual pointer.
  (handle-vptr client (vrt client) event))

(defmethod handle-event ((client console) (event keyboard-event))
  (cond ((keyp event #\Q :c)
         (signal 'exit))
        ((keyp event #\E :c)
         (error "HI!"))
        ((keyp event #\S :c)
         (swank:create-server :dont-close t))
        ((keyp event #\R :c)
         (ctl (:ink #xffffffff #x00000000))
         (ctl (:clr 1 1 (rows *console*) (cols *console*)))
         (ctl (:fls :force t))
         (process-available-events t))))

;;; Since the terminal does not handle key combinations (i.e C-a-b), because
;;; only characters arrive (and not press/release pairs), and because we want
;;; to allow drag-and-drop gestures in the virtual pointer, we need to
;;; introduce the "toggle button" combinations, so when we move the pointer
;;; the button is still "pressed", until manually released.  For convenience,
;;; C-<arrow> moves a pressed cursor. There are no counterpart shortcuts for
;;; other buttons. F8 is bound to "toggle button middle" to make it usable on
;;; keyboards without the keypad.
(defun handle-vptr (client vptr key)
  (unless (cursor-enabledp vptr)
    (when (keyp key :page-down :m)
      (change-cursor-enabledp vptr t))
    (return-from handle-vptr))
  (let ((event (cursor-data vptr))
        (toggled (toggled-btn vptr)))
    (labels ((set-position (row col)
               (let ((row (alexandria:clamp row 1 (rows client)))
                     (col (alexandria:clamp col 1 (cols client))))
                 (setf (row event) row
                       (col event) col)))
             (click (btn)
               (setf (btn event) btn)
               (setf (state event) :press)
               (handle-event client event)
               (setf (state event) :release)
               (handle-event client event))
             (press (btn)
               (letf (((btn event) btn)
                      ((state event) :press))
                 (handle-event client event)))
             (release (btn)
               (letf (((btn event) btn)
                      ((state event) :release))
                 (handle-event client event)))
             (move (row-dx col-dx &aux (btn (btn event)))
               (unless (eql btn toggled)
                 (unless (eql btn :none)
                   (release btn))
                 (setf (btn event) toggled))
               (setf (state event) :motion)
               (set-position (+ (row event) row-dx)
                             (+ (col event) col-dx))
               (handle-event client event))
             (move* (row-dx col-dx btn)
               (unless (eql btn (btn event))
                 (unless (eql btn toggled)
                   (press btn))
                 (setf (btn event) btn))
               (setf (state event) :motion)
               (set-position (+ (row event) row-dx)
                             (+ (col event) col-dx))
               (handle-event client event))
             (toggle-btn (btn)
               ;; We can't toggle "none" and "wheel" buttons.
               (check-type btn (member :left :right :middle
                                       :extra-1 :extra-2 :extra-3 :extra-4))
               (cond ((eq toggled btn)
                      (setf (toggled-btn vptr) :none)
                      (setf (state event) :release)
                      (handle-event client event))
                     ((eq toggled :none)
                      (setf (toggled-btn vptr) btn)
                      (setf (btn event) btn)
                      (setf (state event) :press)
                      (handle-event client event))
                     (t
                      ;; Release the old toggle.
                      (setf (btn event) toggled)
                      (setf (state event) :release)
                      (handle-event client event)
                      ;; Press the new toggle.
                      (setf (toggled-btn vptr) btn)
                      (setf (btn event) btn)
                      (setf (state event) :press)
                      (handle-event client event))))
             (toggle (mod)
               (setf (mods event)
                     (logxor (mods event) mod)))
             (option (name)
               (ecase name
                 (:pos
                  (change-cursor-position vptr 1 1)
                  (setf (row event) 1
                        (col event) 1))
                 (:key
                  (when (toggled-btn vptr)
                    (setf (btn event) toggled)
                    (setf (state event) :release)
                    (handle-event client event))
                  (setf (toggled-btn vptr) :none
                        (btn event) :none
                        (state event) :motion
                        (mods event) 0)))))
      (key-case key
        ;; Cursor manipulation
        (:key-up         (move -1 00))         ; move up
        (:key-down       (move +1 00))         ; move down
        (:key-left       (move 00 -1))         ; move left
        (:key-right      (move 00 +1))         ; move right
        ;; Left-pressed navigation
        ((:key-up :c)    (move* -1 0 :left))   ; left-pressed move up
        ((:key-down :c)  (move* +1 0 :left))   ; left-pressed move down
        ((:key-left :c)  (move* 0 -1 :left))   ; left-pressed move left
        ((:key-right :c) (move* 0 +1 :left))   ; left-pressed move right
        ;; Clicks
        (:insert         (click :left))        ; left click
        (:delete         (click :right))       ; right click
        (:center         (click :middle))      ; middle click
        ;; Presses (no release event)
        (:home           (press :wheel-left))  ; scroll left
        (:end            (press :wheel-right)) ; scroll right
        (:page-up        (press :wheel-up))    ; scroll up
        (:page-down      (press :wheel-down))  ; scroll down
        ;; Toggles/reset
        ((:key-up :m)    (option :pos)) ; reset position
        ((:key-down :m)  (option :key)) ; reset all toggles
        ;; This is truly meta, we use meta to simulate control and meta :)
        ((:key-left :m)  (toggle +ctrl-mod+))   ; toggle control
        ((:key-right :m) (toggle +meta-mod+))   ; toggle meta
        ;;
        ((:insert :m)    (toggle-btn :left))    ; toggle button left
        ((:delete :m)    (toggle-btn :right))   ; toggle button right
        ((:center :m)    (toggle-btn :middle))  ; toggle button middle
        ;;
        ((:home :m)      (toggle-btn :extra-1)) ; toggle button extra-1
        ((:end :m)       (toggle-btn :extra-2)) ; toggle button extra-2
        ((:page-up :m)   (toggle-btn :extra-3)) ; toggle button extra-3
        ((:page-down :m) (toggle-btn :extra-4)) ; toggle button extra-4
        (t (return-from handle-vptr nil)))))
  t)
