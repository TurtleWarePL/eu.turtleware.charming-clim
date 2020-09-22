(in-package #:eu.turtleware.charming-clim)

;; gcc raw-mode.c -shared -o raw-mode.so
;; (cffi:load-foreign-library "/path/to/raw-mode.so")

(cffi:defcfun (enable-raw "enable_raw")
    :pointer)

(cffi:defcfun (disable-raw "disable_raw")
    :void
  (handler :pointer))

(defun init-terminal ()
  (prog1 (enable-raw)
    (reset-terminal)))

(defun close-terminal (handler)
  (disable-raw handler)
  (reset-terminal))

(defvar *terminal*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +delete+ (code-char #x7f)
    "The DEL character (#\Rubout), last in the ASCII table.")
  (defconstant +escape+ (code-char #x1b)
    "The ESC character (#\esc)."))


(defvar *counter* 0)
(defun put (&rest args)
  "Put raw string on a terminal"
  (let* ((str (format nil "~{~a~}" args))
         (len (length str)))
    (incf *counter* len)
    (princ str *terminal*)))

(defun esc (&rest args)
  "Escape sequence"
  (apply #'put +escape+ args))

(defun csi (&rest args)
  "Control sequence introducer"
  (apply #'esc #\[ args))

(defun sgr (&rest args)
  "Select Graphic Rendition"
  (apply #'csi (append args '("m"))))


(defun reset-terminal ()
  "Clears the screen, attributes, cursor position etc."
  (esc "c"))

(defun clear-terminal (&optional (mode 2))
  "Erase in display"
  ;; Defined modes:
  ;; 0 - clear from cursor to the end of the display
  ;; 1 - clear from cursor to the start of the display
  ;; 2 - clear entire display
  (csi mode "J"))

(defun clear-line (&optional (mode 2))
  "Erase in line"
  ;; Defined modes:
  ;; 0 - clear from cursor to the end of the line
  ;; 1 - clear from cursor to the start of the line
  ;; 2 - clear entire line
  (csi mode "K"))

(defun set-foreground-color (color)
  (let ((r (ldb (byte 8 24) color))
        (g (ldb (byte 8 16) color))
        (b (ldb (byte 8  8) color))
        (a (ldb (byte 8  0) color)))
    (declare (ignore a))
    (sgr "38;2;" r ";" g ";" b)))

(defun set-background-color (color)
  (let ((r (ldb (byte 8 24) color))
        (g (ldb (byte 8 16) color))
        (b (ldb (byte 8  8) color))
        (a (ldb (byte 8  0) color)))
    (declare (ignore a))
    (sgr "48;2;" r ";" g ";" b)))


(defun set-text-style (text-style)
  (loop for (key val) on text-style by #'cddr
        collect (ecase key
                  (:intensity
                   (ecase val
                     (:faint 2)
                     (:normal 22)
                     (:bold 1)))
                  (:underline
                   (ecase val
                     (:none 24)
                     (:single 4)
                     (:double 21)))
                  (:italicized (if val 3 23))
                  (:blink      (if val 5 25))
                  (:inverse    (if val 7 27))
                  (:invisible  (if val 8 28))
                  (:crossout   (if val 9 29)))
          into numbers
        finally (when numbers
                  (csi (format nil "~{~a~^;~}" (remove nil numbers)) "m"))))

(defun reset-cursor-attributes ()
  (csi "0" "m"))

(defun save-cursor-position ()
  (csi "s"))

(defun restore-cursor-position ()
  (csi "u"))

(macrolet ((moveit (endch)
             `(if (= n 1)
                  (csi ,endch)
                  (csi n ,endch))))
  (defun cursor-up    (&optional (n 1)) (moveit "A"))
  (defun cursor-down  (&optional (n 1)) (moveit "B"))
  (defun cursor-right (&optional (n 1)) (moveit "C"))
  (defun cursor-left  (&optional (n 1)) (moveit "D")))

(defun set-cursor-position (row col)
  (check-type row integer)
  (check-type col integer)
  (csi row ";" col "H"))

(defmacro with-cursor-position ((row col) &body body)
  `(progn
     (save-cursor-position)
     (set-cursor-position ,row ,col)
     (unwind-protect (progn ,@body)
       (restore-cursor-position))))

(defun set-cursor-visibility (visiblep)
  (if visiblep
      (csi "?" 2 5 "h")
      (csi "?" 2 5 "l")))

;;; (csi ? tracking ; encoding h/l)
;;; tracking: 1000 - normal, 1002 - button, 1003 - all motion
;;;           1004 - focus in/out
;;; encoding: 1006 - sgr encoding scheme
(defun set-mouse-tracking (enabledp)
  (if enabledp
      (csi "?" 1003 ";" 1006 "h")
      (csi "?" 1003 "l")))

(defun request-cursor-position ()
  (csi 6 "n"))

#+poor-coverage
(progn
  (defun request-terminal-ch-size ()
    (csi "18t"))

  (defun request-terminal-px-size ()
    (csi "14t"))

  (defun request-cell-px-size ()
    (csi "16t")))


;;; Input

(defclass event () ())

(defclass terminal-event (event) ())

(defclass unknown-terminal-event (terminal-event)
  ((seq :initarg :seq :accessor seq)))

;;; When this event is signaled then we have a bug!
(defclass oink-terminal-event (unknown-terminal-event)
  ((err :initarg :err :accessor err)
   (res :initarg :res :accessor res)))

(defclass cursor-position-event (terminal-event)
  ((row :initarg :row :accessor row)
   (col :initarg :col :accessor col)))

(defclass terminal-resize-event (terminal-event)
  ((rows :initarg :rows :accessor rows)
   (cols :initarg :cols :accessor cols)))

#+poor-coverage
(progn
  (defclass terminal-px-size-event (terminal-event)
    ((height :initarg :height :accessor height)
     (width :initarg :width :accessor width)))

  (defclass cell-px-size-event (terminal-event)
    ((height :initarg :height :accessor height)
     (width :initarg :width :accessor width)))

  (defclass terminal-ch-size-event (terminal-event)
    ((rows :initarg :rows :accessor rows)
     (cols :initarg :cols :accessor cols))))

(defclass keyboard-event (event)
  ((key :initarg :key :accessor key)
   (kch :initarg :kch :accessor kch)
   (mods :initarg :mods :accessor mods))
  (:default-initargs :mods 0 :kch nil))

(defclass pointer-event (event)
  ((row :initarg :row :accessor row)
   (col :initarg :col :accessor col)
   (btn :initarg :btn :accessor btn)
   (mods :initarg :mods :accessor mods)
   (state :initarg :state :accessor state))
  (:default-initargs :mods 0 :btn :none :state :motion))

(defclass pointer-motion-event  (pointer-event) ())
(defclass pointer-press-event   (pointer-event) ())
(defclass pointer-release-event (pointer-event) ())

(defvar *request-terminal-size* nil
  "When bound to T, cursor position report returns TERMINAL-RESIZE-EVENT.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant  +c1-mod+   16)
  (defconstant  +meta-mod+  8)
  (defconstant  +ctrl-mod+  4)
  (defparameter +alt-mod+   2)
  (defconstant  +alt-mod*+  2)
  (defconstant  +shift-mod+ 1))

(defun decode-mods (mods)
  (loop for p in (list +c1-mod+
                       +meta-mod+
                       +ctrl-mod+
                       +alt-mod*+
                       +shift-mod+)
        for k in '(:c1 :meta :ctrl :alt :shift)
        unless (zerop (logand mods p))
          collect k))

(defun set-alt-is-meta (bool)
  (if bool
      (setf +alt-mod+ +meta-mod+)
      (setf +alt-mod+ +alt-mod*+)))

(defmethod print-object ((o pointer-event) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~s ~s [~s] [~s]" (row o) (col o) (btn o)
            (decode-mods (mods o)))))

(defmethod print-object ((o keyboard-event) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~a [~s]" (key o) (decode-mods (mods o)))))

(defmethod print-object ((o cursor-position-event) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~s ~s" (row o) (col o))))

(defmethod print-object ((o unknown-terminal-event) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "~s" (seq o))))

(defun control-char-p (ch &aux (code (char-code ch)))
  (or (<= 0 code 31)
      (<= 128 code 159)))

(defun controlp (ch &aux (code (char-code ch)))
  "Predicate determining if the character is a control character.
Returns a generalized boolean (when true returns an event)."
  (cond ((<= 0 code 31)
         (make-instance 'keyboard-event
                        :key (code-char (+ code 64))
                        :mods +ctrl-mod+))
        ((<= 128 code 159)
         (make-instance 'keyboard-event
                        :key (code-char (- code 64))
                        :mods +c1-mod+))))

(defvar *key-resolvers* (make-hash-table))

(defmacro define-key-resolver (group terminator args &body body)
  `(setf (gethash ,(+ (char-code terminator)
                      (ash (char-code group) 8))
                  *key-resolvers*)
         (lambda ,args
           ,@body)))

(defun maybe-combo (key num2)
  (alexandria:if-let ((ctrl (and (characterp key) (controlp key))))
    (prog1 ctrl
      (setf (mods ctrl) (logior (1- num2) +ctrl-mod+)))
    (if (null num2)
        (make-instance 'keyboard-event :key key :mods 0)
        (make-instance 'keyboard-event :key key :mods (1- num2)))))

(define-key-resolver #\[ #\~ (num1 &optional num2)
  (let ((key (case num1
               (1 :home) (2  :insert) (3    :delete)
               (4  :end) (5 :page-up) (6 :page-down)
               (11  :f1) (12 :f2)  (13  :f3) (14  :f4) ; deprecated
               (15  :f5) (17 :f6)  (18  :f7) (19  :f8)
               (20  :f9) (21 :f10) (23 :f11) (24 :f12)
               (25 :f13) (26 :f14) (28 :f15) (29 :f16)
               (31 :f17) (32 :f18) (33 :f19) (34 :f20))))
    (maybe-combo key num2)))

;;; fish fish
(define-key-resolver #\[ #\A (num &optional num2)
  (assert (= num 1))
  (maybe-combo :key-up num2))

(define-key-resolver #\[ #\B (num &optional num2)
  (assert (= num 1))
  (maybe-combo :key-down num2))

(define-key-resolver #\[ #\C (num &optional num2)
  (assert (= num 1))
  (maybe-combo :key-right num2))

(define-key-resolver #\[ #\D (num &optional num2)
  (assert (= num 1))
  (maybe-combo :key-left num2))

(define-key-resolver #\[ #\P (num &optional num2)
  (assert (= num 1))
  (maybe-combo :f1 num2))

(define-key-resolver #\[ #\Q (num &optional num2)
  (assert (= num 1))
  (maybe-combo :f2 num2))

(define-key-resolver #\[ #\S (num &optional num2)
  (assert (= num 1))
  (maybe-combo :f4 num2))

;;; This is a key in the center the numeric pad.
(define-key-resolver #\[ #\E (num &optional num2)
  (assert (= num 1))
  (maybe-combo :center num2))

(define-key-resolver #\[ #\F (num &optional num2)
  (assert (= num 1))
  (maybe-combo :end num2))

(define-key-resolver #\[ #\H (num &optional num2)
  (assert (= num 1))
  (maybe-combo :home num2))

;;; This is ambiguous, MOD+F3 may be easily confused with the cursor position
;;; event. Cursor position reports are far more important, so we disable this.
#+ (or)
(define-key-resolver #\[ #\R (num &optional num2)
  (assert (= num 1))
  (maybe-combo :f3 num2))

(define-key-resolver #\[ #\R (row col)
  (if *request-terminal-size*
      (make-instance 'terminal-resize-event :rows row :cols col)
      (make-instance 'cursor-position-event :row row :col col)))

#+poor-coverage
(define-key-resolver #\[ #\t (opcode height width)
  ;; These reports are xterm extensions. There are more! But we do not provide
  ;; requests for them (i.e for toggle full-screen).
  (case opcode
    (4 (make-instance 'terminal-px-size-event :height height :width width))
    (6 (make-instance 'cell-px-size-event     :height height :width width))
    (8 (make-instance 'terminal-ch-size-event :rows   height :cols  width))))

(defun resolve-key (group args |Hasta la vista, baby|)
  (if (null |Hasta la vista, baby|)
      ;; When there is no terminating character, then it is probably a
      ;; result of pressing ALT+<char>. This is ambigous, i.e ALT+[
      ;; generates CSI. We try to be as robust as we can here.
      (maybe-combo (case group
                     (#.+escape+ :escape)
                     (#.+delete+ :delete)
                     (t group))
                   (1+ +alt-mod+))
      (let ((resolver
              (gethash (+ (char-code |Hasta la vista, baby|)
                          (ash (char-code group) 8))
                       *key-resolvers*
                       (lambda (&rest args)
                         (make-instance 'unknown-terminal-event
                                        :seq (list +escape+
                                                   group
                                                   args
                                                   |Hasta la vista, baby|))))))
        (handler-case (apply resolver args)
          (error (condition)
            (cerror "continue" "seq ~s~%err ~a~%res ~s"
                    (list +escape+ group args |Hasta la vista, baby|)
                    condition
                    resolver)
            (make-instance 'oink-terminal-event
                           :seq (list +escape+
                                      group
                                      args
                                      |Hasta la vista, baby|)
                           :err condition
                           :res resolver))))))

(defun resolve-mouse (btn col row |Hasta la vista, baby|)
  (let ((state (cond ((not (zerop (ldb (byte 1 5) btn))) :motion)
                     ((char= #\M |Hasta la vista, baby|) :press)
                     ((char= #\m |Hasta la vista, baby|) :release)))
        (mods (+ (if (zerop (ldb (byte 1 2) btn)) 0 +shift-mod+)
                 (if (zerop (ldb (byte 1 3) btn)) 0 +alt-mod+)
                 (if (zerop (ldb (byte 1 4) btn)) 0 +ctrl-mod+)))
        (btn (case (+ (ldb (byte 2 0) btn)
                      (ash (ldb (byte 2 6) btn) 2))
               (#b0000 :left)
               (#b0001 :middle)
               (#b0010 :right)
               (#b0011 :none)
               ;; 64
               (#b0100 :wheel-up)
               (#b0101 :wheel-down)
               (#b0110 :wheel-left)
               (#b0111 :wheel-right)
               ;; 128 (xterm >= 341)
               (#b1000 :extra-1)
               (#b1001 :extra-2)
               (#b1010 :extra-3)
               (#b1011 :extra-4))))
    (make-instance (ecase state
                     (:motion 'pointer-motion-event)
                     (:press 'pointer-press-event)
                     (:release 'pointer-release-event))
                   :row row :col col :btn btn :mods mods :state state)))

(defun parse-escape-sequence (&aux char)
  (flet ((read-num ()
           (loop while (and char (digit-char-p char))
                 collecting char into num
                 do (setf char (read-char-no-hang *terminal*))
                 finally (when num
                           (return (parse-integer (coerce num 'string)))))))
    (loop
      do (setf char (read-char-no-hang *terminal*))
      collect (or (read-num) 1) into nums
      until (or (null char)
                (char/= #\; char))
      finally (return (values nums char)))))

(defun escapep (ch)
  (unless (char= ch +escape+)
    (return-from escapep nil))
  (alexandria:if-let ((next-ch (read-char-no-hang *terminal*)))
    ;; A keycode: [\[NO](<num>)(;<num>)[~A-Z].
    ;; SGR mouse: '[' '<' num ';' num ';' num ';' [Mm]
    (if (and (char= #\[ next-ch)
             (char= #\< (peek-char t *terminal* nil #\x))
             (read-char-no-hang *terminal*))
        (multiple-value-bind (nums terminator)
            (parse-escape-sequence)
          (destructuring-bind (num1 num2 num3) nums
            (resolve-mouse num1 num2 num3 terminator)))
        (multiple-value-bind (nums terminator)
            (parse-escape-sequence)
          (resolve-key next-ch nums terminator)))
    (make-instance 'keyboard-event :key :escape :mods 0)))

(defun deletep (ch)
  (when (char= ch +delete+)
    (make-instance 'keyboard-event :key :delete :mods 0)))

(defun read-input (&optional (waitp nil))
  ;; READ-CHAR may read more than one byte and return an alphanumeric
  ;; character.
  (let ((ch (if waitp
                (read-char *terminal*)
                (read-char-no-hang *terminal*))))
    (cond ((null ch)
           (return-from read-input))
          ((graphic-char-p ch)
           (return-from read-input
             (make-instance 'keyboard-event :kch ch :key ch :mods 0)))
          ((deletep ch))
          ((escapep ch))
          ((controlp ch))
          (t (make-instance 'unknown-terminal-event :seq (list ch))))))

(defun keyp (ch key &rest mods)
  (and (typep ch 'keyboard-event)
       (eql (key ch) key)
       (eql (mods ch)
            (loop for m in mods
                  summing (ecase m
                            (:c1 +c1-mod+)
                            (:m  +meta-mod+)
                            (:c  +ctrl-mod+)
                            (:a  +alt-mod*+)
                            (:s  +shift-mod+))))))
