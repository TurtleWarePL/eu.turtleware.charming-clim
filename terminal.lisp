(defpackage #:eu.turtleware.charming-clim
  (:use #:cl)
  (:export #:with-console #:out #:ctl))
(in-package #:eu.turtleware.charming-clim)

;; gcc raw-mode.c -shared -o raw-mode.so
;; (cffi:load-foreign-library "/path/to/raw-mode.so")

(cffi:defcfun (enable-raw "enable_raw")
    :pointer)

(cffi:defcfun (disable-raw "disable_raw")
    :void
  (handler :pointer))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +delete+ (code-char #x7f)
    "The DEL character (#\Rubout), last in the ASCII table.")
  (defconstant +escape+ (code-char #x1b)
    "The ESC character (#\esc)."))


(defmacro letf (bindings &body body)
  (loop for (place value) in bindings
        for old-val = (gensym)
        collect `(,old-val ,place)      into saves
        collect `(setf ,place ,value)   into store
        collect `(setf ,place ,old-val) into restore
        finally (return `(let (,@saves)
                           (unwind-protect (progn ,@store ,@body)
                             ,@restore)))))

(defvar *row1* '(1))
(defvar *col1* '(1))
(defvar *row2* '(24))
(defvar *col2* '(80))
(defvar *fun* (list (constantly t)))

(defmacro with-clipping ((&key fun row1 col1 row2 col2) &body body)
  `(let (,@(when row1 `((*row1* (cons (max (car *row1*) ,row1) *row1*))))
         ,@(when col1 `((*col1* (cons (max (car *col1*) ,col1) *col1*))))
         ,@(when row2 `((*row2* (cons (min (car *row2*) ,row2) *row1*))))
         ,@(when col2 `((*col2* (cons (min (car *col2*) ,col2) *col2*))))
         ,@(when fun  `((*fun*  (cons (let ((old (car *fun*)))
                                        (lambda (row col)
                                          (and (funcall ,fun row col)
                                               (funcall old row col))))
                                      *fun*)))))
     ,@body))

(defun inside (row col)
  (and (<= (car *row1*) row (car *row2*))
       (<= (car *col1*) col (car *col2*))
       (funcall (car *fun*) row col)))

(defmacro out ((&key row col fgc bgc) object)
  "Put an object on a console"
  `(let ((str (princ-to-string ,object)))
     (assert (null (find #\newline str)))
     (letf (((pos *console*) (cons (or ,row (car (pos *console*)))
                                   (or ,col (cdr (pos *console*)))))
            ,@(when fgc `(((fgc *console*) ,fgc)))
            ,@(when bgc `(((bgc *console*) ,bgc))))
       (let* ((pos (pos *console*))
              (row (car pos))
              (col (cdr pos)))
         (loop for c from col
               for s across str
               when (inside row c)
                 do (put s))))))

(defmacro ctl (&rest operations)
  `(progn
     ,@(loop for op in operations
             collect (destructuring-bind (name &rest args) op
                       (ecase name
                         (:clr `(clear-rectangle ,@args))
                         (:fgc `(setf (fgc *console*) (list ,@args)))
                         (:bgc `(setf (bgc *console*) (list ,@args)))
                         (:cvp `(setf (cursor-visibility) ,@args))
                         (:ptr `(setf (mouse-tracking) ,@args))
                         (:pos `(setf (pos *console*) (cons ,(car args)
                                                            ,(cdr args)))))))))


(defun put (&rest args)
  "Put raw string on a console"
  (format *console-io* "~{~a~}" args)
  (finish-output *console-io*))

(defun esc (&rest args)
  "Escape sequence"
  (apply #'put +escape+ args))

(defun csi (&rest args)
  "Control sequence introducer"
  (apply #'esc #\[ args))

(defun sgr (&rest args)
  "Select Graphic Rendition"
  (apply #'csi (append args '("m"))))


(defun reset-console ()
  "Clears the screen, attributes, cursor position etc."
  (esc "c"))

(defun clear-console (&optional (mode 2))
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

(defun clear-rectangle (r1 c1 r2 c2)
  (loop with str = (make-string (1+ (- c2 c1)) :initial-element #\space)
        for r from r1 upto r2
        do (out (:row r :col c1) str)))

(defun set-foreground-color (r g b)
  (sgr "38;2;" r ";" g ";" b))

(defun set-background-color (r g b)
  (sgr "48;2;" r ";" g ";" b))

(defun save-cursor-position ()
  (csi "s"))

(defun restore-cursor-position ()
  (csi "u"))

(defun set-cursor-position (row col)
  (cond ((and row col)    (csi row ";" col "H"))
        ((not (null row)) (csi row ";H"))
        ((not (null col)) (csi ";" col "H"))))

(defmacro with-cursor-position ((row col) &body body)
  `(progn
     (save-cursor-position)
     (set-cursor-position ,row ,col)
     (unwind-protect (progn ,@body)
       (restore-cursor-position))))

(defun (setf cursor-visibility) (visiblep)
  (if visiblep
      (csi "?" 2 5 "h")
      (csi "?" 2 5 "l")))

;;; (csi ? tracking ; encoding h/l)
;;; tracking: 1000 - normal, 1002 - button, 1003 - all motion
;;;           1004 - focus in/out
;;; encoding: 1006 - sgr encoding scheme
(defun (setf mouse-tracking) (enabledp)
  (if enabledp
      (csi "?" 1003 ";" 1006 "h")
      (csi "?" 1003 "l")))

(defun request-cursor-position ()
  (csi 6 "n"))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant  +c1-mod+   16)
  (defconstant  +meta-mod+  8)
  (defconstant  +ctrl-mod+  4)
  (defparameter +alt-mod+   2)
  (defconstant  +alt-mod*+  2)
  (defconstant  +shift-mod+ 1))

(defun (setf alt-is-meta) (bool)
  (if bool
      (setf +alt-mod+ +meta-mod+)
      (setf +alt-mod+ +alt-mod*+)))

(defclass gesture ()
  ((key  :initarg :key  :accessor gesture-key)
   (mods :initarg :mods :accessor gesture-mods)))

(defmethod print-object ((o gesture) s)
  (print-unreadable-object (o s :type nil :identity nil)
    (let ((key (gesture-key o))
          (mods (gesture-mods o)))
      (format s "~s ~s"
              key
              (loop for p in (list +c1-mod+
                                   +meta-mod+
                                   +ctrl-mod+
                                   +alt-mod*+
                                   +shift-mod+)
                    for k in '(:C1 :Meta :Ctrl :Alt :Shift)
                    unless (zerop (logand mods p))
                      collect k)))))

(defun control-char-p (ch &aux (code (char-code ch)))
  (or (<= 0 code 31)
      (<= 128 code 159)))

(defun controlp (ch &aux (code (char-code ch)))
  "Predicate determining if the character is a control character.
Returns a generalized boolean (when true returns a gesture)."
  (cond ((<= 0 code 31)
         (make-instance 'gesture
                        :mods +ctrl-mod+
                        :key (code-char (+ code 64))))
        ((<= 128 code 159)
         (make-instance 'gesture
                        :mods +c1-mod+
                        :key (code-char (- code 64))))))

(defvar *key-resolvers* (make-hash-table))

(defmacro define-key-resolver (group terminator (num1 num2) &body body)
  `(setf (gethash ,(+ (char-code terminator)
                      (ash (char-code group) 8))
                  *key-resolvers*)
         (lambda (,num1 ,num2)
           (declare (ignorable ,num1 ,num2))
           ,@body)))

(defun maybe-combo (key num2)
  (alexandria:if-let ((ctrl (and (characterp key) (controlp key))))
    (prog1 ctrl
      (setf (gesture-mods ctrl) (logior (1- num2) +ctrl-mod+)))
    (or (and (= num2 1) key)
        (make-instance 'gesture :key key :mods (1- num2)))))

(define-key-resolver #\[ #\~ (num1 num2)
  (let ((key (case num1
               (1 :home) (2  :insert) (3    :delete)
               (4  :end) (5 :page-up) (6 :page-down)
               (11  :f1) (12 :f2)  (13  :f3) (14  :f4) ; deprecated
               (15  :f5) (17 :f6)  (18  :f7) (19  :f8)
               (20  :f9) (21 :f10) (23 :f11) (24 :f12)
               (25 :f13) (26 :f14) (28 :f15) (29 :f16)
               (31 :f17) (32 :f18) (33 :f19) (34 :f20))))
    (maybe-combo key num2)))

(define-key-resolver #\[ #\A (num1 num2) (maybe-combo :key-up    num2))
(define-key-resolver #\[ #\B (num1 num2) (maybe-combo :key-down  num2))
(define-key-resolver #\[ #\C (num1 num2) (maybe-combo :key-right num2))
(define-key-resolver #\[ #\D (num1 num2) (maybe-combo :key-left  num2))

(define-key-resolver #\O #\P (num1 num2) (maybe-combo :f1 num2))
(define-key-resolver #\O #\Q (num1 num2) (maybe-combo :f2 num2))
(define-key-resolver #\O #\R (num1 num2) (maybe-combo :f3 num2))
(define-key-resolver #\O #\S (num1 num2) (maybe-combo :f4 num2))

(define-condition cursor-position-report ()
  ((row :initarg :row :reader row)
   (col :initarg :col :reader col)))

(define-key-resolver #\[ #\R (row col)
  (signal 'cursor-position-report :row row :col col)
  (make-instance 'gesture
                 :key (format nil "Cursor position: ~s ~s" row col)
                 :mods 0))

(defun resolve-key (group num1 num2 |Hasta la vista, baby|)
  (if (null |Hasta la vista, baby|)
      ;; When there is no terminating character, then it is probably a
      ;; result of pressing ALT+<char>. This is ambigous, i.e ALT+[
      ;; generates CSI. We try to be as robust as we can here.
      (maybe-combo (case group
                     (#.+escape+ :escape)
                     (#.+delete+ :delete)
                     (t group))
                   (1+ +alt-mod+))
      (funcall (gethash (+ (char-code |Hasta la vista, baby|)
                           (ash (char-code group) 8))
                        *key-resolvers*
                        #'(lambda (num1 num2)
                            (let ((k (format nil
                                             "Unknown sequence: ESC ~c ~d ~d ~c"
                                             group num1 num2
                                             |Hasta la vista, baby|)))
                              (make-instance 'gesture :key k :mods 0))))
               num1 num2)))

(defun resolve-mouse (btn col row |Hasta la vista, baby|)
  (let ((state (cond ((not (zerop (ldb (cons 1 5) btn))) :motion)
                     ((char= #\M |Hasta la vista, baby|) :press)
                     ((char= #\m |Hasta la vista, baby|) :release)))
        (mods (+ (if (zerop (ldb (cons 1 2) btn)) 0 +shift-mod+)
                 (if (zerop (ldb (cons 1 3) btn)) 0 +alt-mod+)
                 (if (zerop (ldb (cons 1 4) btn)) 0 +ctrl-mod+)))
        (key (case (+ (ldb (cons 2 0) btn)
                      (ash (ldb (cons 2 6) btn) 2))
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
    (make-instance 'gesture
                   :key (format nil "row: ~2d col: ~2d [~a ~a] ~a"
                                row col key btn state)
                   :mods mods)))

(defun parse-escape-sequence (&aux char)
  (flet ((read-num ()
           (loop while (and char (digit-char-p char))
                 collecting char into num
                 do (setf char (read-char-no-hang *console-io*))
                 finally (when num
                           (return (parse-integer (coerce num 'string)))))))
    (loop
      do (setf char (read-char-no-hang *console-io*))
      collect (or (read-num) 1) into nums
      until (or (null char)
                (char/= #\; char))
      finally (return (values nums char)))))

(defun escapep (ch)
  (unless (char= ch +escape+)
    (return-from escapep nil))
  (alexandria:if-let ((next-ch (read-char-no-hang *console-io*)))
    ;; A keycode: [\[NO](<num>)(;<num>)[~A-Z].
    ;; SGR mouse: '[' '<' num ';' num ';' num ';' [Mm]
    (if (and (char= #\[ next-ch)
             (char= #\< (peek-char t *console-io* nil #\x))
             (read-char-no-hang *console-io*))
        (multiple-value-bind (nums terminator)
            (parse-escape-sequence)
          (destructuring-bind (num1 num2 num3) nums
            (resolve-mouse num1 num2 num3 terminator)))
        (multiple-value-bind (nums terminator)
            (parse-escape-sequence)
          (destructuring-bind (&optional (num1 1) (num2 1)) nums
            (resolve-key next-ch num1 num2 terminator))))
    :escape))

(defun deletep (ch)
  (when (char= ch +delete+)
    :delete))

(defun read-input (&aux (ch (read-char-no-hang *console-io*)))
  ;; READ-CHAR may read more than one byte and return an alphanumeric
  ;; character. That's fine because we will return it as-is then.
  (cond ((or (null ch) (graphic-char-p ch))
         (return-from read-input ch))
        ((deletep ch))
        ((escapep ch))
        ((controlp ch))
        (t (error "Unknown input sequence, char code 0x~x~%." (char-code ch)))))

(defun keyp (ch key &rest mods)
  (if (null mods)
      (eql ch key)
      (and (typep ch 'gesture)
           (eql (gesture-key ch) key)
           (eql (gesture-mods ch)
                (loop for m in mods
                      summing (ecase m
                                (:c1 +c1-mod+)
                                (:m  +meta-mod+)
                                (:c  +ctrl-mod+)
                                (:a  +alt-mod*+)
                                (:s  +shift-mod+)))))))



(defvar *console*)
(defvar *console-io*)

(defun init-console ()
  (prog1 (enable-raw)
    (reset-console)))

(defun close-console (handler)
  (reset-console)
  (disable-raw handler))

(defun get-cursor-position ()
  (request-cursor-position)
  (handler-case (loop (read-input))
    (cursor-position-report (c)
      (values (row c) (col c)))))

(defun update-console-dimensions ()
  (with-cursor-position ((expt 2 16) (expt 2 16))
    (multiple-value-bind (rows cols)
        (get-cursor-position)
      (setf (rows *console*) rows)
      (setf (cols *console*) cols)
      (setf *row2* (list rows))
      (setf *col2* (list cols)))))

(defclass console ()
  ((ios :initarg :ios :accessor ios :documentation "Console I/O stream.")
   (fgc :initarg :fgc :accessor fgc :documentation "Foregorund color.")
   (bgc :initarg :bgc :accessor bgc :documentation "Background color.")
   (pos :initarg :pos :accessor pos :documentation "Cursor position.")
   (cvp :initarg :cvp :accessor cvp :documentation "Cursor visibility.")
   (ptr :initarg :ptr :accessor ptr :documentation "Pointer tracking.")
   (fps :initarg :fps :accessor fps :documentation "Desired framerate.")
   (app :initarg :app :accessor app :documentation "Application state.")
   (hnd               :accessor hnd :documentation "Terminal handler.")
   (rows :accessor rows             :documentation "Terminal number of rows.")
   (cols :accessor cols             :documentation "Terminal number of cols."))
  (:default-initargs :ios (error "I/O stream must be specified.")
                     :fgc '(#xff #xa0 #xa0)
                     :bgc '(#x22 #x22 #x22)
                     :pos '(1 . 1)
                     :cvp nil
                     :ptr t
                     :fps 10
                     :app nil))

(defmethod initialize-instance :after
    ((instance console) &key fgc bgc pos cvp ptr)
  (setf (hnd instance) (init-console))
  (apply #'set-foreground-color fgc)
  (apply #'set-background-color bgc)
  (set-cursor-position (car pos) (cdr pos))
  (setf (cursor-visibility) cvp)
  (setf (mouse-tracking) ptr)
  (let ((*console* instance))
    (update-console-dimensions)))

(defmethod (setf fgc) :after (rgb (instance console))
  (apply #'set-foreground-color rgb))

(defmethod (setf bgc) :after (rgb (instance console))
  (apply #'set-background-color rgb))

(defmethod (setf pos) :before (pos (instance console))
  (check-type (car pos) (integer 1))
  (check-type (cdr pos) (integer 1)))

(defmethod (setf pos) :after (pos (instance console))
  (set-cursor-position (car pos) (cdr pos)))

(defmethod (setf ptr) :after (ptr (instance console))
  (setf (mouse-tracking) (not (null ptr))))

(defmethod (setf cvp) :after (cvp (instance console))
  (setf (cursor-visibility) (not (null cvp))))

(defmacro with-console ((&rest args
                         &key ios fgc bgc cvp fps &allow-other-keys)
                        &body body)
  (declare (ignore fgc bgc cvp fps))
  `(let* ((*console-io* ,ios)
          (*console* (make-instance 'console ,@args)))
     (unwind-protect (progn ,@body)
       (close-console (hnd *console*)))))

