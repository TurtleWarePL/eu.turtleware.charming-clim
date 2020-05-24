(defpackage #:eu.turtleware.charming-clim
  (:use #:cl)
  (:export #:start-display))
(in-package #:eu.turtleware.charming-clim)

;; gcc raw-mode.c -shared -o raw-mode.so
;; (cffi:load-foreign-library "/path/to/raw-mode.so")

(cffi:defcfun (enable-raw "enable_raw")
    :pointer)

(cffi:defcfun (disable-raw "disable_raw")
    :void
  (handler :pointer))

(defvar *console-io* *terminal-io*)


(defun put (&rest args)
  "Put raw string on a console"
  (format *console-io* "~{~a~}" args)
  (finish-output *console-io*))

(defun esc (&rest args)
  "Escape sequence"
  (apply #'put #\esc args))

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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +delete+ (code-char #x7f)
    "The DEL character (#\Rubout), last in the ASCII table.")
  (defconstant +escape+ (code-char #x1b)
    "The ESC character (#\esc)."))

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

(defun parse-escape-sequence ()
  (let ((char (read-char-no-hang *console-io*))
        (num1 1)
        (num2 1))
    (flet ((read-num ()
             (loop while (and char (digit-char-p char))
                   collecting char into num
                   do (setf char (read-char-no-hang *console-io*))
                   finally (when num
                             (return (parse-integer (coerce num 'string)))))))
      (setf num1 (or (read-num) 1))
      (when (null char)
        (return-from parse-escape-sequence (values num1 num2 char)))
      (when (char= char #\;)
        (setf char (read-char-no-hang *console-io*)
              num2 (or (read-num) 1)))
      (values num1 num2 char))))

(defun escapep (ch)
  (unless (char= ch +escape+)
    (return-from escapep nil))
  (alexandria:if-let ((next-ch (read-char-no-hang *console-io*)))
    ;; The escape sequence grammar: [\[NO](<num>)(;<num>)[~A-Z].
    (multiple-value-bind (num1 num2 terminator)
        (parse-escape-sequence)
      (resolve-key next-ch num1 num2 terminator))
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


(defmacro with-console (opts &body body)
  (declare (ignore opts))
  (let ((handler (gensym)))
    `(let ((,handler (enable-raw)))
       (unwind-protect (progn ,@body)
         (disable-raw ,handler)))))

(defparameter *conf*
  (list :sleep 1/60
        :cursorp nil
        :foreground '(#xff #xa0 #xa0)
        :background '(#x00 #x22 #x22)))

(declaim (notinline show-screen))
(let ((characters nil))
  (defun show-screen ()
    (loop for ch = (read-input)
          until (null ch)
          do (push ch characters))
    (setf characters (subseq characters 0 (min 12 (length characters))))
    (set-cursor-position (1+ (random 12))
                         (1+ (random 40)))
    (if (zerop (random 2))
        (put "+")
        (put "-"))
    (with-cursor-position (1 44)
      (loop for row from 1
            for ch in characters
            do (set-cursor-position row 44)
               (format *console-io* (format nil "Read: ~s" ch))
               (clear-line 0)))))

(defun start-display ()
  (swank:create-server)
  (with-console ()
    (loop with conf
          with seconds
          do (unless (equalp conf *conf*)
               (setf conf (copy-list *conf*))
               (destructuring-bind (&key sleep cursorp foreground background)
                   conf
                 (setf seconds sleep)
                 (reset-console)
                 (setf (cursor-visibility) cursorp)
                 (apply #'set-background-color background)
                 (apply #'set-foreground-color foreground)
                 (clear-console)))
             (sleep seconds)
             (show-screen))))
