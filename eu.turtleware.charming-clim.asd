(defpackage #:charming-clim-system
  (:use #:asdf #:cl)
  (:export #:cfile))
(in-package #:charming-clim-system)

(defclass cfile (c-source-file) ())

(defmethod output-files ((o compile-op) (c cfile))
  (list (make-pathname :name (component-name c) :type "so")))

(defmethod perform ((o compile-op) (c cfile))
  (let ((in  (first (input-files o c)))
        (out (first (output-files o c))))
    (uiop:run-program (format nil "cc -shared ~a -o ~a" in out))))

(defmethod perform ((o load-op) (c cfile))
  (let ((in (first (input-files o c))))
    (uiop:call-function "cffi:load-foreign-library" in)))

(defmethod operation-done-p ((o compile-op) (c cfile))
  (let ((in  (first (input-files o c)))
        (out (first (output-files o c))))
    (and (probe-file in)
         (probe-file out)
         (> (file-write-date out) (file-write-date in)))))

(defsystem "eu.turtleware.charming-clim"
  :defsystem-depends-on (#:cffi)
  :depends-on (#:cffi #:swank)
  :components ((:static-file "tutorial.org")
               (:cfile "raw-mode")
               (:file "terminal")))
