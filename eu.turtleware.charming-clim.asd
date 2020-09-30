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
  :description "Terminal manipulation library"
  :author "Daniel 'jackdaniel' Kochmański"
  :license "LGPL-2.1+"
  :defsystem-depends-on (#:cffi)
  :depends-on (#:alexandria #:cffi #:swank)
  :pathname "Sources"
  :components ((:file "packages")
               (:module "l0"
                :depends-on ("packages")
                :components ((:cfile "raw-mode")
                             (:file "terminal" :depends-on ("raw-mode"))))
               (:module "l1"
                :depends-on ("packages" "l0")
                :components ((:file "drawing")
                             (:file "input")
                             (:file "cursor" :depends-on ("input"))
                             (:file "output" :depends-on ("drawing" "cursor"))
                             (:file "surface" :depends-on ("output"))
                             (:file "console" :depends-on ("cursor" "output"))))
               (:module "l2"
                :depends-on ("packages" "l0" "l1")
                :components ((:file "rendering")
                             (:file "display-lists")
                             (:file "frame-manager" :depends-on ("display-lists"))))))

(defsystem "eu.turtleware.charming-clim/examples"
  :license "BSD-2-Clause"
  :depends-on ("eu.turtleware.charming-clim")
  :pathname "Examples"
  :components ((:file "examples")
               (:file "l0-example")
               (:file "l0-input")
               (:file "l1-wool")
               (:file "l1-surfaces")))

(defsystem "eu.turtleware.charming-clim/demo"
  :license "BSD-2-Clause"
  :depends-on ("eu.turtleware.charming-clim")
  :pathname "Auxiliary"
  :components ((:file "example")))
