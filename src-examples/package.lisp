(in-package :cl-user)

(defpackage #:cl-state-machine-examples
  (:use :common-lisp))

(defpackage #:cl-state-machine-examples/tamagochi
  (:use :common-lisp :cl-state-machine :alexandria)
  (:export
   :run
   :run+quit))

(defpackage #:cl-state-machine-examples/graphviz
  (:use :common-lisp :cl-state-machine :cl-state-machine-graphing)
  (:export
   :write-dot-file-example))
