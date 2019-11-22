(in-package :cl-user)

(defpackage #:cl-state-machine-graphing
  (:nicknames :statem-graphing)
  (:use :common-lisp :cl-state-machine)
  (:export :write-dot :write-dot-file))
