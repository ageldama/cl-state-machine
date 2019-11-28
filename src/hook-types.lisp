(in-package :cl-state-machine)


(declaim (type structure-object state-transition))

(deftype before-hook-function ()
  `(function (state-transition &rest t) null))

(deftype after-hook-function ()
  `(function (state-transition &rest t) null))


(declaim (ftype before-hook-function always-nil))
(defun always-nil (a-state-transition &rest args)
  (declare (ignore a-state-transition args))
  nil)


