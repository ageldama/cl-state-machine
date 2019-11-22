(in-package :cl-state-machine)


(declaim (type structure-object state-transition))

(deftype before-hook-function ()
  `(function (state-transition &rest t) boolean))

(deftype after-hook-function ()
  `(function (state-transition &rest t) null))


(declaim (ftype before-hook-function always-t))
(defun always-t (a-state-transition &rest args)
  (declare (ignore a-state-transition args))
  t)


