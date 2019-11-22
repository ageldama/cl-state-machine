(in-package :cl-state-machine)



(defun call-before-hooks (an-before-hook-function-list a-state-transition)
  "Evaluate functions in `an-before-hook-function-list'
sequentially.

Will be evaluated as nil when successfully evaluated every
function. Each `before-hook-function' in
`an-before-hook-function-list' supposed to be evaluated as true,
otherwise this caller function will stop proceeding of evaluation of
subsequent hook functions and will be evaluated as a function value
that evaluated as false."
  (declare (type list an-before-hook-function-list)
           (type state-transition a-state-transition))
  (loop :for hook :in an-before-hook-function-list
        :for retval := (funcall hook a-state-transition)
        :unless retval
          :do (return-from call-before-hooks hook))
  nil)

(defun call-after-hooks (an-after-hook-function-list a-state-transition)
  "Evaluate functions in `an-after-hook-function-list'
sequentially. Return nothing."
  (declare (type list an-after-hook-function-list)
           (type state-transition a-state-transition))
  (loop :for hook :in an-after-hook-function-list
        :do (funcall hook a-state-transition)))




