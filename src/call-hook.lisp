(in-package :cl-state-machine)



(defun call-before-hooks (an-before-hook-function-list a-state-transition)
  "Evaluate functions in `an-before-hook-function-list'
sequentially.

To stop the transition to next state, can use `reject-transition!'
function.  And it will stops subsequent `before-hooks' evaluations.

If any hook function signalled `reject-transition' condition by
evaluating `reject-transition!', this function evaluated as values of
function value of the hook function and the condition value. Unless
evaluated as nil."
  (declare (type list an-before-hook-function-list)
           (type state-transition a-state-transition))
  (let ((cur-hook-fn nil))
    (handler-case
        (loop :for hook :in an-before-hook-function-list
              :do (progn (setf cur-hook-fn hook)
                         (funcall hook a-state-transition)))
      (reject-transition (condition) (return-from call-before-hooks
                                       (values cur-hook-fn condition)))))
  nil)

(defun call-after-hooks (an-after-hook-function-list a-state-transition)
  "Evaluate functions in `an-after-hook-function-list'
sequentially. Return nothing."
  (declare (type list an-after-hook-function-list)
           (type state-transition a-state-transition))
  (loop :for hook :in an-after-hook-function-list
        :do (funcall hook a-state-transition)))


(defun call-before-hooks* (an-before-hook-function-list a-state-transition)
  "Exactly same with `call-before-hooks'.

Except evaluates as a cons cell instead of values, unlike
`call-before-hooks'.

The cons cell looks like: `(HOOK-FUNCTION-VALUE . DATUM)', where
`HOOK-FUNCTION-VALUE' is the function invoked `reject-transition!'
and `DATUM' is the `:datum' key parameter of `reject-transition!', not
`reject-transition' condition itself.

On successful evaluation, no hook function invoked
`reject-transition!', it evaluated as NIL."
  (multiple-value-bind (hook-fn a-condition)
      (call-before-hooks an-before-hook-function-list
                         a-state-transition)
    (if hook-fn
        (with-slots (datum) a-condition
          (cons hook-fn datum))
        ;; else
        nil)))
