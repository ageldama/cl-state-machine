(in-package :cl-state-machine)


(defun jump! (a-state-machine state)
  "Set `current-state' of `a-state-machine' without invoking hook
functions and any constraints check.

Evaluate as a symbol denotes new `state'.

Signal `simple-error' when specified `state' is cannot be found in
`a-state-machine'."
  (declare (type state-machine a-state-machine)
           (type symbol state))
  (with-slots (current-state %state-definition-by-state) a-state-machine
    (multiple-value-bind (state-def present?) (gethash state %state-definition-by-state)
      (declare (ignore state-def))
      (unless present? (error "Cannot `jump!' to (~a) (Undefined state)" state))
      (setf current-state state))))


(defun trigger! (a-state-machine event &rest args)
   "Trigger the `event' on given `a-state-machine'.

Evaluation values are: `(values A-STATE-SYMBOL REJECTED? REJECTION-REASON)'

On success, `A-STATE-SYMBOL' is a symbol of corresponding state
definition of new state and `REJECTED?', `REJECTION-REASON' are `nil'.

If `a-state-machine' has terminated or the specified `event' cannot be
triggered on current state, `REJECTED?' is `:CANNOT-BE-TRIGGERED' and
`REJECTION-REASON' is the specified `event' parameter. And
`A-STATE-SYMBOL' is `nil'.

If `a-state-machine' in illegal current state, will signal
`simple-error'.

Rest arguments `args' will be passed to the `before-hooks' and
`after-hooks' in `a-state-machine' and its' corresponding
`state-definition''s as well.

The hook functions will be evaluated when the before and the after of
state transition. The order of hook functions evaluation is:
global-before -> state-before -> state-after -> global-after.

If any function of `before-hooks' in `a-state-machine' or the
corresponding `state-definition' has evaluated as false value, the
transition will be rejected, `A-STATE-SYMBOL' is `nil', `REJECTED?' is
one of `:STATE-MACHINE-BEFORE-HOOK-REJECTED' or
`:STATE-DEFINITION-BEFORE-HOOK-REJECTED', and `REJECTION-REASON' is
the hook function value that evaluated as false.

And such rejection will suppress the consequent evaluation of
`before-hooks' and `after-hooks' in `a-state-machine' and the
corresponding `state-definition' as well."
  (declare (type state-machine a-state-machine)
           (type symbol event))
  (unless (can? a-state-machine event)
    (return-from trigger! (values nil :cannot-be-triggered event)))
  (let* ((cur-state (current-state a-state-machine))
         ;; find `transition-definition'
         (transition-def
           (find-transition-definition-by-state-and-event a-state-machine
                                                          cur-state
                                                          event))
         (transition-def-nil? (unless transition-def
                                (error "`transition-definition' cannot be found by state/event (~a, ~a) in `state-machine' (~a)"
                                       cur-state event a-state-machine)))
         (next-state (to-state transition-def))
         ;; find `state-definition'
         (state-def (find-state-definition-by-state
                     a-state-machine next-state))
         (state-def-nil? (unless state-def
                           (error "`state-definition' for state (~a) in `state-machine' (~a) cannot be found"
                                  next-state a-state-machine)))
         ;; build `state-transition'
         (a-state-transition (make-state-transition :state-machine a-state-machine
                                                    :transition-definition transition-def
                                                    :args args)))
    (declare (ignore transition-def-nil? state-def-nil?))
    ;; check `before-hooks' of `a-state-machine'
    (let ((state-machine-before-hooks-result
            (call-before-hooks (before-hooks a-state-machine)
                               a-state-transition)))
      (when state-machine-before-hooks-result
        (return-from trigger! (values nil
                                      :state-machine-before-hook-rejected
                                      state-machine-before-hooks-result))))
    ;; check `before-hooks' of found `state-definition'
    (let ((state-def-before-hooks-result
            (call-before-hooks (before-hooks state-def)
                               a-state-transition)))
      (when state-def-before-hooks-result
        (return-from trigger! (values nil
                                      :state-definition-before-hook-rejected
                                      state-def-before-hooks-result))))
    ;; `jump!'
    (jump! a-state-machine next-state)
    ;; and `call-after-hooks's
    (call-after-hooks (after-hooks state-def) a-state-transition)
    (call-after-hooks (after-hooks a-state-machine) a-state-transition)
    ;; return
    (values next-state nil nil)))
