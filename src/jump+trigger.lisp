(in-package :cl-state-machine)


(defun jump! (a-state-machine state)
  "Set `current-state' of `a-state-machine' without hook
function evaluation and constraints check.

Evaluate as a symbol denotes new `state'.

Signal `state-machine-error' when specified `state' is cannot be found in
`a-state-machine'."
  (declare (type state-machine a-state-machine)
           (type symbol state))
  (with-slots (current-state %state-definition-by-state) a-state-machine
    (multiple-value-bind (state-def present?) (gethash state %state-definition-by-state)
      (declare (ignore state-def))
      (unless present? (error 'state-machine-error
                              :format-string "Cannot `jump!' to (~a) (Undefined state)"
                              :format-arguments (list state)))
      (setf current-state state))))


(defun trigger! (a-state-machine event &rest args)
   "Trigger the `event' on given `a-state-machine'.

Evaluation values are: `(values A-STATE-SYMBOL REJECTED? REJECTION-REASON)'

* on Success:
  - `A-STATE-SYMBOL' is a symbol of corresponding state definition
     of the new state.
  - and `REJECTED?', `REJECTION-REASON' both is `nil'.

* if `a-state-machine' has terminated or the specified `event'
  cannot be triggered from current state:
  - `A-STATE-SYMBOL' is nil.
  - `REJECTED?' is `:CANNOT-BE-TRIGGERED'
    and `REJECTION-REASON' is the specified `event' parameter.

* if `a-state-machine' is in illegal state:
  - will signal `state-machine-error'.

* and any hook function could reject the transition. (read following paragraphs)

Rest arguments `args' will be passed to the `before-hooks' and `after-hooks'
registered in `a-state-machine' and its' corresponding `state-definition''s.

The hook functions will be evaluated on state transition.
The evaluation order of hook functions are:

  (1) global-before
  (2) per-state-before
  (3) per-state-after
  (4) global-after

Any global-before or per-state-before hook function could reject the
transition by invoking `reject-transition!'. In this case, any
subsequent hook function evaluation will be stopped and the function's
evaluated values are:

  - `A-STATE-SYMBOL' is `nil',
  - `REJECTED?' is one of
    `:STATE-MACHINE-BEFORE-HOOK-REJECTED' or
    `:STATE-DEFINITION-BEFORE-HOOK-REJECTED'.
  - `REJECTION-REASON' is a cons cell of `(DATUM . REJECTED-HOOk-FUNCTION-VALUE)'
    where `DATUM' is the value the hook function passed as `:datum' key parameter to
    `reject-transition!'.
"
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
                                (error 'state-machine-error
                                       :format-string
                                       "`transition-definition' cannot be found by state/event (~a, ~a) in `state-machine' (~a)"
                                       :format-arguments (list cur-state event a-state-machine))))
         (next-state (to-state transition-def))
         ;; find `state-definition'
         (state-def (find-state-definition-by-state
                     a-state-machine next-state))
         (state-def-nil? (unless state-def
                           (error 'state-machine-error
                                  :format-string
                                  "`state-definition' for state (~a) in `state-machine' (~a) cannot be found"
                                  :format-arguments (list next-state a-state-machine))))
         ;; build `state-transition'
         (a-state-transition (make-state-transition :state-machine a-state-machine
                                                    :transition-definition transition-def
                                                    :args args)))
    (declare (ignore transition-def-nil? state-def-nil?))
    ;; check `before-hooks' of `a-state-machine'
    (let ((state-machine-before-hooks-result
            (call-before-hooks* (before-hooks a-state-machine)
                                a-state-transition)))
      (when state-machine-before-hooks-result
        (return-from trigger! (values nil
                                      :state-machine-before-hook-rejected
                                      state-machine-before-hooks-result))))
    ;; check `before-hooks' of found `state-definition'
    (let ((state-def-before-hooks-result
            (call-before-hooks* (before-hooks state-def)
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
