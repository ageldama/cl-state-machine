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


(defparameter *trigger!-clear-history* t
  "Whether `trigger!' evaluates `empty-trigger-history' on its'
  starting. (Default: `T')")

(defmacro %with-trigger!-params (a-state-machine event args
                                 (sym-cur-state sym-transition-def sym-state-def sym-next-state sym-state-transition)
                                 &rest body)
  (let ((transition-def-nil?# (gensym))
        (state-def-nil?# (gensym)))
    `(let* ((,sym-cur-state (current-state ,a-state-machine))
            ;; find `transition-definition'
            (,sym-transition-def
              (find-transition-definition-by-state-and-event ,a-state-machine
                                                             ,sym-cur-state
                                                             ,event))
            (,transition-def-nil?# (unless ,sym-transition-def
                                     (error 'state-machine-error
                                            :format-string
                                            "`transition-definition' cannot be found by state/event (~a, ~a) in `state-machine' (~a)"
                                            :format-arguments (list ,sym-cur-state ,event ,a-state-machine))))
            (,sym-next-state (to-state ,sym-transition-def))
            ;; find `state-definition'
            (,sym-state-def (find-state-definition-by-state
                             ,a-state-machine ,sym-next-state))
            (,state-def-nil?# (unless ,sym-state-def
                                (error 'state-machine-error
                                       :format-string
                                       "`state-definition' for state (~a) in `state-machine' (~a) cannot be found"
                                       :format-arguments (list ,sym-next-state ,a-state-machine))))
            ;; build `state-transition'
            (,sym-state-transition (make-state-transition :state-machine ,a-state-machine
                                                          :transition-definition ,sym-transition-def
                                                          :args ,args)))
     (declare (ignore ,transition-def-nil?# ,state-def-nil?#))
     ,@body)))

(defun trigger! (a-state-machine event &rest args)
  "Trigger the `event' on given `a-state-machine'.

Evaluation values are: `(values NEW-STATE-SYMBOL REJECTED-BY REJECTION-REASON)'

* on Success:
  - `NEW-STATE-SYMBOL' is a symbol of corresponding state definition
     of the new state.
  - and `REJECTED-BY', `REJECTION-REASON' both is `nil'.

* if `a-state-machine' has terminated or the specified `event'
  cannot be triggered from current state:
  - `NEW-STATE-SYMBOL' is nil.
  - `REJECTED-BY' is `:CANNOT-BE-TRIGGERED'
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
  (3) per-transition-before
  (4) per-transition-after
  (5) per-state-after
  (6) global-after

Any global-before or per-state-before hook function could reject the
transition by invoking `reject-transition!'. In this case, any
subsequent hook function evaluation will be stopped and the function's
evaluated values are:

  - `NEW-STATE-SYMBOL' is `nil',
  - `REJECTED-BY' is could be one of
    `:STATE-MACHINE-BEFORE-HOOK-REJECTED' or
    `:STATE-DEFINITION-BEFORE-HOOK-REJECTED' or
    `:TRANSITION-DEFINITION-BEFORE-HOOK-REJECTED'.
  - `REJECTION-REASON' is a cons cell of `(DATUM . REJECTED-HOOk-FUNCTION-VALUE)'
    where `DATUM' is the value the hook function passed as `:datum' key parameter to
    `reject-transition!'.
"
  (declare (type state-machine a-state-machine)
           (type symbol event))
  ;; ..where macros are:
  (macrolet ((%return-trigger! (new-state rejected-by rejection-reason)
               `(return-from trigger!
                  (values ,new-state ,rejected-by ,rejection-reason)))
             (%return-trigger!-ok (new-state)
               `(%return-trigger! ,new-state nil nil))
             (%log-history+return-trigger!-fail
                 (a-state-machine event args rejected-by rejection-reason)
               `(progn (append-trigger-history* :state-machine ,a-state-machine
                                                :event ,event :args ,args
                                                :rejected-by ,rejected-by
                                                :rejection-reason ,rejection-reason)
                       (%return-trigger! nil ,rejected-by ,rejection-reason)))
             (%check-before-hooks (a-state-machine event args
                                   hooks a-state-transition rejected-by)
               (let ((result# (gensym)))
                 `(let ((,result#
                          (call-before-hooks* ,hooks ,a-state-transition)))
                    (when ,result#
                      (%log-history+return-trigger!-fail ,a-state-machine ,event ,args
                                                         ,rejected-by ,result#))))))
    ;; preconditions
    (unless (can? a-state-machine event)
      (%log-history+return-trigger!-fail a-state-machine event args
                                         :cannot-be-triggered event))
    ;; clear previous history?
    (when *trigger!-clear-history* (empty-trigger-history))
    ;; actual body
    (%with-trigger!-params a-state-machine event args
        (cur-stgate transition-def state-def next-state a-state-transition)
        ;;
        (%check-before-hooks a-state-machine event args
                             (before-hooks a-state-machine)
                             a-state-transition
                             :state-machine-before-hook-rejected)
        (%check-before-hooks a-state-machine event args
                             (before-hooks state-def)
                             a-state-transition
                             :state-definition-before-hook-rejected)
        (%check-before-hooks a-state-machine event args
                             (before-hooks transition-def)
                             a-state-transition
                             :transition-definition-before-hook-rejected)
        ;;
        (jump! a-state-machine next-state)
        ;;
        (call-after-hooks (after-hooks transition-def) a-state-transition)
        (call-after-hooks (after-hooks state-def) a-state-transition)
        (call-after-hooks (after-hooks a-state-machine) a-state-transition)
        ;;
        (append-trigger-history* :state-machine a-state-machine :event event :args args
                                 :new-state (current-state a-state-machine))
        (let ((next-schedule (pop-next-scheduled-trigger)))
          (if next-schedule
              ;; then, tail recursive call
              (let ((*trigger!-clear-history* nil))
                (apply #'trigger!
                       (append (list a-state-machine
                                     (trigger-schedule-entry-event next-schedule))
                               (trigger-schedule-entry-args next-schedule))))
              ;; else: just return.
              (%return-trigger!-ok (current-state a-state-machine)))))))
