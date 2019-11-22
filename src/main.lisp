(in-package :cl-state-machine)



(defmacro predicate-list-of (allow-empty? type a-list)
  `(and (locally
            #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
            (if ,allow-empty? (listp ,a-list)
                (and (listp ,a-list)
                     (not (null ,a-list)))))
        (every #'(lambda (v) (typep v ,type))
               ,a-list)))

(defun function-list? (a-list)
  (predicate-list-of t 'function a-list))

(deftype function-list ()
  `(satisfies function-list?))

(defun symbol-list? (a-list)
  (predicate-list-of t 'symbol a-list))

(deftype symbol-list ()
  `(satisfies symbol-list?))


(declaim (type structure-object state-transition))

(deftype non-nil-symbol ()
  `(and symbol
        (not null)))

(deftype before-hook-function ()
  `(function (state-transition &rest t) boolean))

(deftype after-hook-function ()
  `(function (state-transition &rest t) null))

(defmacro adjoinf (place item)
  `(setf ,place (adjoin ,item ,place)))


(declaim (ftype before-hook-function always-t))
(defun always-t (a-state-transition &rest args)
  (declare (ignore a-state-transition args))
  t)

(defclass state-definition ()
  ((state
    :initarg :state :type non-nil-symbol :reader state
    :documentation "Name of the state.")
   (description
    :initarg :description :initform nil :type string :reader description
    :documentation "Description string. (Optional)")
   (terminal
    :initarg :terminal :initform nil :type boolean :reader terminal
    :documentation "Mark it as terminal state.")
   (before-hooks
    :initarg :before-hooks :initform (list #'always-t) :type function-list :accessor before-hooks
    :documentation "List of `before-hook-function's.

 Will be evaluated sequentially before trainsition of the state of
    `state-machine' to this `state-definition', and each hook function
    should return a boolean value.

When a hook function evaluated as false value, will reject the state
    transition and stop the evaluation of rest hook functions.")
   (after-hooks
    :initarg :after-hooks :initform '() :type function-list :accessor after-hooks
    :documentation "List of `after-hook-function'.

Will be evaluated when the state of `state-machine' has change to this
    `state-definition'.")))

(defmethod print-object ((obj state-definition) out)
  (print-unreadable-object (obj out :type t)
    (format out "state=~a terminal=~a description=~a"
            (state obj)
            (terminal obj)
            (description obj))))

(defun state-definition-list? (a-list)
  (predicate-list-of t 'state-definition a-list))

(deftype state-definition-list ()
  `(satisfies state-definition-list?))


(defclass transition-definition ()
  ((from-state :initarg :from :reader from-state :type symbol)
   (to-state :initarg :to :reader to-state :type symbol)
   (description
    :initarg :description :initform nil :type string :reader description
    :documentation "Description string. (Optional)")
   (event :initarg :event :reader event :type symbol
          :documentation "Event name that triggers a transition to the
          state. (Optional)" ))
  (:documentation "Represent a transition between one state to another
  state."))

(defmethod print-object ((obj transition-definition) out)
  (print-unreadable-object (obj out :type t)
    (format out "event=~a from-state=~a to-state=~a description=~a"
            (event obj)
            (from-state obj)
            (to-state obj)
            (description obj))))

(defun transition-definition-list? (a-list)
  (predicate-list-of t 'transition-definition a-list))

(deftype transition-definition-list ()
  `(satisfies transition-definition-list?))


(defclass state-machine ()
  ((current-state
    :initarg :current-state :initform nil :type symbol :reader current-state
    :documentation "Current state of the `state-machine', can be `nil'.")
   (before-hooks
    :initarg :before-hooks :initform '() :type function-list :accessor before-hooks
    :documentation "List of `before-hook-function's.

Will be evaluated on every state transition of this `state-machine'
    sequentially.

 When a hook function evaluated as false, reject the
    state transition and stop the evaluation of rest hook functions.")
   (after-hooks
    :initarg :after-hooks :initform '() :type function-list :accessor after-hooks
    :documentation "List of `after-hook-function's.

Will be evaluated on every state transition of this `state-machine'.")
   (state-definitions
    :initarg :state-definitions :initform '()
    :type state-definition-list :reader state-machine--state-definitions
    :documentation "List of `state-definition's.")
   (transition-definitions
    :initarg :transition-definitions :initform '()
    :type transition-definition-list :reader state-machine--transition-definitions
    :documentation "List of `transition-definition's.")
   (%state-definition-by-state :initform (make-hash-table))
   (%possible-events-by-state :initform (make-hash-table))
   (%transition-definitions-by-state-event-tuple :initform (make-hash-table :test #'equal))))

(defmethod print-object ((obj state-machine) out)
  (with-slots (state-definitions transition-definitions) obj
    (print-unreadable-object (obj out :type t)
      (format out "current-state=~a"
              (current-state obj)))))


(defstruct state-transition
  "Represent a state transition context.
Will be passed to state transition hook functions.

The name of triggering event is `event' and `args' is every arguments
has passed to `trigger'.

`from-state-name' and `to-state-name' are the same as `event' slot's
value of each `state-definition'."
  (state-machine nil :type state-machine :read-only t)
  (transition-definition nil :type transition-definition :read-only t)
  (args nil :type t :read-only t))

(defun find-state-definition-by-state (a-state-machine state)
  "Find a matching `state-definition' by given `state'-symbol.

Evaluated as `nil' if it cannot be found."
  (declare (type state-machine a-state-machine)
           (type symbol state))
  (with-slots (%state-definition-by-state) a-state-machine
    (multiple-value-bind (val present?) (gethash state %state-definition-by-state)
      ;; No need to check `present?', When cannot be found, `val' is `nil' anyway.
      ;; SEE CLHS: `gethash'
      (declare (ignore present?))
      val)))

(defun terminated? (a-state-machine)
  "True if `a-state-machine' has reached to a `state-definition' which
marked as `terminal' = true.

Can signal `simple-error' on `a-state-machine' with illegal current
state."
  (declare (type state-machine a-state-machine))
  (let* ((cur (current-state a-state-machine))
         (cur-state-def (find-state-definition-by-state a-state-machine cur)))
    (assert (not (null cur)))
    (assert (not (null cur-state-def)))
    (terminal cur-state-def)))

(defun possible-events (a-state-machine)
  "Find all possible `event'-symbols with current state of
`a-state-machine'.

Return a list of symbols and it can be empty if there's no other
possible event or the state machine has terminated.

Signal `simple-error' on `a-state-machine' of illegal current state."
  (declare (type state-machine a-state-machine))
  (when (terminated? a-state-machine)
    (return-from possible-events '()))
  (with-slots (%possible-events-by-state) a-state-machine
    (multiple-value-bind (val present?) (gethash (current-state a-state-machine)
                                                 %possible-events-by-state)
      (if present? val
          '()))))

(defun can? (a-state-machine event)
  "Evaluate as true if `a-state-machine' can be `trigger!'-ed to
`event'. If `a-state-machine` has been terminated, it will be
evaluated as false as well.

Signal `simple-error' on `a-state-machine' of illegal current state."
  (declare (type state-machine a-state-machine)
           (type symbol event))
  (member event (possible-events a-state-machine)))

(defun find-transition-definition-by-state-and-event (a-state-machine state event)
  "Can be evaluated as nil if there's no matching
`transition-definition."
  (with-slots (%transition-definitions-by-state-event-tuple) a-state-machine
    (let ((state-event (cons state event)))
      (gethash state-event %transition-definitions-by-state-event-tuple))))


(defun jump! (a-state-machine state)
  "Set `current-state' of `a-state-machine' without invoking hook
functions and any constraints check.

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

Evaluation values are: `(values A-STATE-DEFINITION REJECTED? REJECTION-REASON)'

On success, `A-STATE-DEFINITION' is corresponding `state-definition'
of triggered state and `REJECTED?', `REJECTION-REASON' are `nil'.

If `state-machine' has terminated or the specified `event' cannot be
triggered on current state, `REJECTED?' is `:CANNOT-BE-TRIGGERED' and
`REJECTION-REASON' is the specified `event' parameter.

If `state-machine' in illegal current state, will signal
`simple-error'.

Rest arguments `args' will be passed to the `before-hooks' and
`after-hooks' in `a-state-machine' and its' corresponding
`state-definition''s as well.

The hook functions will be evaluated when the before and the after of
state transition. The order of hook functions evaluation is:
global-before -> state-before -> state-after -> global-after.

If any function of `before-hooks' in `a-state-machine' or the
corresponding `state-definition' has evaluated as false value, the
transition will be rejected, `A-STATE-DEFINITION' is `nil',
`REJECTED?' is one of `:STATE-MACHINE-BEFORE-HOOK-REJECTED' or
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
    state-def))

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




(defun gethash-list-append-item (key ht item)
  (setf (gethash key ht)
        (append (gethash key ht '()) (list item))))

(defmethod initialize-instance :after ((a-state-machine state-machine) &rest args)
  (declare (ignore args))
  (with-slots (state-definitions transition-definitions
               %state-definition-by-state
               %possible-events-by-state
               %transition-definitions-by-state-event-tuple) a-state-machine
    ;; fill state-definitions/state lookup table
    (loop :for state-def :in state-definitions
          :for state-name := (state state-def)
          :do (setf (gethash state-name %state-definition-by-state) state-def))
    (let ((collected-count (hash-table-count %state-definition-by-state))
          (state-def-count (length state-definitions)))
      (when (/= collected-count state-def-count)
        ;; ensure no dups in states
        (error "Collected `state'-count (~a) does not match with length of `state-definitions' (~a)"
               collected-count state-def-count)))
    (loop :for transition-def :in transition-definitions
          :for event-name := (event transition-def)
          :for state-name := (from-state transition-def)
          :for state-event-tuple := (cons state-name event-name)
          :do (progn
                ;; fill possible-events/state lookup table
                (gethash-list-append-item
                 state-name %possible-events-by-state event-name)
                ;; fill transition-definitions / state-event tuple
                ;; lookup table
                (if (gethash state-event-tuple
                             %transition-definitions-by-state-event-tuple)
                    (error "Duplicated state-event combination (~a)"
                           state-event-tuple)
                    ;; else, OK
                    (setf (gethash state-event-tuple
                                   %transition-definitions-by-state-event-tuple)
                          transition-def))))))

(defmacro state-definitions-of (&rest state-definition-args-list)
  "Turn lists of initargs for `(make-instance 'state-definition)` into
list of `state-definition' instances"
  (let ((i# (gensym)))
    `(loop :for ,i# :in (list ,@state-definition-args-list)
           :collect (apply #'make-instance 'state-definition ,i#))))

(defmacro transition-definitions-of (&rest transition-definition-args-list)
  (let ((i# (gensym)))
    `(loop :for ,i# :in (list ,@transition-definition-args-list)
           :collect (apply #'make-instance 'transition-definition ,i#))))

(defmacro state-machine-of (state-machine-args
                            state-definition-args-list
                            transition-definition-args-list)
  (let ((state-defs# (gensym))
        (transition-defs# (gensym)))
    `(let ((,state-defs# (state-definitions-of ,@state-definition-args-list))
           (,transition-defs# (transition-definitions-of ,@transition-definition-args-list)))
       (apply #'make-instance 'state-machine
              (append ,state-machine-args
                      `(:state-definitions ,,state-defs#)
                      `(:transition-definitions ,,transition-defs#))))))

