
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :fiveam) ;; test
  (ql:quickload :alexandria) ;; test
  )


(in-package :cl-user)

(defpackage #:cl-state-machine
  (:nicknames :statem)
  (:use :common-lisp)
  (:export

   ;; global
   :non-nil-symbol
   :before-hook-function
   :after-hook-function

   ;; `state-definition'
   :state-definition

   :state
   :description
   :terminal
   :before-hooks
   :after-hooks

   :state-definition-list
   :state-definition-list?

   :state-definitions-of

   :call-before-hooks
   :call-after-hooks

   ;; `transition-definition'
   :transition-definition

   :from-state
   :to-state
   :event
   :description

   :transition-definition-list
   :transition-definition-list?

   :transition-definitions-of

   ;; `state-machine'
   :current-state
   :before-hooks
   :after-hooks
   :state-machine
   :state-machine--state-definitions
   :state-machine--transition-definitions

   :find-state-definition-by-state
   :find-transition-definition-by-state-and-event
   :can?
   :possible-events
   :terminated?
   :trigger!
   :jump!

   :state-machine-of

   ;; `state-transition'
   :state-transition
   :make-state-transition
   :state-transition-p

   :state-transition-args
   :state-transition-transition-definition
   :state-transition-state-machine
   ))

(defpackage #:cl-state-machine-test
  (:use :common-lisp :it.bese.FiveAM :alexandria :cl-state-machine)
  (:export :test-suite))

;; TODO: defpackage cl-state-machine-graphviz



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

TODO: Can signal `simple-error' on `a-state-machine' with illegal
current state."
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

TODO: Signal `simple-error' on terminated `a-state-machine'"
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
`event'. Signal `simple-error' on terminated `a-state-machine' TODO"
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

TODO Signal `simple-error' when specified `state' is cannot be found in
`a-state-machine'."
  (declare (type state-machine a-state-machine)
           (type symbol state))
  (with-slots (current-state %state-definition-by-state) a-state-machine
    (multiple-value-bind (state-def present?) (gethash state %state-definition-by-state)
      (declare (ignore state-def))
      (unless present? (error "Cannot `jump!' to (~a) (Undefined state)" state))       ;; TODO
      (setf current-state state))))


(defun trigger! (a-state-machine event &rest args)
   "Trigger the `event' on given `a-state-machine' an instance of
`state-machine' and evaluated as matching `state-definition'.

TODO return-vals -- terminated? can? --> TODO errors

TODO global-before -> state-before -> state-after -> global-after

Any argument can be passed as `args' to the `before-hooks' and
`after-hooks' in `a-state-machine' and its' matching
`state-definition'.

If any function of `before-hooks' in `a-state-machine' or the matching
`state-definition' evaluated as false value, the transition will be
rejected and signal `simple-error'. ;; TODO

And such rejection will suppress the consequent evaluation of
`before-hooks' and `after-hooks' in `a-state-machine' and the matching
`state-definition' as well."
  (declare (type state-machine a-state-machine)
           (type symbol event))
  (when (terminated? a-state-machine)
    (return-from trigger! nil))
  (unless (can? a-state-machine event)
    (return-from trigger! nil))
  (let* ((cur-state (current-state a-state-machine))
         ;; find `transition-definition'
         (transition-def
           (find-transition-definition-by-state-and-event a-state-machine
                                                          cur-state
                                                          event))
         (transition-def-nil? (unless transition-def
                                (error "`transition-definition' cannot be found by state/event (~a, ~a) in `state-machine' (~a)" ;; TODO
                                       cur-state event a-state-machine)))
         (next-state (to-state transition-def))
         ;; find `state-definition'
         (state-def (find-state-definition-by-state
                     a-state-machine next-state))
         (state-def-nil? (unless state-def
                           (error "`state-definition' for state (~a) in `state-machine' (~a)" ; TODO
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
        (error "`before-hooks' -- rejected by a hook function (~a) in `state-machine' (~a)" ; TODO
             state-machine-before-hooks-result a-state-machine)))
    ;; check `before-hooks' of found `state-definition'
    (let ((state-def-before-hooks-result
            (call-before-hooks (before-hooks state-def)
                               a-state-transition)))
      (when state-def-before-hooks-result
        (error "`before-hooks' -- reject by a hook function (~a) in `state-definition' (~a)" ; TODO
               state-def-before-hooks-result state-def)))
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
        (error "Collected `state'-count (~a) does not match with length of `state-definitions' (~a)" ; TODO
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
                         (error "Duplicated state-event combination (~a)" ; TODO
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




(in-package :cl-state-machine-test)

(def-suite test-suite)

(def-suite skip-suite)

(in-suite test-suite)

;;; Do it! (fiveam:run! 'cl-state-machine-test:test-suite)

(test typep--state-definition-list
  (is-true (typep '() 'state-definition-list))
  (is-true (typep (list (make-instance 'state-definition))
                  'state-definition-list))
  (is-false (typep "foobar" 'state-definition-list))
  (is-false (typep '(1 2 3) 'state-definition-list)))

(test typep--non-nil-symbol
  (is-true (typep :foo 'non-nil-symbol))
  (is-true (typep 'foo 'non-nil-symbol))
  (is-false (typep 42 'non-nil-symbol))
  (is-false (typep nil 'non-nil-symbol)))

(test typep--function-list
  (is-true (typep '() 'cl-state-machine::function-list))
  (is-true (typep (list #'identity) 'cl-state-machine::function-list))
  (is-false (typep (list 123) 'cl-state-machine::function-list))
  (is-false (typep "foobar" 'cl-state-machine::function-list)))

(test typep--symbol-list
  (is-true (typep '() 'cl-state-machine::symbol-list))
  (is-true (typep (list 'foo) 'cl-state-machine::symbol-list))
  (is-false (typep (list 123) 'cl-state-machine::symbol-list))
  (is-false (typep "foobar" 'cl-state-machine::symbol-list)))

(test adjoinf
  (let ((s '()))
    (cl-state-machine::adjoinf s :a)
    (cl-state-machine::adjoinf s :a)
    (is-false (zerop (length s)))
    (is (= 1 (length s)))
    (cl-state-machine::adjoinf s :a)
    (cl-state-machine::adjoinf s :b)
    (is (= 2 (length s)))
    (is-true (and (member :a s)
                  (member :b s)))))


(test state-definitions-of
  (let* ((state-defs (state-definitions-of
                      '(:state :a)
                      '(:state :b)))
         (sd-1 (first state-defs))
         (sd-2 (second state-defs)))
    (is (eq :a (state sd-1)))
    (is (eq :b (state sd-2)))))

(test transition-definitions-of
  (let* ((transition-defs (transition-definitions-of
                           '(:from :a :to :b :event :a->b)
                           '(:from :b :to :a :event :b->a)))
         (td-1 (first transition-defs))
         (td-2 (second transition-defs)))
    (is (and (eq :a (from-state td-1))
             (eq :b (to-state td-1))
             (eq :a->b (event td-1))))
    (is (and (eq :b (from-state td-2))
             (eq :a (to-state td-2))
             (eq :b->a (event td-2))))))


(test state-machine-of
  (let ((state-m (state-machine-of
                  '(:current-state :a)
                  (`(:state :a)
                    `(:state :b))
                  (`(:from :a :to :b :event :a->b)))))
    (is (eq :a (current-state state-m)))
    (let ((sd-1 (first (state-machine--state-definitions state-m)))
          (sd-2 (second (state-machine--state-definitions state-m)))
          (td-1 (first (state-machine--transition-definitions state-m))))
      (is (eq :a (state sd-1)))
      (is (eq :b (state sd-2)))
      (is (and (eq :a (from-state td-1))
               (eq :b (to-state td-1))
               (eq :a->b (event td-1)))))))

(defun state-machine-example-01 (&key (global-before-hooks '())
                                   (global-after-hooks '())
                                   (state-before-hooks '())
                                   (state-after-hooks '()))
  (state-machine-of `(:current-state :at-home
                      :before-hooks ,global-before-hooks
                      :after-hooks ,global-after-hooks)
                    (`(:state :at-work
                       :before-hooks ,state-before-hooks
                       :after-hooks ,state-after-hooks)
                      `(:state :at-home
                        :before-hooks ,state-before-hooks
                        :after-hooks ,state-after-hooks)
                      `(:state :in-bed
                        :before-hooks ,state-before-hooks
                        :after-hooks ,state-after-hooks)
                      `(:state :nirvana :terminal t
                        :before-hooks ,state-before-hooks
                        :after-hooks ,state-after-hooks)
                      `(:state :being-rich :terminal t
                        :before-hooks ,state-before-hooks
                        :after-hooks ,state-after-hooks))
                    (`(:from :at-home :to :at-work
                       :event :home->work)
                      `(:from :at-home :to :in-bed
                        :event :home->bed)
                      `(:from :at-home :to :nirvana
                        :event :meditate)
                      `(:from :at-work :to :at-home
                        :event :work->home)
                      `(:from :at-work :to :being-rich
                        :event :show-me-the-money))))

(defun state-machine-example--started ()
  (state-machine-of '(:current-state :a)
                    (`(:state :a)
                      `(:state :b :terminal t))
                    (`(:from :a :to :b :event :a->b))))

(defun state-machine-example--terminated ()
  (state-machine-of '(:current-state :b)
                    (`(:state :a)
                      `(:state :b :terminal t))
                    (`(:from :a :to :b :event :a->b))))

(defun state-machine-example--wrong-current ()
  (state-machine-of '(:current-state :x)
                    (`(:state :a)
                      `(:state :b :terminal t))
                    (`(:from :a :to :b :event :a->b))))


(test find-state-definition-by-state
  (let ((a-state-machine (state-machine-example-01)))
    (is-false (null (find-state-definition-by-state a-state-machine :nirvana)))
    (is-true (null (find-state-definition-by-state a-state-machine :at-rome)))))

(test terminated?
  (is-false (terminated? (state-machine-example--started)))
  (is-true (terminated? (state-machine-example--terminated)))
  (signals simple-error (terminated? (state-machine-example--wrong-current))))

(test find-transition-definition-by-state-and-event
  (let ((sm (state-machine-example-01)))
    (let ((td (find-transition-definition-by-state-and-event sm :at-home :home->work)))
      (is-true (not (null td)))
      (is (and (eq (event td) :home->work)
               (eq (from-state td) :at-home)
               (eq (to-state td) :at-work))))
    (let ((td (find-transition-definition-by-state-and-event sm :at-work :show-me-the-money)))
      (is-true (not (null td)))
      (is (and (eq (event td) :show-me-the-money)
               (eq (from-state td) :at-work)
               (eq (to-state td) :being-rich))))
    ;; let's not do this
    (let ((td (find-transition-definition-by-state-and-event sm :at-kino :fick)))
      (is-true  (null td)))))

(test gethash-list-append-item
  (let ((ht (make-hash-table)))
    (cl-state-machine::gethash-list-append-item :a ht 'a)
    (cl-state-machine::gethash-list-append-item :a ht 'b)
    (cl-state-machine::gethash-list-append-item :b ht 'x)
    ;;
    (is (= 2 (hash-table-count ht)))
    (is (and (equal '(a b) (gethash :a ht))
             (equal '(x) (gethash :b ht))))))

(test possible-events
  (is-true (set-equal '(:home->work :home->bed :meditate)
                      (possible-events (state-machine-example-01))))
  (is-true (set-equal '(:a->b) (possible-events (state-machine-example--started))))
  (is-true (set-equal '() (possible-events (state-machine-example--terminated))))
  (signals simple-error (possible-events (state-machine-example--wrong-current))))

(test can?
  (let ((sm (state-machine-example-01)))
    (is-true (can? sm :meditate))
    (is-true (can? sm :home->bed))
    (is-true (can? sm :home->bed))
    (is-false (can? sm :work->home))))

(test can?-2
  (let ((sm (state-machine-example--terminated)))
    (is-false (can? sm :a->b))
    (is-false (can? sm :b->a))
    (is-false (can? sm :meditate))
    (is-false (can? sm :work->home))))

(test can?-3
  (signals simple-error (can? (state-machine-example--wrong-current) :sth)))

(test state-machine-accessors
  ;; should not be accessible
  (multiple-value-bind (sym kind)
      (ensure-symbol :state-definitions :cl-state-machine)
    (declare (ignore sym))
    (is (eq :internal kind))))

(defmacro append-item-f (a-list-place item)
  `(setf ,a-list-place (append ,a-list-place (list ,item))))

(test (append-item-f-really? :suite skip-suite)
  (let ((l '()))
    (append-item-f l :a)
    (is (equal '(:a) l))))

(defstruct state-transition-record state-transition id
           (:documentation "For testing purpose. Hold arguments passed
           to hook function. Use `id' slot to identify a hook
           function."))

(defmacro make-state-transition-record-appender (list-place a-state-transition-record-id retval)
  "Make new hook function evaluates as `retval'.

Append new `state-transition-record' at the end of `list-place'.

Specify `id' slot as an identifier of new hook function. It will be
matched with `id'-slot of corresponding `state-transition-record'."
  (let ((args# (gensym))
        (item# (gensym)))
    `#'(lambda (a-state-transition &rest rest-args)
         (declare (ignore rest-args))
         (let* ((,args# (state-transition-args a-state-transition))
                (,item# (make-state-transition-record
                         :state-transition a-state-transition
                         :id ,a-state-transition-record-id)))
           (append-item-f ,list-place ,item#))
         ,retval)))

(defmacro with-state-transition-recorder ((records-name appender-maker-name) &rest body)
  "Will evaluate `body' within an established bindings of
`records-name' and `appender-maker-name'.

`appender-maker-name' is a function which takes `(id ret-val)' as
parameter and returns new hook function by
`make-state-transition-record-appender'. `id' and `ret-val' is pass
through to `make-state-transition-record-appender'. And `records-name'
will be the first `list-place' parameter of it."
  `(let ((,records-name '()))
     (flet ((,appender-maker-name (id ret-val)
              (make-state-transition-record-appender ,records-name id ret-val)))
       ,@body)))

(test jump!
  (with-state-transition-recorder
      (records make-hook)
      (let ((sm (state-machine-example-01
                 :global-before-hooks (list (make-hook :global-before t))
                 :global-after-hooks (list (make-hook :global-after t))
                 :state-before-hooks (list (make-hook :state-before t))
                 :state-after-hooks (list (make-hook :state-after t)))))
        ;; OK
        (jump! sm :being-rich)
        (is (eq (current-state sm) :being-rich))
        (is (= 0 (length records))) ; should not evaluate any hook
        ;; FAIL
        (signals simple-error (jump! sm :a))
        (is (= 0 (length records)))))) ; should not evaluate any hook

(test call-after-hooks
  (with-state-transition-recorder
      (records make-hook)
      (let* (;; `state-transition'
             (sm (state-machine-example-01))
             (td (find-transition-definition-by-state-and-event sm :at-home :home->work))
             (a-state-transition (make-state-transition :state-machine sm
                                                        :transition-definition td))
             ;; hooks
             (hook-1 (make-hook :1 nil)) ; `after-hook' no care evaluated value
             (hook-2 (make-hook :2 nil))
             (hooks (list hook-1 hook-2)))
        (is-false (call-after-hooks hooks a-state-transition))
        (is (= 2 (length records)))
        (is (equal '(:1 :2) (mapcar #'state-transition-record-id records)))
        (is-true (every #'(lambda (i) (equal (state-transition-record-state-transition i)
                                             a-state-transition))
                        records)))))

(test call-before-hooks
  (with-state-transition-recorder
      (records make-hook)
      (let* (;; `state-transition'
             (sm (state-machine-example-01))
             (td (find-transition-definition-by-state-and-event sm :at-home :home->work))
             (a-state-transition (make-state-transition :state-machine sm
                                                        :transition-definition td))
             ;; hooks
             (hook-1 (make-hook :1 t)) ; evaluted as true
             (hook-2 (make-hook :2 t)) ; same here
             (hooks (list hook-1 hook-2)))
        (is-false (call-before-hooks hooks a-state-transition))
        (is (= 2 (length records))) ; every hooks evaluated!
        (is (equal '(:1 :2) (mapcar #'state-transition-record-id records)))
        (is-true (every #'(lambda (i) (equal (state-transition-record-state-transition i)
                                             a-state-transition))
                        records)))))

(test call-before-hooks-2
  ;; Does `before-hook' evaluates as false reject consequent evaluations?
  (with-state-transition-recorder
      (records make-hook)
      (let* (;; `state-transition'
             (sm (state-machine-example-01))
             (td (find-transition-definition-by-state-and-event sm :at-home :home->work))
             (a-state-transition (make-state-transition :state-machine sm
                                                        :transition-definition td))
             ;; hooks
             (hook-1 (make-hook :1 nil)) ; should stops here
             (hook-2 (make-hook :2 t))   ; never be evaluated
             (hooks (list hook-1 hook-2)))
        (is (eq hook-1 (call-before-hooks hooks a-state-transition)))
        (is (= 1 (length records)))
        (is (equal '(:1) (mapcar #'state-transition-record-id records)))
        (is (equal (state-transition-record-state-transition (first records))
                   a-state-transition)))))


(test trigger!-on-terminated
  (is-false (trigger! (state-machine-example--terminated)  :a->b)))

(test trigger!-invalid-event
  (let ((sm (state-machine-example-01)))
    (is (eq (current-state sm) :at-home))
    (is-false (trigger! sm (gensym)))
    (is (eq (current-state sm) :at-home))))

(test trigger!-ok
  (with-state-transition-recorder
      (records make-hook)
      (let* ((sm (state-machine-example-01
                  :global-before-hooks
                  (list (make-hook :global-before-a t)
                        (make-hook :global-before-b t))
                  :global-after-hooks
                  (list (make-hook :global-after-a t)
                        (make-hook :global-after-b t))
                  :state-before-hooks
                  (list (make-hook :state-before-a t)
                        (make-hook :state-before-b t))
                  :state-after-hooks
                  (list (make-hook :state-after-a t)
                        (make-hook :state-after-b t))))
             (passing-args (gensym)))
        (is (eq (current-state sm) :at-home))
        (let ((state-def (trigger! sm :meditate passing-args)))
          (is (and (eq (current-state sm) :nirvana)
                   (eq (current-state sm) (state state-def)))))
        (is (equal '(:global-before-a :global-before-b
                     :state-before-a :state-before-b
                     :state-after-a :state-after-b
                     :global-after-a :global-after-b)
                   (mapcar #'state-transition-record-id records)))
        (is-true (every #'(lambda (i) (equal (list passing-args)
                                             (state-transition-args
                                              (state-transition-record-state-transition i))))
                        records))
        ;; check `state-transition-transition-definition' of `state-transition-record-state-transition'
        (loop :with expect-transition-event := :meditate
              :with expect-transition-to := :nirvana
              :with expect-transition-from := :at-home
              :for i :in records
              :for a-state-transition := (state-transition-record-state-transition i)
              :for a-transition-definition := (state-transition-transition-definition
                                               a-state-transition)
              :do (is-true (and (eq (event a-transition-definition)
                                    expect-transition-event)
                                (eq (from-state a-transition-definition)
                                    expect-transition-from)
                                (eq (to-state a-transition-definition)
                                    expect-transition-to)))))))

(test trigger!-global-before-hook-rejection
  (with-state-transition-recorder
      (records make-hook)
      (let* ((sm (state-machine-example-01
                  :global-before-hooks
                  (list (make-hook :global-before-a t)
                        (make-hook :global-before-b nil)) ; <-- the guy
                  :global-after-hooks
                  (list (make-hook :global-after-a t))
                  :state-before-hooks
                  (list (make-hook :state-before-a t))
                  :state-after-hooks
                  (list (make-hook :state-after-a t))))
             (passing-args (gensym)))
        (is (eq (current-state sm) :at-home))
        (signals simple-error (trigger! sm :meditate passing-args))
        (is (eq (current-state sm) :at-home)) ; not changed
        (is (equal '(:global-before-a :global-before-b) ; no `global-after', `state-*'
                   (mapcar #'state-transition-record-id records)))
        (is-true (every #'(lambda (i) (equal (list passing-args)
                                             (state-transition-args
                                              (state-transition-record-state-transition i))))
                        records)))))

(test trigger!-state-before-hook-rejection
  (with-state-transition-recorder
      (records make-hook)
      (let* ((sm (state-machine-example-01
                  :global-before-hooks
                  (list (make-hook :global-before-a t)
                        (make-hook :global-before-b t))
                  :global-after-hooks
                  (list (make-hook :global-after-a t))
                  :state-before-hooks
                  (list (make-hook :state-before-a t)
                        (make-hook :state-before-b nil)) ; <-- the guy
                  :state-after-hooks
                  (list (make-hook :state-after-a t))))
             (passing-args (gensym)))
        (is (eq (current-state sm) :at-home))
        (signals simple-error (trigger! sm :meditate passing-args))
        (is (eq (current-state sm) :at-home)) ; not changed
        (is (equal '(:global-before-a :global-before-b :state-before-a :state-before-b)
                   (mapcar #'state-transition-record-id records)))
        (is-true (every #'(lambda (i) (equal (list passing-args)
                                             (state-transition-args
                                              (state-transition-record-state-transition i))))
                        records)))))




;;; EOF
