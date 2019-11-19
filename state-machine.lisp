
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
   :trigger
   :start-state-machine!
   ;; TODO: more functions
   :before-hook-function
   :after-hook-function

   ;; `state-definition'
   :state-definition
   :state
   :event
   :description
   :terminal
   :requirement
   :before-hooks
   :after-hooks
   :state-definition-list
   :state-definition-list?

   ;; `state-machine'
   :initial-state
   :current-state
   :before-hooks
   :after-hooks
   :state-machine

   ;; `state-transition'
   :state-transition
   :make-state-transition
   :state-transition-args
   :state-transition-event
   :state-transition-from-state-name
   :state-transition-to-state-name
   :state-transition-state-machine
   :state-transition-p
   ))

(defpackage #:cl-state-machine-test
  (:use :common-lisp :it.bese.FiveAM :alexandria :cl-state-machine))



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

;; TODO: make it as a function, no need to be a generic-function
(defgeneric trigger (a-state-machine event &rest args)
  (:documentation
   "Trigger the `event' on given `a-state-machine' an instance of `state-machine'.

Any argument can be passed as `args' to the `a-state-machine` and will
be passed to its' callbacks TODO:"))


(declaim (ftype before-hook-function always-t))
(defun always-t (a-state-transition &rest args)
  (declare (ignore a-state-transition args))
  t)

(defclass state-definition ()
  ((event
    :initarg :event
    :type non-nil-symbol
    :documentation "Event name that triggers a transition to the state."
    :reader event)
   (state
    :initarg :state
    :type non-nil-symbol
    :documentation "Name of the state."
    :reader state)
   (description
    :initarg :description
    :initform nil
    :type string
    :documentation "Description string. (Optional)"
    :reader description)
   (terminal
    :initarg :terminal
    :initform nil
    :type boolean
     :documentation "Mark it as terminal state."
    :reader terminal)
   (requirement
    :initarg :requirement
    :initform '()
    :type symbol-list
    :documentation "List of symbols. Only can be transitioned this
    list includes the `state' of previous state."
    :reader requirement)
   (before-hooks
    :initarg :before-hooks
    :initform (list #'always-t)
    :type function-list
    :documentation "List of `before-hook-function's. All will be
    evaluated sequentially before change the state, and each should
    return a boolean value. If there's any false evaluated function in
    the middle of sequential evaluation, The transition will be
    rejected. Will not evaluated subsequent hook functions when it
    evaluated to a `nil'."
    :accessor before-hooks)
   (after-hooks
    :initarg :after-hooks
    :initform '()
    :type function-list
    :documentation "List of `after-hook-function'. All will be
    evaluated when the state has change to this state."
    :accessor after-hooks)))

(defun state-definition-list? (a-list)
  (predicate-list-of t 'state-definition a-list))

(deftype state-definition-list ()
  `(satisfies state-definition-list?))


(defclass state-machine ()
  ((initial-state
    :initarg :initial-state
    :initform nil
    :type symbol
    :documentation "Starting point of this `state-machine'"
    :reader initial-state)
   (current-state
    :initform nil
    :type symbol
    :documentation "Current state of the `state-machine', can be `nil'"
    :reader current-state)
   (before-hooks
    :initarg :before-hooks
    :initform '()
    :type function-list
    :documentation "TODO"
    :accessor before-hooks)
   (after-hooks
    :initarg :after-hooks
    :initform '()
    :type function-list
    :documentation "List of `after-hook-function's. Will be evaluated
    the state has changed."
    :accessor after-hooks)
   (state-definitions
    :initarg :state-definitions
    :initform '()
    :documentation "List of `state-definition's. Will be evaluated
    before the state transition. Can reject the transition if any of
    hook function evaluated to `nil' and will stop evaluating the
    subsequent hook functions."
    :type state-definition-list)))


(defstruct state-transition
  "Represent a state transition context.
Will be passed to state transition hook functions.

The name of triggering event is `event' and `args' is every arguments
has passed to `trigger'.

`from-state-name' and `to-state-name' are the same as `event' slot's
value of each `state-definition'."

  (event nil :type symbol :read-only t)
  (state-machine nil :type state-machine :read-only t)
  (from-state-name nil :type symbol :read-only t)
  (to-state-name nil :type symbol :read-only t)
  (args nil :type t :read-only t))

(defun can? (a-state-machine event)
  (declare (ignore a-state-machine event))
  ;; TODO: type-spec
  nil) ;; TODO

(defun avail-states (a-state-machine)
  (declare (ignore a-state-machine))
  ;; TODO: type-spec
  nil) ;; TODO

(defun avail-events (a-state-machine)
  (declare (ignore a-state-machine))
  ;; TODO: type-spec
  nil) ;; TODO

(defun possible-events (a-state-machine)
  (declare (ignore a-state-machine))
  ;; TODO: type-sepc
  nil) ;; TODO

(defun state-machine-started? (a-state-machine)
  (declare (ignore a-state-machine))
  nil) ;; TODO:

(defun start-state-machine! (a-state-machine)
  (declare (ignore a-state-machine))
  nil) ;; TODO

(defun state-definition-by-state (a-state-machine state)
  (declare (ignore a-state-machine state))
  nil) ;; TODO

;; TODO: map/and?
;; TODO: map/list?


;; TODO: state-machine builder DSL




(in-package :cl-state-machine-test)

;;; Do it! (fiveam:run!)

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



(defun state-machine-example-01 ()
  (make-instance 'state-machine
                 :initial-state :in-bed
                 :state-definitions
                 `(,(make-instance 'state-definition
                                   :event :go-to-work
                                   :state :at-work
                                   :requirement :at-home)
                   ,(make-instance 'state-definition
                                   :event :go-home
                                   :state :at-home
                                   :requirement :at-work)
                   ,(make-instance 'state-definition
                                   :event :go-to-sleep
                                   :state :in-bed
                                   :requirement :at-home)
                   ,(make-instance 'state-definition
                                   :event :wake-up
                                   :state :at-home
                                   :requirement :in-bed)
                   ,(make-instance 'state-definition
                                   :event :meditate
                                   :state :nirvana
                                   :requirement :at-home
                                   :terminal t)
                   ,(make-instance 'state-definition
                                   :event :make-big-money
                                   :state :being-rich
                                   :requirement :at-work
                                   :terminal t))))

(test state-machine-accessors
  ;; should not be accessible
  (multiple-value-bind (sym kind)
      (ensure-symbol :state-definitions :cl-state-machine)
    (declare (ignore sym))
    (is (eq :internal kind))))


;;; EOF
