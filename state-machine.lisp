
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :fiveam)
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
  (:use :common-lisp :it.bese.FiveAM :cl-state-machine))



(in-package :cl-state-machine)


(declaim (type structure-object state-transition))


(deftype non-nil-symbol ()
  `(and symbol
        (not null)))

(deftype before-hook-function ()
  `(function (state-transition &rest t) boolean))

(deftype after-hook-function ()
  `(function (state-transition &rest t) null))

(deftype before-hook-function-list ()
  `(list before-hook-function))

(deftype after-hook-function-list ()
  `(list after-hook-function))

(defgeneric trigger (a-state-machine event &rest args)
  (:documentation
   "Trigger the `event' on given `a-state-machine' an instance of `state-machine'.

Any argument can be passed as `args' to the `a-state-machine` and will
be passed to its' callbacks TODO:"))

;; TODO: type-spec
;; TODO: docstr

(declaim (ftype before-hook-function always-t))
(defun always-t (a-state-transition &rest args)
  (declare (ignore a-state-transition args))
  t)

(defclass state-definition ()
  ;; TODO: docstr
  ((event
    :initarg :event
    :type non-nil-symbol
    :reader event)
   (state
    :initarg :state
    :type non-nil-symbol
    :reader state)
   (description
    :initarg :description
    :initform nil
    :type string
    ;; TODO :documentation
    :reader description)
   (terminal
    :initarg :terminal
    :initform nil
    :type boolean
    ;; TODO :documentation
    :reader terminal)
   (requirement
    :initarg :requirement
    :type non-nil-symbol
    ;; TODO :documentation
    :reader requirement)
   (before-hooks
    :initarg :before-hooks
    :initform (list #'always-t)
    :type before-hook-function-list
    ;; TODO :documentation
    :accessor before-hooks)
   (after-hooks
    :initarg :after-hooks
    :initform '()
    :type after-hook-function-list
    ;; TODO :documentation
    :accessor after-hooks)))

(defun state-definition-list? (a-list)
  (and (listp a-list) ; allow an empty list
       (every #'(lambda (v)
                  (subtypep (type-of v) 'state-definition))
              a-list)))

(deftype state-definition-list ()
  `(satisfies state-definition-list?))


(defclass state-machine ()
  ((initial-state
    :initarg :initial-state
    :initform nil
    :reader initial-state)
   (current-state
    :initform nil
    :reader current-state)
   (before-hooks
    :initarg :before-hooks
    :initform '()
    :accessor before-hooks)
   (after-hooks
    :initarg :after-hooks
    :initform '()
    :accessor after-hooks)
   (state-definitions
    :initarg :state-definitions
    :initform '()
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

(test check-type--state-definition-list
  (is-true (typep '() 'state-definition-list))
  (is-true (typep (list (make-instance 'state-definition))
                         'state-definition-list))
  (is-false (typep "foobar" 'state-definition-list))
  (is-false (typep '(1 2 3) 'state-definition-list)))

(test check-type--non-nil-symbol
  (is-true (typep :foo 'non-nil-symbol))
  (is-true (typep 'foo 'non-nil-symbol))
  (is-false (typep 42 'non-nil-symbol))
  (is-false (typep nil 'non-nil-symbol)))



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
  (let ((sm (state-machine-example-01)))
    (signals undefined-function ;; no accessor
      (locally
          #+sbcl (declare (sb-ext:muffle-conditions cl:style-warning))
          (state-definitions sm)))))



;;; EOF
