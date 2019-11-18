
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :fiveam)
  )


(in-package :cl-user)

(defpackage #:cl-state-machine
  (:nicknames :statem)
  (:use :common-lisp)
  (:export

   ;; functions
   :trigger

   ;; state-definition
   :state-definition
   :state
   :event
   :description
   :terminal
   :requirement
   :before-hooks
   :after-hooks
   :list-of-state-definitions
   :list-of-state-definitions?

   ;; state-machine
   :initial-state
   :current-state
   :before-hooks
   :after-hooks
   :state-machine

   ;; state-transition
   :state-transition
   ;; TODO: struct funcs, accessors
   ;; TODO: more functions
   ))

(defpackage #:cl-state-machine-test
  (:use :common-lisp :it.bese.FiveAM :cl-state-machine))



(in-package :cl-state-machine)

(defgeneric trigger (a-state-machine event &rest args)
  (:documentation
   "Trigger the `event' on given `a-state-machine' an instance of `state-machine'.

Any argument can be passed as `args' to the `a-state-machine` and will
be passed to its' callbacks TODO:"))

(declaim (type structure-object state-transition))

;; TODO: type-spec
;; TODO: docstr

(declaim (ftype (function (&rest t) boolean) always-t))
(defun always-t (&rest args)
  (declare (ignore args))
  t)

(defclass state-definition ()
  ((event
    :initarg :event
    :reader event)
   (state
    :initarg :state
    :reader state)
   (description
    :initarg :description
    :initform nil
    :reader description)
   (terminal
    :initarg :terminal
    :initform nil
    :type boolean
    :reader terminal)
   (requirement
    :initarg :requirement
    :reader requirement)
   (before-hooks
    :initarg :before-hooks
    :initform (list #'always-t)
    :accessor before-hooks)
   (after-hooks
    :initarg :after-hooks
    :initform '()
    :accessor after-hooks)))

(defun list-of-state-definitions? (a-list)
  (and (listp a-list) ; allow an empty list
       (every #'(lambda (v)
                  (subtypep (type-of v) 'state-definition))
              a-list)))

(deftype list-of-state-definitions ()
  `(satisfies list-of-state-definitions?))

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
    :type list-of-state-definitions)))


(defstruct state-transition
  "Represent a state transition context.
Will be passed to state transition hook functions.

The name of triggering event is `event' and `args' is every arguments
has passed to `trigger'.

`from-state-name' and `to-state-name' are the same as `event' slot's
value of each `state-definition'."

  (event nil :type symbol :read-only t)
  (a-state-machine nil :type state-machine :read-only t)
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

(defun restart-state-machine (a-state-machine)
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

(test check-type-list-of-state-definitions
  (is (null (let ((x '()))
              (check-type x list-of-state-definitions))))
  (is (null (let ((x (list (make-instance 'state-definition))))
              (check-type x list-of-state-definitions))))
  (signals simple-type-error
    (let ((x "foobar"))
      (check-type x list-of-state-definitions)))
  (signals simple-type-error
    (let ((x '(1 2 3)))
      (check-type x list-of-state-definitions))))

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

(test state-machine-accesors
  (let ((sm (state-machine-example-01)))
    (signals undefined-function ;; no accessor
      (locally
          #+sbcl (declare (sb-ext:muffle-conditions cl:style-warning))
          (state-definitions sm)))
  ))



;;; EOF
