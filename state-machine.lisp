
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :fiveam)
  )


(in-package :cl-user)

(defpackage #:cl-state-machine
  (:use :common-lisp)
  (:export
   :trigger
   :state-definition :list-of-state-definitions
   :initial
   :current
   :state-machine))

(defpackage #:cl-state-machine-test
  (:use :common-lisp :it.bese.FiveAM :cl-state-machine))



(in-package :cl-state-machine)

(defgeneric trigger (a-state-machine event &rest args)
  (:documentation
   "Trigger the `event' on given `a-state-machine' an instance of `state-machine'.

Any argument can be passed as `args' to the `a-state-machine` and will
be passed to its' callbacks TODO:"))

;; TODO: type-spec
;; TODO: docstr

(defun always-t (&rest args)
  (declare (ignore args))
  t)

(defclass state-definition () ((event
                                :initarg :event
                                :reader event)
                               (name
                                :initarg :state
                                :reader name)
                               (description
                                :initarg :description
                                :initform nil
                                :reader description)
                               (requirement
                                :initarg :requirement
                                :reader requirement)
                               (guard
                                :initarg :guard
                                :initform #'always-t
                                :accessor guard)))

(defun list-of-state-definitions? (a-list)
  (and (listp a-list) ; allow an empty list
       (every #'(lambda (v)
                  (subtypep (type-of v) 'state-definition))
              a-list)))

(deftype list-of-state-definitions ()
  `(satisfies list-of-state-definitions?))

(defclass state-machine () ((initial
                             :initarg :initial
                             :initform nil
                             :reader initial)
                            (before-hooks
                             :initarg :before-hooks
                             :initform '()
                             :accessor before-hooks)
                            (after-hooks
                             :initarg :after-hooks
                             :initform '()
                             :accessor after-hooks)
                            (start-hooks
                             :initarg :start-hooks
                             :initform '()
                             :accessor start-hooks)
                            (terminated-hooks
                             :initarg :terminated-hooks
                             :initform '()
                             :accessor terminated-hooks)
                            (definitions
                             :initarg :definitions
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


(defun current (a-state-machine)
  (declare (ignore a-state-machine))
  ;; TODO: type-spec
  nil) ;; TODO

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

(defun state-definition-by-name (a-state-machine state-name)
  (declare (ignore a-state-machine state-name))
  nil) ;; TODO

;; TODO: map/and?
;; TODO: map/list?







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



;;; EOF
