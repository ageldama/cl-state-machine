
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
   :event
   :description
   :terminal
   :requirement
   :before-hooks
   :after-hooks
   :state-definition-list
   :state-definition-list?

   ;; `state-machine'
   :current-state
   :before-hooks
   :after-hooks
   :state-machine

   :all-states-and-events
   :find-state-definition-by-state
   :find-state-definition-by-event
   :can?
   :possible-events
   :terminated?
   :trigger

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

(defmacro adjoinf (place item)
  `(setf ,place (adjoin ,item ,place)))


(declaim (ftype before-hook-function always-t))
(defun always-t (a-state-transition &rest args)
  (declare (ignore a-state-transition args))
  t)

(defclass state-definition ()
  ((event
    :initarg :event
    :type symbol
    :documentation "Event name that triggers a transition to the state. (Optional)"
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
    list includes the `state' of previous state. Can be an empty list
    and it is by default."
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
  ((current-state
    :initarg :current-state
    :initform nil
    :type symbol
    :documentation "Current state of the `state-machine', can be `nil'."
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

(defun all-states-and-events (a-state-machine)
  "Return every state and event symbol of `a-state-machine'
as `(values list-of-states list-of-events)'"
  (declare (type state-machine a-state-machine))
  (let ((states '())
        (events '()))
    (with-slots (state-definitions) a-state-machine
      (loop for a-state-def in state-definitions
            do (progn (adjoinf states (state a-state-def))
                      (adjoinf events (event a-state-def)))))
    (values states events)))

(defun find-state-definition-by-state (a-state-machine state)
  "Find a matching `state-definition' by given `state'-symbol. `nil'
if it cannot be found."
  (declare (type state-machine a-state-machine)
           (type symbol state))
  (with-slots (state-definitions) a-state-machine
    (loop for state-def in state-definitions
          if (eq (state state-def) state)
            do (return-from find-state-definition-by-state state-def))
    ;; No result
    nil))

(defun find-state-definition-by-event (a-state-machine event)
  "Find a matching `state-definition' by given `event'-symbol. `nil'
if it cannot be found."
  (declare (type state-machine a-state-machine)
           (type symbol event))
  (with-slots (state-definitions) a-state-machine
    (loop for state-def in state-definitions
          if (eq (event state-def) event)
            do (return-from find-state-definition-by-event state-def))
    ;; No result
    nil))


(defun can? (a-state-machine event)
  (declare (ignore a-state-machine event))
  ;; TODO: type-spec
  nil) ;; TODO

(defun terminated? (a-state-machine)
  (declare (type state-machine a-state-machine))
  (let* ((cur (current-state a-state-machine))
         (cur-state-def (find-state-definition-by-state a-state-machine cur)))
    (assert (not (null cur)))
    (assert (not (null cur-state-def)))
    (terminal cur-state-def)))

;; TODO: test
(defun possible-events (a-state-machine)
  "Find all possible `event'-symbols with current state of
`a-state-machine'. Return a list of symbols and it can be empty if
there's no other possible event or the state machine has terminated."
  (declare (type state-machine a-state-machine))
  (when (terminated? a-state-machine)
    (return-from possible-events '()))
  (let ((events '())
        (cur (current-state a-state-machine)))
    (with-slots (state-definitions) a-state-machine
      (loop for state-def in state-definitions
            if (member cur (requirement state-def))
              do (adjoinf events (event state-def))))
    events))


;; TODO: make it as a function, no need to be a generic-function
(defgeneric trigger (a-state-machine event &rest args)
  (:documentation
   "Trigger the `event' on given `a-state-machine' an instance of `state-machine'.

Any argument can be passed as `args' to the `a-state-machine` and will
be passed to its' callbacks TODO:"))


;; TODO: map/and?
;; TODO: map/list?



;; TODO: initialize :after -- ensure no dup states/events in state-definitions


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


(defun state-machine-example-01 ()
  (make-instance 'state-machine
                 :current-state :at-home
                 :state-definitions
                 `(,(make-instance 'state-definition
                                   :event :go-to-work
                                   :state :at-work
                                   :requirement '(:at-home))
                   ,(make-instance 'state-definition
                                   :event :go-home
                                   :state :at-home
                                   :requirement '(:at-work))
                   ,(make-instance 'state-definition
                                   :event :go-to-sleep
                                   :state :in-bed
                                   :requirement '(:at-home))
                   ,(make-instance 'state-definition
                                   :event :wake-up
                                   :state :at-home
                                   :requirement '(:in-bed))
                   ,(make-instance 'state-definition
                                   :event :meditate
                                   :state :nirvana
                                   :requirement '(:at-home)
                                   :terminal t)
                   ,(make-instance 'state-definition
                                   :event :make-big-money
                                   :state :being-rich
                                   :requirement '(:at-work)
                                   :terminal t))))

(defun state-machine-example--started ()
  (make-instance 'state-machine
                 :current-state :a
                 :state-definitions
                 `(,(make-instance 'state-definition :state :a)
                   ,(make-instance 'state-definition
                     :state :b :event :go-b
                     :requirement '(:a)
                     :terminal t))))

(defun state-machine-example--terminated ()
  (make-instance 'state-machine
                 :current-state :b
                 :state-definitions
                 `(,(make-instance 'state-definition :state :a)
                   ,(make-instance 'state-definition
                     :state :b :event :go-b
                     :requirement '(:a)
                     :terminal t))))

(test all-states-and-events
  (multiple-value-bind (states events)
      (cl-state-machine:all-states-and-events (state-machine-example-01))
    (loop for state in '(:at-work :at-home :in-bed :nirvana :being-rich)
          do (is-true (member state states)))
    (loop for event in '(:go-to-work :go-home :go-to-sleep :wake-up :meditate :make-big-money)
          do (is-true (member event events)))))

(test find-state-definition-by-state
  (let ((a-state-machine (state-machine-example-01)))
    (is-false (null (find-state-definition-by-state a-state-machine :nirvana)))
    (is-true (null (find-state-definition-by-state a-state-machine :at-rome)))))

(test find-state-definition-by-event
  (let ((a-state-machine (state-machine-example-01)))
    (is-false (null (find-state-definition-by-event a-state-machine :meditate)))
    (is-true (null (find-state-definition-by-event a-state-machine :go-to-rome)))))

(test terminated?
  (is-false (terminated? (state-machine-example--started)))
  (is-true (terminated? (state-machine-example--terminated))))

(defun equal-set (a b)
  (and (zerop (length (set-difference a b)))
       (zerop (length (set-difference b a)))))

(test possible-events
  (is-true (equal-set '(:go-to-work :go-to-sleep :meditate)
                      (possible-events (state-machine-example-01)))))

(test state-machine-accessors
  ;; should not be accessible
  (multiple-value-bind (sym kind)
      (ensure-symbol :state-definitions :cl-state-machine)
    (declare (ignore sym))
    (is (eq :internal kind))))


;;; EOF
