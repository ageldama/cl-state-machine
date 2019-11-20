
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

   :state-definitions-of

   ;; `state-machine'
   :current-state
   :before-hooks
   :after-hooks
   :state-machine
   :state-machine--state-definitions

   :find-state-definition-by-state
   :can?
   :possible-events
   :terminated?
   :trigger!
   :jump!

   :state-machine-of

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
    :initarg :event :type symbol :reader event
    :documentation "Event name that triggers a transition to the
    state. (Optional)")
   (state
    :initarg :state :type non-nil-symbol :reader state
    :documentation "Name of the state.")
   (description
    :initarg :description :initform nil :type string :reader description
    :documentation "Description string. (Optional)")
   (terminal
    :initarg :terminal :initform nil :type boolean :reader terminal
    :documentation "Mark it as terminal state.")
   (requirement
    :initarg :requirement :initform '() :type symbol-list :reader requirement
    :documentation "List of symbols. Only can be transitioned this
    list includes the `state' of previous state. Can be an empty list
    and it is by default.")
   (before-hooks
    :initarg :before-hooks :initform (list #'always-t) :type function-list :accessor before-hooks
    :documentation "List of `before-hook-function's. Will be evaluated
    sequentially before trainsition of the state of `state-machine' to
    this `state-definition', and each hook function should return a
    boolean value. When a hook function evaluated as false, reject the
    state transition and stop the evaluation of rest hook functions.")
   (after-hooks
    :initarg :after-hooks :initform '() :type function-list :accessor after-hooks
    :documentation "List of `after-hook-function'. Will be evaluated
    when the state of `state-machine' has change to this
    `state-definition'.")))

(defun state-definition-list? (a-list)
  (predicate-list-of t 'state-definition a-list))

(deftype state-definition-list ()
  `(satisfies state-definition-list?))


(defclass state-machine ()
  ((current-state
    :initarg :current-state :initform nil :type state-definition :reader current-state
    :documentation "Current state of the `state-machine', can be `nil'.")
   (before-hooks
    :initarg :before-hooks :initform '() :type function-list :accessor before-hooks
    :documentation "List of `before-hook-function's. Will be evaluated
    on every state transition of this `state-machine'
    sequentially. When a hook function evaluated as false, reject the
    state transition and stop the evaluation of rest hook functions.")
   (after-hooks
    :initarg :after-hooks :initform '() :type function-list :accessor after-hooks
    :documentation "List of `after-hook-function's. Will be evaluated
    on every state transition of this `state-machine'.")
   (state-definitions
    :initarg :state-definitions :initform '()
    :type state-definition-list :reader state-machine--state-definitions
    :documentation "List of `state-definition's. Will be evaluated
    before the state transition. Can reject the transition if any of
    hook function evaluated to `nil' and will stop evaluating the
    subsequent hook functions.")
   (%state-definition-by-state :initform (make-hash-table))))


(defstruct state-transition
  "Represent a state transition context.
Will be passed to state transition hook functions.

The name of triggering event is `event' and `args' is every arguments
has passed to `trigger'.

`from-state-name' and `to-state-name' are the same as `event' slot's
value of each `state-definition'."
  (event nil :type symbol :read-only t)
  (state-machine nil :type state-machine :read-only t)
  (from-state-name nil :type state-definition :read-only t)
  (to-state-name nil :type state-definition :read-only t)
  (args nil :type t :read-only t))

(defun find-state-definition-by-state (a-state-machine state)
  "Find a matching `state-definition' by given `state'-symbol. `nil'
if it cannot be found."
  (declare (type state-machine a-state-machine)
           (type symbol state))
  (with-slots (%state-definition-by-state) a-state-machine
    (multiple-value-bind (val present?) (gethash state %state-definition-by-state)
      ;; No need to check `present?', When cannot be found, `val' is `nil' anyway.
      ;; SEE CLHS: `gethash'
      (declare (ignore present?))
      val)))

(defun terminated? (a-state-machine)
  (declare (type state-machine a-state-machine))
  (let* ((cur (current-state a-state-machine))
         (cur-state-def (find-state-definition-by-state a-state-machine cur)))
    (assert (not (null cur)))
    (assert (not (null cur-state-def)))
    (terminal cur-state-def)))

;; TODO faster
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
      (loop :for state-def :in state-definitions
            :if (member cur (requirement state-def))
              :do (adjoinf events (event state-def))))
    events))

(defun can? (a-state-machine event)
  (declare (type state-machine a-state-machine)
           (type symbol event))
  (member event (possible-events a-state-machine)))


;; TODO: make it as a function, no need to be a generic-function
(defgeneric trigger! (a-state-machine event &rest args)
  (:documentation
   "Trigger the `event' on given `a-state-machine' an instance of `state-machine'.

Any argument can be passed as `args' to the `a-state-machine` and will
be passed to its' callbacks TODO:"))

(defun jump! (a-state-machine event)
  "TODO"
  (declare (type state-machine a-state-machine)
           (type symbol event)
           (ignore a-state-machine event))
  nil)

;; TODO: call-before-hooks

;; TODO: call-after-hooks

(defmethod initialize-instance :after ((a-state-machine state-machine) &rest args)
  (declare (ignore args))
  (with-slots (state-definitions %state-definition-by-state) a-state-machine
    (loop :for state-def :in state-definitions
          :for state-name := (state state-def)
          :do (setf (gethash state-name %state-definition-by-state) state-def))
    (let ((collected-count (hash-table-count %state-definition-by-state))
          (state-def-count (length state-definitions)))
      (when (/= collected-count state-def-count)
        ;; ensure no dups in states
        (error (format nil "Collected `state'-count (~a) does not match with length of `state-definitions' (~a)"
                       collected-count state-def-count))))))

(defmacro state-definitions-of (&rest state-definition-args-list)
  "Turn lists of initargs for `(make-instance 'state-definition)` into
list of `state-definition' instances"
  (let ((i# (gensym)))
    `(loop :for ,i# :in (list ,@state-definition-args-list)
           :collect (apply #'make-instance 'state-definition ,i#))))

(defmacro state-machine-of (state-machine-args &rest state-definition-args-list)
  (let ((state-defs# (gensym)))
    `(let ((,state-defs# (state-definitions-of ,@state-definition-args-list)))
       (apply #'make-instance 'state-machine
              (append ,state-machine-args `(:state-definitions ,,state-defs#))))))





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


(test state-definitions-of
  (let* ((state-defs (state-definitions-of
                      '(:state :a :event :go-to-a)
                      '(:state :b :event :go-to-b)))
         (sd-1 (first state-defs))
         (sd-2 (second state-defs)))
    (is (eq :a (state sd-1)))
    (is (eq :go-to-a (event sd-1)))
    (is (eq :b (state sd-2)))
    (is (eq :go-to-b (event sd-2)))))

(test state-machine-of
  (let ((state-m (state-machine-of
                  '(:current-state :a)
                  '(:state :a :event :go-to-a)
                  '(:state :b :event :go-to-b))))
    (is (eq :a (current-state state-m)))
    (let ((sd-1 (first (state-machine--state-definitions state-m)))
          (sd-2 (second (state-machine--state-definitions state-m))))
      (is (eq :a (state sd-1)))
      (is (eq :go-to-a (event sd-1)))
      (is (eq :b (state sd-2)))
      (is (eq :go-to-b (event sd-2))))))


(defun state-machine-example-01 ()
  (state-machine-of '(:current-state :at-home)
                    '(:event :go-to-work :state :at-work
                      :requirement (:at-home))
                    '(:event :go-home :state :at-home
                      :requirement (:at-work))
                    '(:event :go-to-sleep :state :in-bed
                      :requirement (:at-home))
                    '(:event :wake-up :state :at-home
                      :requirement (:in-bed))
                    '(:event :meditate :state :nirvana
                      :requirement (:at-home) :terminal t)
                    '(:event :make-big-money :state :being-rich
                      :requirement (:at-work) :terminal t)))

(defun state-machine-example--started ()
  (state-machine-of '(:current-state :a)
                    '(:state :a)
                    '(:state :b :event :go-to-b
                      :requirement (:a) :terminal t)))

(defun state-machine-example--terminated ()
  (state-machine-of '(:current-state :b)
                    '(:state :a)
                    '(:state :b :event :go-to-b
                      :requirement '(:a) :terminal t)))

#| FIXME
(test find-state-definition-by-state
  (let ((a-state-machine (state-machine-example-01)))
    (is-false (null (find-state-definition-by-state a-state-machine :nirvana)))
    (is-true (null (find-state-definition-by-state a-state-machine :at-rome)))))
|#

#| FIXME
(test terminated?
  (is-false (terminated? (state-machine-example--started)))
  (is-true (terminated? (state-machine-example--terminated))))
|#

(defun equal-set (a b)
  (and (zerop (length (set-difference a b)))
       (zerop (length (set-difference b a)))))

#| FIXME
(test possible-events
  (is-true (equal-set '(:go-to-work :go-to-sleep :meditate)
                      (possible-events (state-machine-example-01))))
  (is-true (equal-set '(:go-to-b) (possible-events (state-machine-example--started))))
  (is-true (equal-set '() (possible-events (state-machine-example--terminated)))))
|#

#| FIXME
(test can?
  (let ((sm (state-machine-example-01)))
    (is-true (can? sm :meditate))
    (is-true (can? sm :go-to-sleep))
    (is-true (can? sm :go-to-work))
    (is-false (can? sm :go-home))))
|#


(test state-machine-accessors
  ;; should not be accessible
  (multiple-value-bind (sym kind)
      (ensure-symbol :state-definitions :cl-state-machine)
    (declare (ignore sym))
    (is (eq :internal kind))))


;; TODO: cl-state-machine-graphviz

;;; EOF
