
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
    :documentation "List of `state-definition's.")
   (transition-definitions
    :initarg :transition-definitions :initform '()
    :type transition-definition-list :reader state-machine--transition-definitions
    :documentation "List of `transition-definition's.")
   (%state-definition-by-state :initform (make-hash-table))
   (%possible-events-by-state :initform (make-hash-table))
   (%transition-definitions-by-state-event-tuple :initform (make-hash-table :test #'equal))))


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

(defun possible-events (a-state-machine)
  "Find all possible `event'-symbols with current state of
`a-state-machine'. Return a list of symbols and it can be empty if
there's no other possible event or the state machine has terminated."
  (declare (type state-machine a-state-machine))
  (when (terminated? a-state-machine)
    (return-from possible-events '()))
  (with-slots (%possible-events-by-state) a-state-machine
    (multiple-value-bind (val present?) (gethash (current-state a-state-machine)
                                                 %possible-events-by-state)
      (if present? val
          '()))))

(defun can? (a-state-machine event)
  (declare (type state-machine a-state-machine)
           (type symbol event))
  (member event (possible-events a-state-machine)))

(defun find-transition-definition-by-state-and-event (a-state-machine state event)
  (with-slots (%transition-definitions-by-state-event-tuple) a-state-machine
    (let ((state-event (cons state event)))
      (gethash state-event %transition-definitions-by-state-event-tuple))))

(defun trigger! (a-state-machine event &rest args)
   "Trigger the `event' on given `a-state-machine' an instance of `state-machine'.

Any argument can be passed as `args' to the `a-state-machine` and will
be passed to its' callbacks TODO:"
  (declare (ignore event args) (type state-machine a-state-machine)
           (type symbol event))
  (when (terminated? a-state-machine)
    (return-from trigger! nil))
  nil)

(defun call-before-hooks (an-before-hook-function-list a-state-transition)
  (declare (type list an-before-hook-function-list)
           (type state-transition a-state-transition))
  (loop :for hook :in an-before-hook-function-list
        :for retval := (funcall hook a-state-transition)
        :unless retval
          :do (return-from call-before-hooks hook))
  nil)
;; TODO docstring
;; TODO test

(defun call-after-hooks (an-after-hook-function-list a-state-transition)
  (declare (type list an-after-hook-function-list)
           (type state-transition a-state-transition))
  (loop :for hook :in an-after-hook-function-list
        :do (funcall hook a-state-transition)))
;; TODO docstring
;; TODO test




(defun jump! (a-state-machine state)
  "Set `current-state' of `a-state-machine' without invoking hook
functions and any constraints check."
  (declare (type state-machine a-state-machine)
           (type symbol state))
  (with-slots (current-state %state-definition-by-state) a-state-machine
    (multiple-value-bind (state-def present?) (gethash state %state-definition-by-state)
      (declare (ignore state-def))
      (unless present? (error (format nil "Cannot `jump!' to (~a) (Undefined state)" state)))
      (setf current-state state))))

(defun gethash-list-append-item (key ht item)
  (setf (gethash key ht)
        (append (gethash key ht '()) (list item))))

(defmethod initialize-instance :after ((a-state-machine state-machine) &rest args)
  (declare (ignore args))
  (with-slots (state-definitions transition-definitions
               %state-definition-by-state
               %possible-events-by-state
               %transition-definitions-by-state-event-tuple) a-state-machine
    (loop :for state-def :in state-definitions
          :for state-name := (state state-def)
          :do (setf (gethash state-name %state-definition-by-state) state-def))
    (let ((collected-count (hash-table-count %state-definition-by-state))
          (state-def-count (length state-definitions)))
      (when (/= collected-count state-def-count)
        ;; ensure no dups in states
        (error (format nil
                       "Collected `state'-count (~a) does not match with length of `state-definitions' (~a)"
                       collected-count state-def-count))))
    (loop :for transition-def :in transition-definitions
          :for event-name := (event transition-def)
          :for state-name := (from-state transition-def)
          :for state-event-tuple := (cons state-name event-name)
          :do (progn
                ;; possible-events-by-state
                (gethash-list-append-item
                 state-name %possible-events-by-state event-name)
                ;; transition-definitions by state-event tuple
                (if (gethash state-event-tuple
                             %transition-definitions-by-state-event-tuple)
                         (error (format nil "Duplicated state-event combination (~a)"
                                        state-event-tuple))
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

(defun equal-set (a b)
  (and (zerop (length (set-difference a b)))
       (zerop (length (set-difference b a)))))

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
  (is-true (equal-set '(:home->work :home->bed :meditate)
                      (possible-events (state-machine-example-01))))
  (is-true (equal-set '(:a->b) (possible-events (state-machine-example--started))))
  (is-true (equal-set '() (possible-events (state-machine-example--terminated))))
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

(test jump
  (let ((count 0))
    (flet ((count++ (&rest args)
             (declare (ignore args))
             (incf count)))
      (let ((sm (state-machine-example-01 :global-before-hooks (list #'count++)
                                          :global-after-hooks (list #'count++)
                                          :state-before-hooks (list #'count++)
                                          :state-after-hooks (list #'count++))))
        ;; OK
        (jump! sm :being-rich)
        (is (eq (current-state sm) :being-rich))
        (is (= 0 count))
        ;; FAIL
        (signals simple-error (jump! sm :a))))))

(test state-machine-accessors
  ;; should not be accessible
  (multiple-value-bind (sym kind)
      (ensure-symbol :state-definitions :cl-state-machine)
    (declare (ignore sym))
    (is (eq :internal kind))))


;; TODO: cl-state-machine-graphviz

;;; EOF
