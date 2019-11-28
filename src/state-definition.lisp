(in-package :cl-state-machine)


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
    :initarg :before-hooks :initform (list #'always-nil) :type function-list :accessor before-hooks
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
