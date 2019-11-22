(in-package :cl-state-machine)


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
