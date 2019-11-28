(in-package :cl-state-machine)



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
   (datum
    :initarg :datum :initform nil
    :reader state-machine--datum
    :documentation "Anything you want to attach to a `state-machine'")
   (%state-definition-by-state :initform (make-hash-table))
   (%possible-events-by-state :initform (make-hash-table))
   (%transition-definitions-by-state-event-tuple :initform (make-hash-table :test #'equal))))

(defmethod print-object ((obj state-machine) out)
  (with-slots (state-definitions transition-definitions) obj
    (print-unreadable-object (obj out :type t)
      (format out "current-state=~a"
              (current-state obj)))))


