(in-package :cl-state-machine)


(defun find-state-definition-by-state (a-state-machine state)
  "Find a matching `state-definition' by given `state'-symbol.

Evaluated as `nil' if it cannot be found."
  (declare (type state-machine a-state-machine)
           (type symbol state))
  (with-slots (%state-definition-by-state) a-state-machine
    (multiple-value-bind (val present?) (gethash state %state-definition-by-state)
      ;; No need to check `present?', When cannot be found, `val' is `nil' anyway.
      ;; SEE CLHS: `gethash'
      (declare (ignore present?))
      val)))

(defun terminated? (a-state-machine)
  "True if `a-state-machine' has reached to a `state-definition' which
marked as `terminal' = true.

Can signal `simple-error' on `a-state-machine' with illegal current
state."
  (declare (type state-machine a-state-machine))
  (let* ((cur (current-state a-state-machine))
         (cur-state-def (find-state-definition-by-state a-state-machine cur)))
    (assert (not (null cur)))
    (assert (not (null cur-state-def)))
    (terminal cur-state-def)))

(defun possible-events (a-state-machine)
  "Find all possible `event'-symbols with current state of
`a-state-machine'.

Return a list of symbols and it can be empty if there's no other
possible event or the state machine has terminated.

Signal `simple-error' on `a-state-machine' of illegal current state."
  (declare (type state-machine a-state-machine))
  (when (terminated? a-state-machine)
    (return-from possible-events '()))
  (with-slots (%possible-events-by-state) a-state-machine
    (multiple-value-bind (val present?) (gethash (current-state a-state-machine)
                                                 %possible-events-by-state)
      (if present? val
          '()))))

(defun can? (a-state-machine event)
  "Evaluate as true if `a-state-machine' can be `trigger!'-ed to
`event'. If `a-state-machine` has been terminated, it will be
evaluated as false as well.

Signal `simple-error' on `a-state-machine' of illegal current state."
  (declare (type state-machine a-state-machine)
           (type symbol event))
  (member event (possible-events a-state-machine)))

(defun find-transition-definition-by-state-and-event (a-state-machine state event)
  "Can be evaluated as nil if there's no matching
`transition-definition."
  (with-slots (%transition-definitions-by-state-event-tuple) a-state-machine
    (let ((state-event (cons state event)))
      (gethash state-event %transition-definitions-by-state-event-tuple))))
