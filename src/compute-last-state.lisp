(in-package :cl-state-machine)


(defun compute-last-state (a-state-machine
                           event-steps
                           &optional (start-state (current-state a-state-machine)))
  "Compute the state transition by each `event-steps' in list.

Beware that this function never invoke any registered hook function,
only follows the `transition-definition's in `a-state-machine', it
means this isn't a guarantee that every step of given
`event-steps'-list would successfully `trigger!'-ed.

Evaluated as `(values ALL-CONSUMED? STATE-TRAIL-LIST EVENT-STEP-TRAIL-LIST)',
Where:

  - `ALL-CONSUMED?' is true, if every `event-steps' item have been
    consumed successfully.

  - `STATE-TRAIL-LIST' is a list of state symbols, every transitioned
    state symbol will be included. If none of `event-steps' were made
    transition, this list will be empty. (`nil')

  - `EVENT-STEP-TRAIL-LIST' is a list of sublists that generated from
    given `event-steps'. But only the step in `event-steps' that
    successfully made transition will be included in this list.
     - For example, the first item of `event-steps' cannot be triggered
       from `start-state', `EVENT-STEP-TRAIL-LIST' will be empty.
     - But you have `(:a :b c)' as `event-steps' where
       the `:b' cannot be triggered, in this case `EVENT-STEP-TRAIL-LIST'
       is `((:a :b :c))' because only `:a' was the triggered event step.
     - If all of the `event-steps' are trigger-able, the `EVENT-STRP-TRAIL-LIST'
       is `((:a :b :c) (:b :c) (:c))'."
  (let ((last-state start-state)
        (event-step-trail '())
        (state-trail '()))
    (loop :for event-step-sublist :on event-steps
          :do (let* ((event-step (car event-step-sublist))
                     (a-transition-def (find-transition-definition-by-state-and-event
                                        a-state-machine last-state event-step)))
                (unless a-transition-def (return))
                (append-f event-step-trail (list event-step-sublist))
                (append-f state-trail
                          (list (setf last-state (to-state a-transition-def))))))
    (values (eq (length event-steps)
                (length state-trail))
            state-trail event-step-trail)))

