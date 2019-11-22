(in-package :cl-state-machine)


(defmethod initialize-instance :after ((a-state-machine state-machine) &rest args)
  (declare (ignore args))
  (with-slots (state-definitions transition-definitions
               %state-definition-by-state
               %possible-events-by-state
               %transition-definitions-by-state-event-tuple) a-state-machine
    ;; fill state-definitions/state lookup table
    (loop :for state-def :in state-definitions
          :for state-name := (state state-def)
          :do (setf (gethash state-name %state-definition-by-state) state-def))
    (let ((collected-count (hash-table-count %state-definition-by-state))
          (state-def-count (length state-definitions)))
      (when (/= collected-count state-def-count)
        ;; ensure no dups in states
        (error "Collected `state'-count (~a) does not match with length of `state-definitions' (~a)"
               collected-count state-def-count)))
    (loop :for transition-def :in transition-definitions
          :for event-name := (event transition-def)
          :for state-name := (from-state transition-def)
          :for state-event-tuple := (cons state-name event-name)
          :do (progn
                ;; fill possible-events/state lookup table
                (gethash-list-append-item
                 state-name %possible-events-by-state event-name)
                ;; fill transition-definitions / state-event tuple
                ;; lookup table
                (if (gethash state-event-tuple
                             %transition-definitions-by-state-event-tuple)
                    (error "Duplicated state-event combination (~a)"
                           state-event-tuple)
                    ;; else, OK
                    (setf (gethash state-event-tuple
                                   %transition-definitions-by-state-event-tuple)
                          transition-def))))))
