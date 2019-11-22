(in-package :cl-state-machine)

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
