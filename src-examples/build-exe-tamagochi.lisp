(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cl-state-machine)
  (asdf:operate :build-op :cl-state-machine-examples))
