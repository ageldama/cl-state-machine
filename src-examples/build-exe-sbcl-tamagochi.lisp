(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cl-state-machine-examples))

(asdf:operate :build-op :cl-state-machine-examples)
