(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cl-state-machine-examples))

#+sbcl (sb-ext:disable-debugger)
(asdf:operate :build-op :cl-state-machine-examples)
