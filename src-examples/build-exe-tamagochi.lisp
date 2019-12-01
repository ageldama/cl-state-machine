(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cl-state-machine-examples))

(sb-ext:save-lisp-and-die #p"tamagochi.sbcl.exe"
  :executable t :toplevel #'cl-state-machine-examples/tamagochi:run
  :purify nil
  )
