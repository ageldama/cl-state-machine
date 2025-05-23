(defsystem "cl-state-machine"
  :version "1.0.1"
  :author "Jong-Hyouk Yun <ageldama@gmail.com>"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "conditions")
                 (:file "type-pred")
                 (:file "hook-types")
                 (:file "state-definition")
                 (:file "transition-definition")
                 (:file "state-machine")
                 (:file "state-machine-init")
                 (:file "state-transition")
                 (:file "call-hook")
                 (:file "builder")
                 (:file "finders+preds")
                 (:file "compute-last-state")
                 (:file "triggerx")
                 (:file "jump+trigger")
                 )))
  :description "Finite state machine"
  :in-order-to ((test-op (test-op "cl-state-machine-test"))))
