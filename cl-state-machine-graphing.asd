(defsystem "cl-state-machine-graphing"
  :version "1.0.0"
  :author "Jong-Hyouk Yun <ageldama@gmail.com>"
  :license "MIT"
  :depends-on ("cl-state-machine" "uiop")
  :components ((:module "src-graphing"
                :serial t
                :components
                ((:file "package")
                 (:file "graphviz")
                 )))
  :description "Graphing of cl-state-machine"
  ;; TODO: :in-order-to ((test-op (test-op "cl-state-machine-graphing-test")))
  )
