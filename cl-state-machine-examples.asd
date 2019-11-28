(defsystem "cl-state-machine-examples"
  :version "1.0.0"
  :author "Jong-Hyouk Yun <ageldama@gmail.com>"
  :license "MIT"
  :depends-on ("cl-state-machine" "cl-state-machine-graphing" "alexandria")
  :components ((:module "src-examples"
                :serial t
                :components
                ((:file "package")
                 (:file "tamagochi")
                 (:file "graphviz")
                 )))
  :description "Examples of cl-state-machine")
