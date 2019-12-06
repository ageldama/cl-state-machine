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
  :build-operation "asdf:program-op"
  :build-pathname "tamagochi.exe"
  :entry-point "cl-state-machine-examples/tamagochi:run+quit"
  :description "Examples of cl-state-machine")
