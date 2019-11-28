(defsystem "cl-state-machine-examples"
  :version "1.0.0"
  :author "Jong-Hyouk Yun <ageldama@gmail.com>"
  :license "MIT"
  :depends-on ("cl-state-machine")
  :components ((:module "src-examples"
                :serial t
                :components
                ((:file "package")
                 )))
  :description "Examples of cl-state-machine")
