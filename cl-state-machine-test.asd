(defsystem "cl-state-machine-test"
  :version "1.0.0"
  :author "Jong-Hyouk Yun <ageldama@gmail.com>"
  :license "MIT"
  :depends-on ("fiveam"
               "alexandria"
               "cl-state-machine")
  :components ((:module "test"
                :serial t
                :components
                ((:file "package")
                 (:file "tests"))))
  :description "Test suites of cl-state-machine"
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam '#:run!
                       (uiop:find-symbol* '#:test-suite
                                            :cl-state-machine-test)))
  )
