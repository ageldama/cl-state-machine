(in-package :cl-state-machine-test)

(in-suite test-suite)


(test state-machine-accessors
  (multiple-value-bind (sym kind)
      (ensure-symbol :state-definitions :cl-state-machine)
    (is (eq :external kind))
    (is (fdefinition sym))))


