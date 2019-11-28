(in-package :cl-state-machine-test)

(in-suite test-suite)


(test reject-transition!
  (signals reject-transition (reject-transition!)))

(test reject-transition!-only-datum
  (handler-case
      (reject-transition! :datum :foobar)
    (reject-transition (condition)
      (with-slots (cl-state-machine::datum) condition
        (is (eq cl-state-machine::datum :foobar))))))

