(in-package :cl-state-machine-test)

(in-suite test-suite)




(test jump!
  (with-state-transition-recorder
      (records make-hook)
      (let ((sm (state-machine-example-01
                 :global-before-hooks (list (make-hook :global-before t))
                 :global-after-hooks (list (make-hook :global-after t))
                 :state-before-hooks (list (make-hook :state-before t))
                 :state-after-hooks (list (make-hook :state-after t)))))
        ;; OK
        (jump! sm :being-rich)
        (is (eq (current-state sm) :being-rich))
        (is (= 0 (length records))) ; should not evaluate any hook
        ;; FAIL
        (signals simple-error (jump! sm :a))
        (is (= 0 (length records)))))) ; should not evaluate any hook
