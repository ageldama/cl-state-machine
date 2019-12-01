(in-package :cl-state-machine-test)

(in-suite test-suite)


(test compute-last-state
      (let ((sm (state-machine-example-01)))
        (multiple-value-bind (ok? state-trail event-steps-trail)
            (compute-last-state sm '(:home->work
                                     :work->home
                                     :home->bed))
          ;;
          (is-true ok?)
          (is (equal '(:at-work :at-home :in-bed)
                     state-trail))
          (is (eq 3 (length event-steps-trail)))
          (is (equal '(:home->work :work->home :home->bed)
                     (first event-steps-trail)))
          (is (equal '(:work->home :home->bed)
                     (second event-steps-trail)))
          (is (equal '(:home->bed)
                     (last (car event-steps-trail)))))))

(test compute-last-state--fail-1st
      (let ((sm (state-machine-example-01)))
        (multiple-value-bind (ok? state-trail event-steps-trail)
            (compute-last-state sm '(:home->work
                                     :work->home
                                     :home->bed) :at-work)
          ;;
          (is-false ok?)
          (is-false state-trail)
          (is-false event-steps-trail))))

(test compute-last-state--fail-middle
      (let ((sm (state-machine-example-01)))
        (multiple-value-bind (ok? state-trail event-steps-trail)
            (compute-last-state sm '(:work->home
                                     :home->work
                                     :home->bed) :at-work)
          ;;
          (is-false ok?)
          (is (equal '(:at-home :at-work) state-trail))
          (is (eq 2 (length event-steps-trail)))
          (is (equal '(:work->home :home->work :home->bed)
                     (first event-steps-trail)))
          (is (equal '(:home->work :home->bed)
                     (second event-steps-trail))))))
