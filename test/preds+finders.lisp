(in-package :cl-state-machine-test)

(in-suite test-suite)


(test find-state-definition-by-state
  (let ((a-state-machine (state-machine-example-01)))
    (is-false (null (find-state-definition-by-state a-state-machine :nirvana)))
    (is-true (null (find-state-definition-by-state a-state-machine :at-rome)))))

(test terminated?
  (is-false (terminated? (state-machine-example--started)))
  (is-true (terminated? (state-machine-example--terminated)))
  (signals simple-error (terminated? (state-machine-example--wrong-current))))

(test find-transition-definition-by-state-and-event
  (let ((sm (state-machine-example-01)))
    (let ((td (find-transition-definition-by-state-and-event sm :at-home :home->work)))
      (is-true (not (null td)))
      (is (and (eq (event td) :home->work)
               (eq (from-state td) :at-home)
               (eq (to-state td) :at-work))))
    (let ((td (find-transition-definition-by-state-and-event sm :at-work :show-me-the-money)))
      (is-true (not (null td)))
      (is (and (eq (event td) :show-me-the-money)
               (eq (from-state td) :at-work)
               (eq (to-state td) :being-rich))))
    ;; let's not do this
    (let ((td (find-transition-definition-by-state-and-event sm :at-kino :fick)))
      (is-true  (null td)))))

(test possible-events
  (is-true (set-equal '(:home->work :home->bed :meditate)
                      (possible-events (state-machine-example-01))))
  (is-true (set-equal '(:a->b) (possible-events (state-machine-example--started))))
  (is-true (set-equal '() (possible-events (state-machine-example--terminated))))
  (signals simple-error (possible-events (state-machine-example--wrong-current))))

(test can?
  (let ((sm (state-machine-example-01)))
    (is-true (can? sm :meditate))
    (is-true (can? sm :home->bed))
    (is-true (can? sm :home->bed))
    (is-false (can? sm :work->home))))

(test can?-2
  (let ((sm (state-machine-example--terminated)))
    (is-false (can? sm :a->b))
    (is-false (can? sm :b->a))
    (is-false (can? sm :meditate))
    (is-false (can? sm :work->home))))

(test can?-3
  (signals simple-error (can? (state-machine-example--wrong-current) :sth)))
