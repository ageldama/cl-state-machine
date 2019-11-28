(in-package :cl-state-machine-test)

(in-suite test-suite)



(test trigger!-on-terminated
  (multiple-value-bind (next-state-def rejected? rejection-reason)
      (trigger! (state-machine-example--terminated)  :a->b)
    (is-false next-state-def)
    (is (eq :cannot-be-triggered rejected?))
    (is (eq :a->b rejection-reason))))

(test trigger!-invalid-event
  (let ((sm (state-machine-example-01)))
    (is (eq (current-state sm) :at-home))
    (is-false (trigger! sm (gensym)))
    (is (eq (current-state sm) :at-home))))

(test trigger!-invalid-event-2
  (multiple-value-bind (next-state-def rejected? rejection-reason)
      (trigger! (state-machine-example-01)  :a->b)
    (is-false next-state-def)
    (is (eq :cannot-be-triggered rejected?))
    (is (eq :a->b rejection-reason))))

(test trigger!-illegal-current-state
  (signals state-machine-error (trigger! (state-machine-example--wrong-current)  :a->b)))



(test trigger!-ok
  (with-state-transition-recorder
      (records make-hook)
      (let* ((sm (state-machine-example-01
                  :global-before-hooks
                  (list (make-hook :global-before-a)
                        (make-hook :global-before-b))
                  :global-after-hooks
                  (list (make-hook :global-after-a)
                        (make-hook :global-after-b))
                  :state-before-hooks
                  (list (make-hook :state-before-a)
                        (make-hook :state-before-b))
                  :state-after-hooks
                  (list (make-hook :state-after-a)
                        (make-hook :state-after-b))))
             (passing-args (gensym)))
        (is (eq (current-state sm) :at-home))
        (multiple-value-bind (new-state rejected? rejection-reason)
            (trigger! sm :meditate passing-args)
          ;;
          (is-false rejected?)
          (is-false rejection-reason)
          (is (and (eq (current-state sm) :nirvana)
                   (eq (current-state sm) new-state))))
        (is (equal '(:global-before-a :global-before-b
                     :state-before-a :state-before-b
                     :state-after-a :state-after-b
                     :global-after-a :global-after-b)
                   (mapcar #'state-transition-record-id records)))
        (is-true (every #'(lambda (i) (equal (list passing-args)
                                             (state-transition-args
                                              (state-transition-record-state-transition i))))
                        records))
        ;; check `state-transition-transition-definition' of `state-transition-record-state-transition'
        (loop :with expect-transition-event := :meditate
              :with expect-transition-to := :nirvana
              :with expect-transition-from := :at-home
              :for i :in records
              :for a-state-transition := (state-transition-record-state-transition i)
              :for a-transition-definition := (state-transition-transition-definition
                                               a-state-transition)
              :do (is-true (and (eq (event a-transition-definition)
                                    expect-transition-event)
                                (eq (from-state a-transition-definition)
                                    expect-transition-from)
                                (eq (to-state a-transition-definition)
                                    expect-transition-to)))))))

(test trigger!-global-before-hook-rejection
  (with-state-transition-recorder
      (records make-hook)
      (let* ((bad-boy (make-hook :global-before-b t))
             (sm (state-machine-example-01
                  :global-before-hooks
                  (list (make-hook :global-before-a)
                        bad-boy) ; <-- the guy
                  :global-after-hooks
                  (list (make-hook :global-after-a))
                  :state-before-hooks
                  (list (make-hook :state-before-a))
                  :state-after-hooks
                  (list (make-hook :state-after-a))))
             (passing-args (gensym)))
        (is (eq (current-state sm) :at-home))
        (multiple-value-bind (next-state-def rejected? rejection-reason)
            (trigger! sm :meditate passing-args)
          (declare (ignore next-state-def))
          ;;
          (is (eq :state-machine-before-hook-rejected rejected?))
          (is (equal (cons bad-boy :global-before-b) rejection-reason))
          (is (eq (current-state sm) :at-home)) ; not changed
          (is (equal '(:global-before-a :global-before-b) ; no `global-after', `state-*'
                     (mapcar #'state-transition-record-id records)))
          (is-true (every #'(lambda (i) (equal (list passing-args)
                                               (state-transition-args
                                                (state-transition-record-state-transition i))))
                          records))))))

(test trigger!-state-before-hook-rejection
  (with-state-transition-recorder
      (records make-hook)
      (let* ((bad-boy (make-hook :state-before-b t))
               (sm (state-machine-example-01
                  :global-before-hooks
                  (list (make-hook :global-before-a)
                        (make-hook :global-before-b))
                  :global-after-hooks
                  (list (make-hook :global-after-a))
                  :state-before-hooks
                  (list (make-hook :state-before-a)
                        bad-boy) ; <-- the guy
                  :state-after-hooks
                  (list (make-hook :state-after-a t))))
             (passing-args (gensym)))
        (is (eq (current-state sm) :at-home))
        (multiple-value-bind (next-state-def rejected? rejection-reason)
            (trigger! sm :meditate passing-args)
          (declare (ignore next-state-def))
          ;;
          (is (eq :state-definition-before-hook-rejected rejected?))
          (is (equal (cons bad-boy :state-before-b) rejection-reason))
          (is (eq (current-state sm) :at-home)) ; not changed
          (is (equal '(:global-before-a :global-before-b :state-before-a :state-before-b)
                     (mapcar #'state-transition-record-id records)))
          (is-true (every #'(lambda (i) (equal (list passing-args)
                                               (state-transition-args
                                                (state-transition-record-state-transition i))))
                          records))))))



