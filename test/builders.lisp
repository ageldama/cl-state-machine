(in-package :cl-state-machine-test)

(in-suite test-suite)




(test state-definitions-of
  (let* ((state-defs (state-definitions-of
                      '(:state :a)
                      '(:state :b)))
         (sd-1 (first state-defs))
         (sd-2 (second state-defs)))
    (is (eq :a (state sd-1)))
    (is (eq :b (state sd-2)))))

(test transition-definitions-of
  (let* ((transition-defs (transition-definitions-of
                           '(:from :a :to :b :event :a->b)
                           '(:from :b :to :a :event :b->a)))
         (td-1 (first transition-defs))
         (td-2 (second transition-defs)))
    (is (and (eq :a (from-state td-1))
             (eq :b (to-state td-1))
             (eq :a->b (event td-1))))
    (is (and (eq :b (from-state td-2))
             (eq :a (to-state td-2))
             (eq :b->a (event td-2))))))


(test state-machine-of
  (let ((state-m (state-machine-of
                  '(:current-state :a)
                  (`(:state :a)
                    `(:state :b))
                  (`(:from :a :to :b :event :a->b)))))
    (is (eq :a (current-state state-m)))
    (let ((sd-1 (first (state-machine--state-definitions state-m)))
          (sd-2 (second (state-machine--state-definitions state-m)))
          (td-1 (first (state-machine--transition-definitions state-m))))
      (is (eq :a (state sd-1)))
      (is (eq :b (state sd-2)))
      (is (and (eq :a (from-state td-1))
               (eq :b (to-state td-1))
               (eq :a->b (event td-1)))))))
