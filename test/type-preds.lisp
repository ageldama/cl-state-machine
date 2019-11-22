(in-package :cl-state-machine-test)

(in-suite test-suite)


(test typep--state-definition-list
  (is-true (typep '() 'state-definition-list))
  (is-true (typep (list (make-instance 'state-definition))
                  'state-definition-list))
  (is-false (typep "foobar" 'state-definition-list))
  (is-false (typep '(1 2 3) 'state-definition-list)))

(test typep--non-nil-symbol
  (is-true (typep :foo 'non-nil-symbol))
  (is-true (typep 'foo 'non-nil-symbol))
  (is-false (typep 42 'non-nil-symbol))
  (is-false (typep nil 'non-nil-symbol)))

(test typep--function-list
  (is-true (typep '() 'cl-state-machine::function-list))
  (is-true (typep (list #'identity) 'cl-state-machine::function-list))
  (is-false (typep (list 123) 'cl-state-machine::function-list))
  (is-false (typep "foobar" 'cl-state-machine::function-list)))

(test typep--symbol-list
  (is-true (typep '() 'cl-state-machine::symbol-list))
  (is-true (typep (list 'foo) 'cl-state-machine::symbol-list))
  (is-false (typep (list 123) 'cl-state-machine::symbol-list))
  (is-false (typep "foobar" 'cl-state-machine::symbol-list)))
