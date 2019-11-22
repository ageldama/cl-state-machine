(in-package :cl-state-machine-test)

(in-suite test-suite)

(test call-after-hooks
  (with-state-transition-recorder
      (records make-hook)
      (let* (;; `state-transition'
             (sm (state-machine-example-01))
             (td (find-transition-definition-by-state-and-event sm :at-home :home->work))
             (a-state-transition (make-state-transition :state-machine sm
                                                        :transition-definition td))
             ;; hooks
             (hook-1 (make-hook :1 nil)) ; `after-hook' no care evaluated value
             (hook-2 (make-hook :2 nil))
             (hooks (list hook-1 hook-2)))
        (is-false (call-after-hooks hooks a-state-transition))
        (is (= 2 (length records)))
        (is (equal '(:1 :2) (mapcar #'state-transition-record-id records)))
        (is-true (every #'(lambda (i) (equal (state-transition-record-state-transition i)
                                             a-state-transition))
                        records)))))

(test call-before-hooks
  (with-state-transition-recorder
      (records make-hook)
      (let* (;; `state-transition'
             (sm (state-machine-example-01))
             (td (find-transition-definition-by-state-and-event sm :at-home :home->work))
             (a-state-transition (make-state-transition :state-machine sm
                                                        :transition-definition td))
             ;; hooks
             (hook-1 (make-hook :1 t)) ; evaluted as true
             (hook-2 (make-hook :2 t)) ; same here
             (hooks (list hook-1 hook-2)))
        (is-false (call-before-hooks hooks a-state-transition))
        (is (= 2 (length records))) ; every hooks evaluated!
        (is (equal '(:1 :2) (mapcar #'state-transition-record-id records)))
        (is-true (every #'(lambda (i) (equal (state-transition-record-state-transition i)
                                             a-state-transition))
                        records)))))

(test call-before-hooks-2
  ;; Does `before-hook' evaluates as false reject consequent evaluations?
  (with-state-transition-recorder
      (records make-hook)
      (let* (;; `state-transition'
             (sm (state-machine-example-01))
             (td (find-transition-definition-by-state-and-event sm :at-home :home->work))
             (a-state-transition (make-state-transition :state-machine sm
                                                        :transition-definition td))
             ;; hooks
             (hook-1 (make-hook :1 nil)) ; should stops here
             (hook-2 (make-hook :2 t))   ; never be evaluated
             (hooks (list hook-1 hook-2)))
        (is (eq hook-1 (call-before-hooks hooks a-state-transition)))
        (is (= 1 (length records)))
        (is (equal '(:1) (mapcar #'state-transition-record-id records)))
        (is (equal (state-transition-record-state-transition (first records))
                   a-state-transition)))))

