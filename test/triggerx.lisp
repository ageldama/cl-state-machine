(in-package :cl-state-machine-test)

(in-suite test-suite)


(test trigger-schedule-entry
  (let* ((event :my-event)
         (args :my-args)
         (entry (make-trigger-schedule-entry event args)))
    (is (eq (trigger-schedule-entry-event entry) event))
    (is (eq (trigger-schedule-entry-args entry) args))))

(test with-own-trigger-schedules-and-history
  (empty-trigger-history)
  (is (eq 0 (length *trigger-schedules*)))
  (is (eq 0 (length *trigger-history*)))
  ;;
  (let ((result (with-own-trigger-schedules-and-history
                    (:schedules `(,(make-trigger-schedule-entry :a nil))
                     :history `())
                    (empty-next-trigger-schedules)
                    (append-trigger-history '(:done-a))
                    (append-trigger-history '(:done-b)))))
    (is (eq 0 (length (getf result :schedules))))
    (is (equal (getf result :history)
               '((:done-a) (:done-b)))))
  ;;
  (is (eq 0 (length *trigger-schedules*)))
  (is (eq 0 (length *trigger-history*))))

(test schedule-next-trigger
  (let ((entry (first (getf (with-own-trigger-schedules-and-history
                                ()
                                (schedule-next-trigger :my-event))
                            :schedules))))
    (is (eq :my-event (trigger-schedule-entry-event entry)))
    (is-false (trigger-schedule-entry-args entry))))

(test pop-next-scheduled-trigger
  (with-own-trigger-schedules-and-history
      ()
      ;;
      (schedule-next-trigger :my-event 1 2 3)
      (schedule-next-trigger :another-event 7 8 9)
      ;;
      (let ((entry (pop-next-scheduled-trigger)))
        (is (eq :my-event (trigger-schedule-entry-event entry)))
        (is (equal '(1 2 3) (trigger-schedule-entry-args entry))))
      (let ((entry (pop-next-scheduled-trigger)))
        (is (eq :another-event (trigger-schedule-entry-event entry)))
        (is (equal '(7 8 9) (trigger-schedule-entry-args entry))))
      (is-false (pop-next-scheduled-trigger))))

(test empty-next-trigger-schedules
  (with-own-trigger-schedules-and-history
      ()
      ;;
      (schedule-next-trigger :my-event 1 2 3)
      (schedule-next-trigger :another-event 7 8 9)
      ;;
      (empty-next-trigger-schedules)
      (is-false (pop-next-scheduled-trigger))))

(test append-trigger-history-and-empty-trigger-history
  (with-own-trigger-schedules-and-history
      ()
      ;;
      (append-trigger-history `(:a 1 2 3))
      (append-trigger-history `(:b 7 8 9))
      (is (eq 2 (length *trigger-history*)))
      (empty-trigger-history)
      (is (eq 0 (length *trigger-history*)))))

(test append-trigger-history*
  (with-own-trigger-schedules-and-history
      ()
      ;;
      (let ((sm (state-machine-example-01)))
        (append-trigger-history* :state-machine sm
                                 :event :my-event
                                 :args (list 'a 'b)
                                 :new-state :unknown
                                 :rejected-by :itself
                                 :rejection-reason :just-because)
        ;;
        (let* ((item (first *trigger-history*))
               (param (getf item :param))
               (result (getf item :result)))
          (is (eq sm (getf param :state-machine)))
          (is (eq :my-event (getf param :event)))
          (is (equal '(a b) (getf param :args)))
          (is (eq :unknown (getf result :new-state)))
          (is (eq :itself (getf result :rejected-by)))
          (is (eq :just-because (getf result :rejection-reason)))))))

(test (trigger!-appends-history :suite test-suite)
  (empty-trigger-history)
  (let ((sm (state-machine-example-01))
        (*trigger!-clear-history* nil))
    (with-own-trigger-schedules-and-history
        ()
        ;;
        (trigger! sm :home->work)
        (trigger! sm :work->home :quickly)
        (trigger! sm :show-me-the-money :hugely) ; failing
        (is (eq :at-home (current-state sm)))
        (is (eq 3 (length *trigger-history*)))
        (let* ((2nd (second *trigger-history*))
               (param (getf 2nd :param))
               (result (getf 2nd :result)))
          (is (eq (getf param :state-machine) sm))
          (is (eq (getf param :event) :work->home))
          (is (equal (getf param :args) '(:quickly)))
          (is (eq (getf result :new-state) :at-home))
          (is (eq (getf result :rejected-by) nil))
          (is (eq (getf result :rejection-reason) nil)))
        (let* ((3rd (third *trigger-history*))
               (param (getf 3rd :param))
               (result (getf 3rd :result)))
          (is (eq (getf param :state-machine) sm))
          (is (eq (getf param :event) :show-me-the-money))
          (is (equal (getf param :args) '(:hugely)))
          (is (eq (getf result :new-state) nil))
          (is (eq (getf result :rejected-by) :cannot-be-triggered))
          (is (eq (getf result :rejection-reason) :show-me-the-money)))
        ;;
        (let ((*trigger!-clear-history* t))
          (is (eq 3 (length *trigger-history*)))
          (trigger! sm :home->bed)
          (is (eq 1 (length *trigger-history*))))))
  (is (eq 0 (length *trigger-history*))))
