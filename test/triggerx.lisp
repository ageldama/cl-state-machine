(in-package :cl-state-machine-test)

(in-suite test-suite)


(test trigger-schedule-entry
  (let* ((event :my-event)
         (args :my-args)
         (entry (make-trigger-schedule-entry event args)))
    (is (eq (trigger-schedule-entry-event entry) event))
    (is (eq (trigger-schedule-entry-args entry) args))))

(test with-own-trigger-schedules-and-history
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
               '(:done-a :done-b))))
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
      (append-trigger-history `(:a))
      (append-trigger-history `(:b))
      (is (eq 2 (length *trigger-history*)))
      (empty-trigger-history)
      (is (eq 0 (length *trigger-history*)))))
