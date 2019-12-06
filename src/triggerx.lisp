(in-package :cl-state-machine)


(defparameter *trigger-schedules* '())

(defparameter *trigger-history* '())


(defun make-trigger-schedule-entry (event args)
  (cons event args))

(defun trigger-schedule-entry-event (a-trigger-schedule-entry)
  (car a-trigger-schedule-entry))

(defun trigger-schedule-entry-args (a-trigger-schedule-entry)
  (cdr a-trigger-schedule-entry))

(defun %schedule-next-trigger! (event args)
  (append-f *trigger-schedules*
            `(,(make-trigger-schedule-entry event args))))

(defun schedule-next-trigger! (a-state-machine event &rest args)
  "Append new `trigger!'-schedule with possibility check by `compute-last-state'.

Evaluated as false unless it's possible `event' with current state of
`a-state-machine' plus existing `*trigger-schedules*'."
  (let ((schedules (append (loop :for entry :in *trigger-schedules*
                                 :collect (trigger-schedule-entry-event entry))
                           (list event))))
    (multiple-value-bind (ok? state-trail event-steps-trail)
        (compute-last-state a-state-machine schedules)
      (declare (ignore state-trail event-steps-trail))
      (if ok? (apply #'%schedule-next-trigger! (list event args))
          nil))))

(defun schedule-next-trigger-without-check! (a-state-machine event &rest args)
  (declare (ignore a-state-machine))
  (apply #'%schedule-next-trigger! (list event args)))

(defun pop-next-scheduled-trigger! ()
  "`nil' if there's no entry in `*trigger-schedules*'."
  (pop *trigger-schedules*))

(defun empty-next-trigger-schedules! ()
  (setf *trigger-schedules* '()))

(defun %append-trigger-history! (trigger!-values-list)
  (append-f *trigger-history* (list trigger!-values-list)))

(defun append-trigger-history!
  (&key ((:state-machine a-state-machine)) event args
     new-state rejected-by rejection-reason)
  (%append-trigger-history!
  `(:param (:state-machine ,a-state-machine
             :event ,event
             :args ,args)
    :result (:new-state ,new-state
             :rejected-by ,rejected-by
             :rejection-reason ,rejection-reason))))

(defun empty-trigger-history! ()
  (setf *trigger-history* '()))


(defmacro with-own-trigger-schedules-and-history
    ((&key (schedules '())
        (history '()))
     &rest body)
  `(let ((*trigger-schedules* ,schedules)
         (*trigger-history* ,history))
     ,@body
     (list :schedules *trigger-schedules*
           :history *trigger-history*)))

