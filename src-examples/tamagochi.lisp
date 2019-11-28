(in-package :cl-state-machine-examples)

(defclass tamagochi-status ()
  ((money :initarg :money
          :accessor money
          :initform 5)
   (food :initarg :food
         :accessor food
         :initform 5)
   (hygiene :initarg :hygiene
            :accessor hygiene
            :initform 5)
   (health :initarg :health
           :accessor health
           :initform 5)
   (turns :initarg :turns
          :accessor turns
          :initform 0)
   (cause-of-death :initarg :cause-of-death
                   :accessor cause-of-death
                   :initform nil)))

(defmacro with-tamagochi-status (binding-name a-state-transition &rest body)
  (let ((a-state-machine# (gensym)))
    `(let* ((,a-state-machine# (state-transition-state-machine ,a-state-transition))
            (,binding-name (datum ,a-state-machine#)))
       ,@body)))

(defun need-to-die? (a-state-transition)
  (with-tamagochi-status status a-state-transition
    (cond ((>= 0 (food status)) :too-hungry)
          ((>= 0 (hygiene status)) :too-dirty)
          ((>= 0 (health status)) :too-unhealthy)
          (t nil))))

(defun is-alive?-after-hook (a-state-transition &rest args)
  (declare (ignore args))
  (with-tamagochi-status a-status a-state-transition
    (when-let (cause (need-to-die? a-state-transition))
      (let ((a-state-machine (state-transition-state-machine a-state-transition)))
        (setf (cause-of-death a-status) cause)
        (jump! a-state-machine :dead)))))

(defun inc-turns-after-hook (a-state-transition &rest args)
  (declare (ignore args))
  (with-tamagochi-status status a-state-transition
    (incf (turns status))))

(defun go-to-work-before-hook (a-state-transition &rest args)
  (declare (ignore args))
  (with-tamagochi-status status a-state-transition
    (when (>= 0 (money status)) (reject-transition! :datum :no-money))
    ;; else
    (incf (money status) 5)
    (decf (food status) 1)
    (decf (hygiene status) 1)
    (decf (health status) 1)))

(defun go-home-before-hook (a-state-transition &rest args)
  (declare (ignore args))
  (with-tamagochi-status status a-state-transition
    (when (>= 0 (money status)) (reject-transition! :datum :no-money))
    ;; else
    (decf (money status) 1)
    (decf (food status) 1)
    (decf (hygiene status) 1)
    (incf (health status) 1)))

(defun eat-before-hook (a-state-transition &rest args)
  (declare (ignore args))
  (with-tamagochi-status status a-state-transition
    (when (>= 0 (money status)) (reject-transition! :datum :no-money))
    ;; else
    (decf (money status) 2)
    (incf (food status) 3)
    (decf (hygiene status) 1)
    (incf (health status) 2)))

(defun sleep-before-hook (a-state-transition &rest args)
  (declare (ignore args))
  (with-tamagochi-status status a-state-transition
    (decf (food status) 1)
    (decf (hygiene status) 1)
    (incf (health status) 3)))

(defun shower-before-hook (a-state-transition &rest args)
  (declare (ignore args))
  (with-tamagochi-status status a-state-transition
    (decf (food status) 1)
    (incf (hygiene status) 3)
    (incf (health status) 1)))

(defun make-tamagochi-state-machine (a-tamagochi-status)
  (state-machine-of `(:current-state :home
                      :datum ,a-tamagochi-status
                      :after-hooks (list ,#'is-alive?-after-hook
                                         ,#'inc-turns-after-hook))
                    (`(:state :home)
                      `(:state :work)
                      `(:state :dead :terminal t))
                    (`(:from :home :to :work
                       :event :go-to-work
                       :before-hooks (list ,#'go-to-work-before-hook))
                      `(:from :work :to :home
                        :event :go-home
                        :before-hooks (list ,#'go-home-before-hook))
                      `(:from :home :to :home
                        :event :eat
                        :before-hooks (list ,#'eat-before-hook))
                      `(:from :home :to :home
                        :event :sleep
                        :before-hooks (list ,#'sleep-before-hook))
                      `(:from :home :to :home
                        :event :shower
                        :before-hooks (list ,#'shower-before-hook)))))

(defun display-status (a-tamagochi-status a-state-machine)
  (terpri)
  ;; alive?
  (when (eq :dead (current-state a-state-machine))
    (format t "Your tamagochi is dead (Cause: ~a)~%"
            (cause-of-death a-tamagochi-status)))
  ;; numbers..
  (format t "Status: Money=~a Food=~a Hygiene=~a Health=~a / Turns=~a~%"
          (money a-tamagochi-status)
          (food a-tamagochi-status)
          (hygiene a-tamagochi-status)
          (health a-tamagochi-status)
          (turns a-tamagochi-status)))

(defun prompt (a-tamagochi-status a-state-machine)
  (declare (ignore a-tamagochi-status))
  (terpri)
  (format t "Choice:~% ~{~s ~}~%"
          (append (list :quit)
                  (possible-events a-state-machine)))
  (princ ">>> ")
  (read))

(defun run-tamagochi ()
  (let* ((a-status (make-instance 'tamagochi-status))
         (a-state-machine (make-tamagochi-state-machine a-status))
         (choice nil))
    (loop :named repl-loop
          :do (progn (display-status a-status a-state-machine)
                     (setf choice (prompt a-status a-state-machine))
                     (when (eq :quit choice)
                       (format t "~%~%Bye!~%")
                       (return-from repl-loop))
                     (if (member choice (possible-events a-state-machine))
                         (trigger! a-state-machine choice)
                         (format t "~a ???~%" choice))))))

