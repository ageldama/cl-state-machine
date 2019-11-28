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
          :initform 0)))

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
  (when-let (cause-of-death (need-to-die? a-state-transition))
    (let ((a-state-machine (state-transition-state-machine a-state-transition)))
      ;; TODO:
      (jump! a-state-machine :dead))))

(defun inc-turns-after-hook (a-state-transition &rest args)
  (with-tamagochi-status status a-state-transition
    (incf (turns status))))

(defun make-tamagochi-state-machine ()
  (let* ((a-status (make-instance 'tamagochi-status)))
    (state-machine-of `(:current-state :home
                        :datum ,a-status)
                      (`(:state :home)
                       `(:state :work)
                        `(:state :dead :terminal t))
                      (`(:from :home :to :work
                         :event :go-to-work)
                        `(:from :work :to :home
                          :event :go-home)
                        `(:from :home :to :home
                          :event :eat)
                        `(:from :home :to :home
                          :event :sleep)
                        `(:from :home :to :home
                          :event :shower)))))

(defun run-tamagochi ()
  ;; TODO
  )
