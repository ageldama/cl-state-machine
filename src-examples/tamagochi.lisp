(in-package :cl-state-machine-examples/tamagochi)

(defparameter *tamagochi-model*
  `(:initargs (:money 5
               :food 5
               :hygiene 5
               :health 5)
    :event->effect
    (:eat (:money -2
                 :food +3
                 :hygiene -1
                 :health +2)
     :go-home (:money -1
                     :food -1
                     :hygiene -1
                     :health +1)
     :go-to-work (:money +5
                        :food -1
                        :hygiene -1
                        :health -2)
     :sleep (:food -1
                  :hygiene -1
                  :health +3)
     :shower (:food -1
                   :hygiene +3
                   :health +1))
    :constraints
    (:money ,(lambda (current-val effect-val)
               (when (< (+ current-val effect-val) 0)
                 :not-enough-money)))))

(defclass tamagochi-status ()
  ((money :initarg :money
          :accessor money
          :initform 0)
   (food :initarg :food
         :accessor food
         :initform 0)
   (hygiene :initarg :hygiene
            :accessor hygiene
            :initform 0)
   (health :initarg :health
           :accessor health
           :initform 0)
   (turns :initarg :turns
          :accessor turns
          :initform 0)
   (cause-of-death :initarg :cause-of-death
                   :accessor cause-of-death
                   :initform nil)
   (model :initarg :model
          :reader model
          :initform *tamagochi-model*)))

(defun keyword->symbol (kw-or-symbol)
  (with-input-from-string (s-in (string kw-or-symbol))
    (read s-in)))

(defmethod initialize-instance :after ((a-tamagochi-status tamagochi-status) &rest args)
  (declare (ignore args))
  ;; copy `:initargs' of `model'-slot into corresponding slot of `tamagochi-status'
  (let ((initargs (getf (model a-tamagochi-status) :initargs)))
    (doplist (k v initargs)
             (setf (slot-value a-tamagochi-status (keyword->symbol k)) v))))

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

(defun apply-model-before-hook (a-state-transition &rest args)
  (declare (ignore args))
  (with-tamagochi-status a-status a-state-transition
    (let* ((transition-def
             (state-transition-transition-definition a-state-transition))
           (event (event transition-def))
           (model (model a-status))
           (effects (getf (getf model :event->effect) event))
           (constraints (getf model :constraints)))
      (doplist (k v effects)
               (let ((slot-symbol (keyword->symbol k)))
                 ;; constraint checks
                 (when-let (constraint (getf constraints k))
                   (when-let (reason (funcall constraint
                                              (slot-value a-status slot-symbol) v))
                     (reject-transition! :datum reason)))
                 ;; all constraints satisfied, update state
                 (incf (slot-value a-status slot-symbol) v))))))

(defun make-tamagochi-state-machine (a-tamagochi-status)
  (state-machine-of `(:current-state :home
                      :datum ,a-tamagochi-status
                      :before-hooks (list ,#'apply-model-before-hook)
                      :after-hooks (list ,#'is-alive?-after-hook
                                         ,#'inc-turns-after-hook))
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
                        :event :shower))))

(defun display-status (a-tamagochi-status a-state-machine)
  (terpri)
  ;; alive?
  (when (eq :dead (current-state a-state-machine))
    (format *query-io* "Your tamagochi is dead (Cause: ~a)~%"
            (cause-of-death a-tamagochi-status)))
  ;; numbers..
  (format *query-io* "Status: Money=~a Food=~a Hygiene=~a Health=~a / Turns=~a~%"
          (money a-tamagochi-status)
          (food a-tamagochi-status)
          (hygiene a-tamagochi-status)
          (health a-tamagochi-status)
          (turns a-tamagochi-status)))

(defun prompt (a-tamagochi-status a-state-machine)
  (declare (ignore a-tamagochi-status))
  (terpri)
  (format *query-io* "Choice:~% ~{~s ~}~%"
          (append (list :quit)
                  (possible-events a-state-machine)))
  (format *query-io* ">>> ")
  (read *query-io*))

(defun run ()
  (let* ((a-status (make-instance 'tamagochi-status))
         (a-state-machine (make-tamagochi-state-machine a-status))
         (choice nil))
    (loop :named repl-loop
          :do (progn (display-status a-status a-state-machine)
                     (setf choice (prompt a-status a-state-machine))
                     (when (eq :quit choice)
                       (format *query-io* "~%~%Bye!~%")
                       (return-from repl-loop))
                     (if (member choice (possible-events a-state-machine))
                         (multiple-value-bind (new-state rejected-by rejection-reason)
                             (trigger! a-state-machine choice)
                           (declare (ignore rejected-by))
                           ;; transition rejected
                           (unless new-state
                             (format *query-io* "Cannot do ~a, because of ~a~%~%"
                                     choice (cdr rejection-reason))))
                         ;; unknown choice
                         (format *query-io* "~a ???~%" choice))))))

