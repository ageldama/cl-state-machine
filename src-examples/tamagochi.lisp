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

(defun foo ()
  (state-machine-of `(:current-state :home)
                    (`(:state :work)
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

(defun run-tamagochi ()
  ;; TODO
  )
