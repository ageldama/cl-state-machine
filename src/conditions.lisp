(in-package :cl-state-machine)

(define-condition state-machine-error (error)
  ((format-string :initarg :format-string
                  :initform "")
   (format-arguments :initarg :format-arguments
                     :initform '())
   (extra :initarg :extra
          :initform nil))
  (:report (lambda (condition stream)
             (with-slots (format-string format-arguments) condition
               (apply #'format (append (list stream format-string)
                                       format-arguments))))))
