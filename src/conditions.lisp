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

(define-condition reject-transition (condition)
  ((format-string :initarg :format-string
                  :initform "Transition Rejected")
   (format-arguments :initarg :format-arguments
                     :initform '())
   (datum :initarg :datum
          :initform nil))
  (:report (lambda (condition stream)
             (with-slots (format-string format-arguments) condition
               (apply #'format (append (list stream format-string)
                                       format-arguments))))))

(defun reject-transition! (&key (datum nil)
                             (format-string nil)
                             (format-arguments nil))
  "Signal `reject-transition' condition"
  (apply #'error (append '(reject-transition)
                         (plist-merge nil
                                      `(:datum ,datum)
                                      `(:format-string ,format-string)
                                      `(:format-arguments ,format-arguments)))))



