(in-package :cl-state-machine-test)

(in-suite test-suite)


(defmacro append-item-f (a-list-place item)
  `(setf ,a-list-place (append ,a-list-place (list ,item))))

(test (append-item-f-really? :suite skip-suite)
  (let ((l '()))
    (append-item-f l :a)
    (is (equal '(:a) l))))

(defstruct state-transition-record state-transition id
           (:documentation "For testing purpose. Hold arguments passed
           to hook function. Use `id' slot to identify a hook
           function."))

(defmacro make-state-transition-record-appender (list-place a-state-transition-record-id reject-transition?)
  "Make new hook function evaluates as `retval'.

Append new `state-transition-record' at the end of `list-place'.

Specify `id' slot as an identifier of new hook function. It will be
matched with `id'-slot of corresponding `state-transition-record'.

`reject-transition?' is true, it will invoke `reject-transition!'
before anything else. (which traps the evaluation of the function)"
  (let ((item# (gensym)))
    `#'(lambda (a-state-transition &rest rest-args)
         (declare (ignore rest-args))
         (let* ((,item# (make-state-transition-record
                         :state-transition a-state-transition
                         :id ,a-state-transition-record-id)))
           (append-item-f ,list-place ,item#))
         ;; at least, could know that it has been here
         (when ,reject-transition?
           (reject-transition! :datum ,a-state-transition-record-id)))))

(test make-state-transition-record-appender
  (let* ((l '())
         (appender-ok (make-state-transition-record-appender l :ok nil))
         (appender-no (make-state-transition-record-appender l :no t)))
    ;; when accepts
    (funcall appender-ok :a-state-transition)
    (is (and (= (length l) 1)
             (let ((i (car l)))
               (and (eq :a-state-transition
                        (state-transition-record-state-transition i))
                    (eq :ok
                        (state-transition-record-id i))))))
    ;; when rejects
    (signals reject-transition
      (funcall appender-no :another-state-transition))
    (is (and (= (length l) 2) ; because, anyway, it needs to be recorded
             (let ((i (cadr l)))
               (and (eq :another-state-transition
                        (state-transition-record-state-transition i))
                    (eq :no
                        (state-transition-record-id i))))))
    ;; the `datum' slot of `reject-transition' condition
    (handler-case (funcall appender-no :yet-another-state-transition)
      (reject-transition (a-condition)
        (with-slots (cl-state-machine::datum) a-condition
          (is (eq :no cl-state-machine::datum)))))))


(defmacro with-state-transition-recorder ((records-name appender-maker-name) &rest body)
  "Will evaluate `body' within an established bindings of
`records-name' and `appender-maker-name'.

`appender-maker-name' is a function which takes `(id
reject-transition?)' as parameter and returns new hook function by
`make-state-transition-record-appender'. `id' and `reject-transition?'
is pass through to `make-state-transition-record-appender'. And
`records-name' will be the first `list-place' parameter of it."
  `(let ((,records-name '()))
     (flet ((,appender-maker-name (id &optional (reject-transition? nil))
              (make-state-transition-record-appender ,records-name id reject-transition?)))
       ,@body)))
