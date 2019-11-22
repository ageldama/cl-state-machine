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

(defmacro make-state-transition-record-appender (list-place a-state-transition-record-id retval)
  "Make new hook function evaluates as `retval'.

Append new `state-transition-record' at the end of `list-place'.

Specify `id' slot as an identifier of new hook function. It will be
matched with `id'-slot of corresponding `state-transition-record'."
  (let ((item# (gensym)))
    `#'(lambda (a-state-transition &rest rest-args)
         (declare (ignore rest-args))
         (let* ((,item# (make-state-transition-record
                         :state-transition a-state-transition
                         :id ,a-state-transition-record-id)))
           (append-item-f ,list-place ,item#))
         ,retval)))

(defmacro with-state-transition-recorder ((records-name appender-maker-name) &rest body)
  "Will evaluate `body' within an established bindings of
`records-name' and `appender-maker-name'.

`appender-maker-name' is a function which takes `(id ret-val)' as
parameter and returns new hook function by
`make-state-transition-record-appender'. `id' and `ret-val' is pass
through to `make-state-transition-record-appender'. And `records-name'
will be the first `list-place' parameter of it."
  `(let ((,records-name '()))
     (flet ((,appender-maker-name (id ret-val)
              (make-state-transition-record-appender ,records-name id ret-val)))
       ,@body)))
