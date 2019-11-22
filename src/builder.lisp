(in-package :cl-state-machine)


(defmacro state-definitions-of (&rest state-definition-args-list)
  "Turn lists of initargs for `(make-instance 'state-definition)` into
list of `state-definition' instances"
  (let ((i# (gensym)))
    `(loop :for ,i# :in (list ,@state-definition-args-list)
           :collect (apply #'make-instance 'state-definition ,i#))))

(defmacro transition-definitions-of (&rest transition-definition-args-list)
  (let ((i# (gensym)))
    `(loop :for ,i# :in (list ,@transition-definition-args-list)
           :collect (apply #'make-instance 'transition-definition ,i#))))

(defmacro state-machine-of (state-machine-args
                            state-definition-args-list
                            transition-definition-args-list)
  (let ((state-defs# (gensym))
        (transition-defs# (gensym)))
    `(let ((,state-defs# (state-definitions-of ,@state-definition-args-list))
           (,transition-defs# (transition-definitions-of ,@transition-definition-args-list)))
       (apply #'make-instance 'state-machine
              (append ,state-machine-args
                      `(:state-definitions ,,state-defs#)
                      `(:transition-definitions ,,transition-defs#))))))

