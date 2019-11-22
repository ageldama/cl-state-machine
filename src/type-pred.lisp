(in-package :cl-state-machine)


(defmacro predicate-list-of (allow-empty? type a-list)
  `(and (locally
            #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
            (if ,allow-empty? (listp ,a-list)
                (and (listp ,a-list)
                     (not (null ,a-list)))))
        (every #'(lambda (v) (typep v ,type))
               ,a-list)))

(defun function-list? (a-list)
  (predicate-list-of t 'function a-list))

(deftype function-list ()
  `(satisfies function-list?))

(defun symbol-list? (a-list)
  (predicate-list-of t 'symbol a-list))

(deftype symbol-list ()
  `(satisfies symbol-list?))

(deftype non-nil-symbol ()
  `(and symbol
        (not null)))


