(in-package :cl-state-machine)


(defun gethash-list-append-item (key ht item)
  (setf (gethash key ht)
        (append (gethash key ht '()) (list item))))




(defmacro append-f (place &rest lists)
  "Modifying macro of `append'.

  Append each element of `lists' to `place'."
  `(setf ,place (append ,place ,@lists)))


(defmacro plist-append-f (place key val &key (include-nil-val? t))
  "Append `key' and `value' to `place' the property list.

It is a modifying macro.

If `include-nil-val?' is false, skip appending."
  `(if ,include-nil-val?
       (setf (getf ,place ,key) ,val)
       (when ,val
         (setf (getf ,place ,key) ,val))))


(defmacro loop-over-plist (plist kv-names &rest body)
  "Example:

> (loop-over-plist '(:a 1
>                    :b 2
>                    :c nil
>                    nil 4
>                    :e) (k v)
>  (format t \"~a => ~a~%\" k v))

Will print:
  A => 1
  B => 2
  C => NIL
  NIL => 4
  E => NIL

And evaluate as `nil'."
  (let ((k# (gensym))
        (v# (gensym)))
    `(let ((,(car kv-names) nil)
           (,(cadr kv-names) nil))
       (loop :for idx :from 0 :below (length ,plist) :by 2
             :for ,k# := (nth idx ,plist)
             :for ,v# := (nth (1+ idx) ,plist)
             :do (progn (psetf ,(car kv-names) ,k#
                               ,(cadr kv-names) ,v#)
                            ,@body)))))



(defun plist-copy (include-nil-val? a-plist)
  (let ((result '()))
    (loop-over-plist a-plist (k v)
                     (if include-nil-val?
                         (setf (getf result k) v)
                         (when v (setf (getf result k) v))))
    result))

(defun plist-merge (include-nil-val?
                    &rest plists)
  "Merge multiple plists into one plist."
  (unless plists (return-from plist-merge nil))
  (let ((plist* (plist-copy include-nil-val? (car plists))))
    (loop :for another-plist :in (cdr plists)
          :do (loop-over-plist another-plist (k v)
                               (plist-append-f plist* k v
                                               :include-nil-val? include-nil-val?)))
    plist*))

