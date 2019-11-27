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

> (loop-over-plist '(:a 1 :b 2) (k v)
>  (format t \"~a => ~a~%\" k v))

Will print:
  :A => 1
  :B => 2

And evaluate as `nil'."
  (let ((plist*# (gensym))
        (k# (gensym))
        (v# (gensym)))
    `(let ((,(car kv-names) nil)
           (,(cadr kv-names) nil))
       (loop :with ,plist*# := ,plist
             :for ,k# := (car ,plist*#)
             :for ,v# := (cadr ,plist*#)
             :do (if (null ,k#) (return nil)
                     (progn (psetf ,(car kv-names) ,k#
                                   ,(cadr kv-names) ,v#)
                            ,@body
                            (setf ,plist*# (cddr ,plist*#))))))))


(defun plist-merge (include-nil-val?
                    &rest plists)
  "Merge multiple plists into one plist
TODO"
  (unless plists (return-from plist-merge nil))
  (let ((plist* (copy-list (car plists))))
    (loop :for another-plist :in (cdr plists)
          :do (loop-over-plist another-plist (k v)
                               (plist-append-f plist* k v
                                               :include-nil-val? include-nil-val?)))
    plist*))

