(in-package :cl-state-machine)


(defun gethash-list-append-item (key ht item)
  (setf (gethash key ht)
        (append (gethash key ht '()) (list item))))




(defmacro append-f (place &rest lists)
  "Modifying macro of `append'.

  Append each element of `lists' to `place'."
  `(setf ,place (append ,place ,@lists)))


(defmacro plist-append-f (place key val &key (when-val-ok? t))
  "Append `key' and `value' to `place' the property list.

It is a modifying macro.

Append even `val' is a false value when `when-val-ok?' is specified as
false value. (default: `when-val-ok?' = true, do not append false
`val' value)"
  `(append-f ,place
             (if ,when-val-ok? (when ,val (list ,key ,val))
                 (list ,key ,val))))

(defmacro loop-over-plist (plist kv-names &rest body)
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


(defun plist-merge (when-val-ok?
                    &rest plists)
  "Merge multiple plists into one plist
TODO"
  (unless plists (return-from plist-merge nil))
  (let ((plist* (copy-list (car plists))))
    (loop :for another-plist :in (cdr plists)
          :do (progn (assert (evenp (length another-plist)))
                     (loop :for idx :from 0 :below (length another-plist) :by 2
                           :for k := (elt another-plist idx)
                           :for v := (elt another-plist (1+ idx))
                           :do (plist-append-f plist* k v
                                               :when-val-ok? when-val-ok?))))
    plist*))

