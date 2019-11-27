(in-package :cl-state-machine)


(defun gethash-list-append-item (key ht item)
  (setf (gethash key ht)
        (append (gethash key ht '()) (list item))))




(defmacro append-f (place &rest lists)
  `(setf ,place (append ,place ,@lists)))


(defmacro plist-append-if-ok-f (place key val)
  `(when ,val (append-f ,place (list ,key ,val))))

(defun plist-merge-if-ok-f (place-plist a-plist)
  (assert (evenp (length a-plist)))
  (loop :for idx :from 0 :below (length a-plist) :by 2
        :for k := (elt a-plist idx)
        :for v := (elt a-plist (1+ idx))
        :do (plist-append-if-ok-f place-plist k v)))


