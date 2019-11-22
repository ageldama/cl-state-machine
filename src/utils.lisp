(in-package :cl-state-machine)


(defun gethash-list-append-item (key ht item)
  (setf (gethash key ht)
        (append (gethash key ht '()) (list item))))


