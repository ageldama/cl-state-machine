(in-package :cl-state-machine-test)

(in-suite test-suite)


(test gethash-list-append-item
  (let ((ht (make-hash-table)))
    (cl-state-machine::gethash-list-append-item :a ht 'a)
    (cl-state-machine::gethash-list-append-item :a ht 'b)
    (cl-state-machine::gethash-list-append-item :b ht 'x)
    ;;
    (is (= 2 (hash-table-count ht)))
    (is (and (equal '(a b) (gethash :a ht))
             (equal '(x) (gethash :b ht))))))

(test append-f
  (let ((l '()))
    (cl-state-machine::append-f l)
    (is (equal l '())))
  (let ((l '(:a)))
    (cl-state-machine::append-f l '(:b))
    (is (equal l '(:a :b)))
    (cl-state-machine::append-f l '(:c) '(:d))
    (is (equal l '(:a :b :c :d)))))



(defun hash-table-equal? (ht-a ht-b
                          &key
                          (keyset-test #'equal)
                          (each-value-test #'equal)
                          (key-compare #'string<))
  (flet ((hash-table-sorted-keys (ht)
           (sort (alexandria:hash-table-keys ht) key-compare)))
    (let ((keys-a (hash-table-sorted-keys ht-a))
          (keys-b (hash-table-sorted-keys ht-b)))
      (unless (funcall keyset-test keys-a keys-b)
        (return-from hash-table-equal? nil))
      (loop :for k :being :the :hash-keys :of ht-a
            :unless (funcall each-value-test
                             (gethash k ht-a)
                             (gethash k ht-b))
              :do (return-from hash-table-equal? nil)))
    t))

(test hash-table-equal?
  (is-false (hash-table-equal? (alexandria:plist-hash-table '(:a 1 :b 2 :c))
                               (alexandria:plist-hash-table '(:a 1 :b 2 :c 3))))
  (is (hash-table-equal? (alexandria:plist-hash-table '())
                         (alexandria:plist-hash-table nil)))
  (is (hash-table-equal? (alexandria:plist-hash-table '(:a 1 :b 2 :c 3))
                         (alexandria:plist-hash-table '(:c 3 :a 1 :b 2)))))

(defmacro plist-equal? (plist-a plist-b)
  `(hash-table-equal? (alexandria:plist-hash-table ,plist-a)
                      (alexandria:plist-hash-table ,plist-b)))

(test plist-append-f-include-nil
  (let ((l '(:banana :yellow)))
    (cl-state-machine::plist-append-f l :apple :red)
    (cl-state-machine::plist-append-f l :pineapple nil)
    (cl-state-machine::plist-append-f l :anana :green)
    (cl-state-machine::plist-append-f l :apple :green)
    (is (plist-equal? l
                      '(:banana :yellow :apple :green :pineapple nil :anana :green)))))

(test plist-append-f-exclude-nil
  (let ((l '(:banana :yellow)))
    (cl-state-machine::plist-append-f l :apple :red)
    (cl-state-machine::plist-append-f l :pineapple nil :include-nil-val? nil)
    (cl-state-machine::plist-append-f l :anana :green)
    (is (plist-equal? l '(:banana :yellow :apple :red :anana :green)))))

(test plist-append-f-nil-1st
  (let ((l '()))
    (cl-state-machine::plist-append-f l :apple :red)
    (is (equal l '(:apple :red)))))


(test loop-over-plist
  (let ((l '(:a 1 :b 2 :c))
        (collected '(:a 42)))
    (cl-state-machine::loop-over-plist l (k v)
                                       (when (and k v)
                                         (setf (getf collected k) v)))
    (is (plist-equal? '(:a 1 :b 2) collected))
    (is (equal '(:a 1 :b 2 :c) l))
    ;;
    (setf collected '())
    (cl-state-machine::loop-over-plist '() (k v)
                                       (when (and k v)
                                         (setf (getf collected k) v)))
    (is (equal '() collected))))


(test plist-merge
  (let ((plist-a '(:apple :red :banana :yellow))
        (plist-nil nil)
        (plist-b '(:pokemon :yellow))
        (plist-c '(:투명드래곤 nil
                   :apple :green
                   )))
    (is (plist-equal? '(:apple :green :banana :yellow
                        :pokemon :yellow)
               (cl-state-machine::plist-merge nil plist-a plist-nil plist-b plist-c)))
    (is (plist-equal? '(:apple :red :banana :yellow) plist-a))
    (is (plist-equal? '(:apple :green :banana :yellow
                        :pokemon :yellow
                        :투명드래곤 nil)
                      (cl-state-machine::plist-merge t plist-a plist-nil plist-b plist-c)))
    (is (plist-equal? '(:pokemon :yellow
                        :투명드래곤 nil :apple :green)
                      ;; NOTE: forget to specify `when-val?' positional arg.
                      (cl-state-machine::plist-merge plist-a plist-nil plist-b plist-c)))))
