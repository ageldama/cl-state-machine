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


(test plist-append-if-ok-f
  (let ((l '(:banana :yellow)))
    (cl-state-machine::plist-append-if-ok-f l :apple :red)
    (cl-state-machine::plist-append-if-ok-f l :pineaple nil)
    (cl-state-machine::plist-append-if-ok-f l :anana :green)
    (is (equal l '(:banana :yellow :apple :red :anana :green)))))

