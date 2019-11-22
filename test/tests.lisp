(in-package :cl-state-machine-test)

(def-suite test-suite)

(def-suite skip-suite)

(in-suite test-suite)

;;; Do it! (fiveam:run! 'cl-state-machine-test:test-suite)

(test typep--state-definition-list
  (is-true (typep '() 'state-definition-list))
  (is-true (typep (list (make-instance 'state-definition))
                  'state-definition-list))
  (is-false (typep "foobar" 'state-definition-list))
  (is-false (typep '(1 2 3) 'state-definition-list)))

(test typep--non-nil-symbol
  (is-true (typep :foo 'non-nil-symbol))
  (is-true (typep 'foo 'non-nil-symbol))
  (is-false (typep 42 'non-nil-symbol))
  (is-false (typep nil 'non-nil-symbol)))

(test typep--function-list
  (is-true (typep '() 'cl-state-machine::function-list))
  (is-true (typep (list #'identity) 'cl-state-machine::function-list))
  (is-false (typep (list 123) 'cl-state-machine::function-list))
  (is-false (typep "foobar" 'cl-state-machine::function-list)))

(test typep--symbol-list
  (is-true (typep '() 'cl-state-machine::symbol-list))
  (is-true (typep (list 'foo) 'cl-state-machine::symbol-list))
  (is-false (typep (list 123) 'cl-state-machine::symbol-list))
  (is-false (typep "foobar" 'cl-state-machine::symbol-list)))

(test adjoinf
  (let ((s '()))
    (cl-state-machine::adjoinf s :a)
    (cl-state-machine::adjoinf s :a)
    (is-false (zerop (length s)))
    (is (= 1 (length s)))
    (cl-state-machine::adjoinf s :a)
    (cl-state-machine::adjoinf s :b)
    (is (= 2 (length s)))
    (is-true (and (member :a s)
                  (member :b s)))))


(test state-definitions-of
  (let* ((state-defs (state-definitions-of
                      '(:state :a)
                      '(:state :b)))
         (sd-1 (first state-defs))
         (sd-2 (second state-defs)))
    (is (eq :a (state sd-1)))
    (is (eq :b (state sd-2)))))

(test transition-definitions-of
  (let* ((transition-defs (transition-definitions-of
                           '(:from :a :to :b :event :a->b)
                           '(:from :b :to :a :event :b->a)))
         (td-1 (first transition-defs))
         (td-2 (second transition-defs)))
    (is (and (eq :a (from-state td-1))
             (eq :b (to-state td-1))
             (eq :a->b (event td-1))))
    (is (and (eq :b (from-state td-2))
             (eq :a (to-state td-2))
             (eq :b->a (event td-2))))))


(test state-machine-of
  (let ((state-m (state-machine-of
                  '(:current-state :a)
                  (`(:state :a)
                    `(:state :b))
                  (`(:from :a :to :b :event :a->b)))))
    (is (eq :a (current-state state-m)))
    (let ((sd-1 (first (state-machine--state-definitions state-m)))
          (sd-2 (second (state-machine--state-definitions state-m)))
          (td-1 (first (state-machine--transition-definitions state-m))))
      (is (eq :a (state sd-1)))
      (is (eq :b (state sd-2)))
      (is (and (eq :a (from-state td-1))
               (eq :b (to-state td-1))
               (eq :a->b (event td-1)))))))

(defun state-machine-example-01 (&key (global-before-hooks '())
                                   (global-after-hooks '())
                                   (state-before-hooks '())
                                   (state-after-hooks '()))
  (state-machine-of `(:current-state :at-home
                      :before-hooks ,global-before-hooks
                      :after-hooks ,global-after-hooks)
                    (`(:state :at-work
                       :before-hooks ,state-before-hooks
                       :after-hooks ,state-after-hooks)
                      `(:state :at-home
                        :before-hooks ,state-before-hooks
                        :after-hooks ,state-after-hooks)
                      `(:state :in-bed
                        :before-hooks ,state-before-hooks
                        :after-hooks ,state-after-hooks)
                      `(:state :nirvana :terminal t
                        :before-hooks ,state-before-hooks
                        :after-hooks ,state-after-hooks)
                      `(:state :being-rich :terminal t
                        :before-hooks ,state-before-hooks
                        :after-hooks ,state-after-hooks))
                    (`(:from :at-home :to :at-work
                       :event :home->work)
                      `(:from :at-home :to :in-bed
                        :event :home->bed)
                      `(:from :at-home :to :nirvana
                        :event :meditate)
                      `(:from :at-work :to :at-home
                        :event :work->home)
                      `(:from :at-work :to :being-rich
                        :event :show-me-the-money))))

(defun state-machine-example--started ()
  (state-machine-of '(:current-state :a)
                    (`(:state :a)
                      `(:state :b :terminal t))
                    (`(:from :a :to :b :event :a->b))))

(defun state-machine-example--terminated ()
  (state-machine-of '(:current-state :b)
                    (`(:state :a)
                      `(:state :b :terminal t))
                    (`(:from :a :to :b :event :a->b))))

(defun state-machine-example--wrong-current ()
  (state-machine-of '(:current-state :x)
                    (`(:state :a)
                      `(:state :b :terminal t))
                    (`(:from :a :to :b :event :a->b))))


(test find-state-definition-by-state
  (let ((a-state-machine (state-machine-example-01)))
    (is-false (null (find-state-definition-by-state a-state-machine :nirvana)))
    (is-true (null (find-state-definition-by-state a-state-machine :at-rome)))))

(test terminated?
  (is-false (terminated? (state-machine-example--started)))
  (is-true (terminated? (state-machine-example--terminated)))
  (signals simple-error (terminated? (state-machine-example--wrong-current))))

(test find-transition-definition-by-state-and-event
  (let ((sm (state-machine-example-01)))
    (let ((td (find-transition-definition-by-state-and-event sm :at-home :home->work)))
      (is-true (not (null td)))
      (is (and (eq (event td) :home->work)
               (eq (from-state td) :at-home)
               (eq (to-state td) :at-work))))
    (let ((td (find-transition-definition-by-state-and-event sm :at-work :show-me-the-money)))
      (is-true (not (null td)))
      (is (and (eq (event td) :show-me-the-money)
               (eq (from-state td) :at-work)
               (eq (to-state td) :being-rich))))
    ;; let's not do this
    (let ((td (find-transition-definition-by-state-and-event sm :at-kino :fick)))
      (is-true  (null td)))))

(test gethash-list-append-item
  (let ((ht (make-hash-table)))
    (cl-state-machine::gethash-list-append-item :a ht 'a)
    (cl-state-machine::gethash-list-append-item :a ht 'b)
    (cl-state-machine::gethash-list-append-item :b ht 'x)
    ;;
    (is (= 2 (hash-table-count ht)))
    (is (and (equal '(a b) (gethash :a ht))
             (equal '(x) (gethash :b ht))))))

(test possible-events
  (is-true (set-equal '(:home->work :home->bed :meditate)
                      (possible-events (state-machine-example-01))))
  (is-true (set-equal '(:a->b) (possible-events (state-machine-example--started))))
  (is-true (set-equal '() (possible-events (state-machine-example--terminated))))
  (signals simple-error (possible-events (state-machine-example--wrong-current))))

(test can?
  (let ((sm (state-machine-example-01)))
    (is-true (can? sm :meditate))
    (is-true (can? sm :home->bed))
    (is-true (can? sm :home->bed))
    (is-false (can? sm :work->home))))

(test can?-2
  (let ((sm (state-machine-example--terminated)))
    (is-false (can? sm :a->b))
    (is-false (can? sm :b->a))
    (is-false (can? sm :meditate))
    (is-false (can? sm :work->home))))

(test can?-3
  (signals simple-error (can? (state-machine-example--wrong-current) :sth)))

(test state-machine-accessors
  ;; should not be accessible
  (multiple-value-bind (sym kind)
      (ensure-symbol :state-definitions :cl-state-machine)
    (declare (ignore sym))
    (is (eq :internal kind))))

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

(test jump!
  (with-state-transition-recorder
      (records make-hook)
      (let ((sm (state-machine-example-01
                 :global-before-hooks (list (make-hook :global-before t))
                 :global-after-hooks (list (make-hook :global-after t))
                 :state-before-hooks (list (make-hook :state-before t))
                 :state-after-hooks (list (make-hook :state-after t)))))
        ;; OK
        (jump! sm :being-rich)
        (is (eq (current-state sm) :being-rich))
        (is (= 0 (length records))) ; should not evaluate any hook
        ;; FAIL
        (signals simple-error (jump! sm :a))
        (is (= 0 (length records)))))) ; should not evaluate any hook

(test call-after-hooks
  (with-state-transition-recorder
      (records make-hook)
      (let* (;; `state-transition'
             (sm (state-machine-example-01))
             (td (find-transition-definition-by-state-and-event sm :at-home :home->work))
             (a-state-transition (make-state-transition :state-machine sm
                                                        :transition-definition td))
             ;; hooks
             (hook-1 (make-hook :1 nil)) ; `after-hook' no care evaluated value
             (hook-2 (make-hook :2 nil))
             (hooks (list hook-1 hook-2)))
        (is-false (call-after-hooks hooks a-state-transition))
        (is (= 2 (length records)))
        (is (equal '(:1 :2) (mapcar #'state-transition-record-id records)))
        (is-true (every #'(lambda (i) (equal (state-transition-record-state-transition i)
                                             a-state-transition))
                        records)))))

(test call-before-hooks
  (with-state-transition-recorder
      (records make-hook)
      (let* (;; `state-transition'
             (sm (state-machine-example-01))
             (td (find-transition-definition-by-state-and-event sm :at-home :home->work))
             (a-state-transition (make-state-transition :state-machine sm
                                                        :transition-definition td))
             ;; hooks
             (hook-1 (make-hook :1 t)) ; evaluted as true
             (hook-2 (make-hook :2 t)) ; same here
             (hooks (list hook-1 hook-2)))
        (is-false (call-before-hooks hooks a-state-transition))
        (is (= 2 (length records))) ; every hooks evaluated!
        (is (equal '(:1 :2) (mapcar #'state-transition-record-id records)))
        (is-true (every #'(lambda (i) (equal (state-transition-record-state-transition i)
                                             a-state-transition))
                        records)))))

(test call-before-hooks-2
  ;; Does `before-hook' evaluates as false reject consequent evaluations?
  (with-state-transition-recorder
      (records make-hook)
      (let* (;; `state-transition'
             (sm (state-machine-example-01))
             (td (find-transition-definition-by-state-and-event sm :at-home :home->work))
             (a-state-transition (make-state-transition :state-machine sm
                                                        :transition-definition td))
             ;; hooks
             (hook-1 (make-hook :1 nil)) ; should stops here
             (hook-2 (make-hook :2 t))   ; never be evaluated
             (hooks (list hook-1 hook-2)))
        (is (eq hook-1 (call-before-hooks hooks a-state-transition)))
        (is (= 1 (length records)))
        (is (equal '(:1) (mapcar #'state-transition-record-id records)))
        (is (equal (state-transition-record-state-transition (first records))
                   a-state-transition)))))


(test trigger!-on-terminated
  (multiple-value-bind (next-state-def rejected? rejection-reason)
      (trigger! (state-machine-example--terminated)  :a->b)
    (is-false next-state-def)
    (is (eq :cannot-be-triggered rejected?))
    (is (eq :a->b rejection-reason))))

(test trigger!-invalid-event
  (let ((sm (state-machine-example-01)))
    (is (eq (current-state sm) :at-home))
    (is-false (trigger! sm (gensym)))
    (is (eq (current-state sm) :at-home))))

(test trigger!-invalid-event-2
  (multiple-value-bind (next-state-def rejected? rejection-reason)
      (trigger! (state-machine-example-01)  :a->b)
    (is-false next-state-def)
    (is (eq :cannot-be-triggered rejected?))
    (is (eq :a->b rejection-reason))))

(test trigger!-illegal-current-state
  (signals simple-error (trigger! (state-machine-example--wrong-current)  :a->b)))



(test trigger!-ok
  (with-state-transition-recorder
      (records make-hook)
      (let* ((sm (state-machine-example-01
                  :global-before-hooks
                  (list (make-hook :global-before-a t)
                        (make-hook :global-before-b t))
                  :global-after-hooks
                  (list (make-hook :global-after-a t)
                        (make-hook :global-after-b t))
                  :state-before-hooks
                  (list (make-hook :state-before-a t)
                        (make-hook :state-before-b t))
                  :state-after-hooks
                  (list (make-hook :state-after-a t)
                        (make-hook :state-after-b t))))
             (passing-args (gensym)))
        (is (eq (current-state sm) :at-home))
        (multiple-value-bind (state-def rejected? rejection-reason)
            (trigger! sm :meditate passing-args)
          ;;
          (is-false rejected?)
          (is-false rejection-reason)
          (is (and (eq (current-state sm) :nirvana)
                   (eq (current-state sm) (state state-def)))))
        (is (equal '(:global-before-a :global-before-b
                     :state-before-a :state-before-b
                     :state-after-a :state-after-b
                     :global-after-a :global-after-b)
                   (mapcar #'state-transition-record-id records)))
        (is-true (every #'(lambda (i) (equal (list passing-args)
                                             (state-transition-args
                                              (state-transition-record-state-transition i))))
                        records))
        ;; check `state-transition-transition-definition' of `state-transition-record-state-transition'
        (loop :with expect-transition-event := :meditate
              :with expect-transition-to := :nirvana
              :with expect-transition-from := :at-home
              :for i :in records
              :for a-state-transition := (state-transition-record-state-transition i)
              :for a-transition-definition := (state-transition-transition-definition
                                               a-state-transition)
              :do (is-true (and (eq (event a-transition-definition)
                                    expect-transition-event)
                                (eq (from-state a-transition-definition)
                                    expect-transition-from)
                                (eq (to-state a-transition-definition)
                                    expect-transition-to)))))))

(test trigger!-global-before-hook-rejection
  (with-state-transition-recorder
      (records make-hook)
      (let* ((bad-boy (make-hook :global-before-b nil))
             (sm (state-machine-example-01
                  :global-before-hooks
                  (list (make-hook :global-before-a t)
                        bad-boy) ; <-- the guy
                  :global-after-hooks
                  (list (make-hook :global-after-a t))
                  :state-before-hooks
                  (list (make-hook :state-before-a t))
                  :state-after-hooks
                  (list (make-hook :state-after-a t))))
             (passing-args (gensym)))
        (is (eq (current-state sm) :at-home))
        (multiple-value-bind (next-state-def rejected? rejection-reason)
            (trigger! sm :meditate passing-args)
          (declare (ignore next-state-def))
          ;;
          (is (eq :state-machine-before-hook-rejected rejected?))
          (is (eq bad-boy rejection-reason))
          (is (eq (current-state sm) :at-home)) ; not changed
          (is (equal '(:global-before-a :global-before-b) ; no `global-after', `state-*'
                     (mapcar #'state-transition-record-id records)))
          (is-true (every #'(lambda (i) (equal (list passing-args)
                                               (state-transition-args
                                                (state-transition-record-state-transition i))))
                          records))))))

(test trigger!-state-before-hook-rejection
  (with-state-transition-recorder
      (records make-hook)
      (let* ((bad-boy (make-hook :state-before-b nil))
               (sm (state-machine-example-01
                  :global-before-hooks
                  (list (make-hook :global-before-a t)
                        (make-hook :global-before-b t))
                  :global-after-hooks
                  (list (make-hook :global-after-a t))
                  :state-before-hooks
                  (list (make-hook :state-before-a t)
                        bad-boy) ; <-- the guy
                  :state-after-hooks
                  (list (make-hook :state-after-a t))))
             (passing-args (gensym)))
        (is (eq (current-state sm) :at-home))
        (multiple-value-bind (next-state-def rejected? rejection-reason)
            (trigger! sm :meditate passing-args)
          (declare (ignore next-state-def))
          ;;
          (is (eq :state-definition-before-hook-rejected rejected?))
          (is (eq bad-boy rejection-reason))
          (is (eq (current-state sm) :at-home)) ; not changed
          (is (equal '(:global-before-a :global-before-b :state-before-a :state-before-b)
                     (mapcar #'state-transition-record-id records)))
          (is-true (every #'(lambda (i) (equal (list passing-args)
                                               (state-transition-args
                                                (state-transition-record-state-transition i))))
                          records))))))



