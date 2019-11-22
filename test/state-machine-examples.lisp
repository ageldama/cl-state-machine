(in-package :cl-state-machine-test)


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
