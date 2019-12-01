(in-package :cl-state-machine-examples/graphviz)

(defun state-machine-example-01 ()
  (state-machine-of `(:current-state :at-home)
                    (`(:state :at-work)
                      `(:state :at-home)
                      `(:state :in-bed)
                      `(:state :nirvana :terminal t)
                      `(:state :being-rich :terminal t))
                    (`(:from :at-home :to :at-work
                       :event :home->work)
                      `(:from :at-home :to :in-bed
                        :event :home->bed)
                      `(:from :in-bed :to :at-home
                        :event :wake-up)
                      `(:from :at-home :to :nirvana
                        :event :meditate)
                      `(:from :at-work :to :at-home
                        :event :work->home)
                      `(:from :at-work :to :being-rich
                        :event :show-me-the-money))))

(defun write-dot-file-example (pathname)
  (write-dot-file pathname (state-machine-example-01)))

