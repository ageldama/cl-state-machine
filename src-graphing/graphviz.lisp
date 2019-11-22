(in-package :cl-state-machine-graphing)



(defun write-dot (out a-state-machine &key (rankdir :LR) (size "8,5") extra-header-str)
  (declare (type state-machine a-state-machine))
  (format out "digraph finite_state_machine {~%")

  (when rankdir
    (format out "  rankdir=~a;~%" (string-upcase rankdir)))

  (when size
    (format out "  size=\"~a\";~%" size))

  (when extra-header-str
    (format out extra-header-str))

  ;; nodes
  (loop :for state-def :in (state-machine--state-definitions
                            a-state-machine)
        :do (format out "  node [shape = ~a, label = \"~a\"]; \"~a\";~%"
                    (if (terminal state-def) "doublecircle"
                        "circle")
                    (string (state state-def))
                    (string (state state-def))))

  ;; starting points
  (let ((entry-sym (gensym)))
    (format out "  node [shape=point, label=\"\"] \"~a\";~%" entry-sym)
    (format out "  \"~a\"->\"~a\" [label=\"\"];~%"
            entry-sym
            (current-state a-state-machine)))

  ;; edges
  (loop :for transition-def :in (state-machine--transition-definitions
                                 a-state-machine)
        :do (format out "  \"~a\"->\"~a\" [label=\"~a\"];~%"
                    (string (from-state transition-def))
                    (string (to-state transition-def))
                    (string (event transition-def))))

  (format out "}~%"))


(defun write-dot-file (output-pathname &rest dot-string-args)
  (uiop:call-with-output-file output-pathname
                              #'(lambda (out) (apply #'write-dot (append (list out)
                                                                         dot-string-args)))))
