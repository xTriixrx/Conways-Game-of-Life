(defsystem "conways-game-of-life"
  :version "0.1.0"
  :author "Vincent Nigro"
  :license ""
  :depends-on ("cl-progress-bar")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Conway's Game of Life in POSIX Common Lisp"
  :in-order-to ((test-op (test-op "conways-game-of-life/tests"))))

(defsystem "conways-game-of-life/tests"
  :author "Vincent Nigro"
  :license ""
  :depends-on ("conways-game-of-life"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for conways-game-of-life"
  :perform (test-op (op c) (symbol-call :rove :run c)))
