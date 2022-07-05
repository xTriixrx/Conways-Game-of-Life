(defpackage conways-game-of-life/tests/main
  (:use :cl
        :conways-game-of-life
        :rove))
(in-package :conways-game-of-life/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :conways-game-of-life)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
