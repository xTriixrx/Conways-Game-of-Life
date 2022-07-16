(defpackage conways-game-of-life/tests/main
  (:use :cl
	:rove
        :conways-game-of-life))
(in-package :conways-game-of-life/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :conways-game-of-life)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

(deftest test-glider-gen-1
  (testing "Testing of glider pattern for first generation."
    (let ((actual-world (make-array '(10 10)))
	  (expected-world (make-array '(10 10))))
      (setf (row-major-aref expected-world 10) 1)
      (setf (row-major-aref expected-world 12) 1)
      (setf (row-major-aref expected-world 21) 1)
      (setf (row-major-aref expected-world 22) 1)
      (setf (row-major-aref expected-world 31) 1)
      (init-glider-pattern actual-world)
      (game-of-life actual-world 1 0)
      (ok (world-equal actual-world expected-world)))))
