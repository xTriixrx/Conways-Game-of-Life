(defpackage conways-game-of-life/tests/main
  (:use :cl
	:rove
        :conways-game-of-life))
(in-package :conways-game-of-life/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :conways-game-of-life)' in your Lisp.

(deftest test-square-p
  (testing "Testing square-p predicate function."
    (let ((nil-world nil)
	  (flat-world (make-array '10 :element-type 'bit))
	  (valid-world (make-array '(10 10) :element-type 'bit))
	  (invalid-world (make-array '(10 2) :element-type 'bit)))
      (ng (square-p nil-world))
      (ng (square-p flat-world))
      (ok (square-p valid-world))
      (ng (square-p invalid-world)))))

(deftest test-glider-gen-1
  (testing "Testing of glider pattern for first generation."
    (let ((actual-world (make-array '(10 10) :element-type 'bit))
	  (expected-world (make-array '(10 10) :element-type 'bit)))
      (setf (row-major-aref expected-world 10) 1)
      (setf (row-major-aref expected-world 12) 1)
      (setf (row-major-aref expected-world 21) 1)
      (setf (row-major-aref expected-world 22) 1)
      (setf (row-major-aref expected-world 31) 1)
      (init-glider-pattern actual-world)
      (game-of-life actual-world 1 0 nil)
      (ok (world-equal actual-world expected-world)))))
