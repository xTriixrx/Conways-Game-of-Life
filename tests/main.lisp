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

(deftest test-world-length
  (testing "Testing world length function"
    (let ((nil-world nil)
	  (flat-world (make-array 10 :element-type 'bit))
	  (valid-world (make-array '(10 10) :element-type 'bit))
	  (invalid-world (make-array '(10 2) :element-type 'bit)))
      (ok (eql (world-length nil-world) nil))
      (ok (eql (world-length flat-world) nil))
      (ok (eql (world-length valid-world) 10))
      (ok (eql (world-length invalid-world) nil)))))

(deftest test-world-equal
  (testing "Testing world equal function"
    (let ((nil-world nil)
	  (flat-world (make-array 10 :element-type 'bit))
	  (valid-world (make-array '(10 10) :element-type 'bit))
	  (valid-world2 (make-array '(10 10) :element-type 'bit))
	  (invalid-world (make-array '(10 2) :element-type 'bit)))
      (init-glider-pattern valid-world)
      (init-glider-pattern valid-world2)
      (ng (world-equal nil-world flat-world))
      (ng (world-equal flat-world nil-world))
      (ok (world-equal valid-world valid-world2))
      (ng (world-equal invalid-world valid-world))
      (ng (world-equal valid-world invalid-world)))))

(deftest test-row-access-world
  (testing "Testing row access world function"
    (let ((nil-world nil)
	  (flat-world (make-array 10 :element-type 'bit))
	  (valid-world (make-array '(10 10) :element-type 'bit))
	  (invalid-world (make-array '(10 2) :element-type 'bit)))
      (setf (row-major-aref valid-world 0) 1)
      (ng (row-access-world nil-world 0))
      (ng (row-access-world flat-world 0))
      (ng (row-access-world valid-world -1))
      (ng (row-access-world invalid-world 0))
      (ng (row-access-world valid-world 100))
      (ok (eql (row-access-world valid-world 0) 1)))))

(deftest test-neighbors
  (testing "Testing the computation of determining active neighbor cells"
    (let ((test-world (make-array '(10 10) :element-type 'bit)))
      (init-glider-pattern test-world)
      (ok (eql (active-neighbors test-world 0) 1))
      (ok (eql (active-neighbors test-world 1) 1))
      (ok (eql (active-neighbors test-world 10) 3))
      (ok (eql (active-neighbors test-world 11) 5))
      (ok (eql (active-neighbors test-world 12) 3))
      (ok (eql (active-neighbors test-world 31) 3)))))

(deftest test-glider
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
