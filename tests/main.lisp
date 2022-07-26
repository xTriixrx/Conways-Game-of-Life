(defpackage conways-game-of-life/tests/main
  (:use :cl
        :rove
        :conways-game-of-life))
(in-package :conways-game-of-life/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :conways-game-of-life)' in your Lisp.

(deftest test-square-p
  (testing "Testing square-p predicate function."
    (let ((nil-world nil)
          (valid-flat-world (make-array 100 :element-type 'bit))
          (invalid-world (make-array '(10 2) :element-type 'bit))
          (invalid-flat-world (make-array '10 :element-type 'bit))
          (invalid-square-world (make-array '(10 10) :element-type 'bit)))
      (ng (square-p nil-world))
      (ng (square-p invalid-world))
      (ng (square-p invalid-flat-world))
      (ng (square-p invalid-square-world))
      (ok (square-p valid-flat-world)))))

(deftest test-world-length
  (testing "Testing world length function"
    (let ((nil-world nil)
          (valid-flat-world (make-array 100 :element-type 'bit))
          (invalid-world (make-array '(10 2) :element-type 'bit))
          (invalid-flat-world (make-array 10 :element-type 'bit))
          (invalid-square-world (make-array '(10 10) :element-type 'bit)))
      (ng (world-length nil-world))
      (ng (world-length invalid-world))
      (ng (world-length invalid-flat-world))
      (ng (world-length invalid-square-world))
      (ok (eql (world-length valid-flat-world) 10)))))

(deftest test-copy-world-row
  (testing "Testing copy-world-row function"
    (let ((nil-world nil)
          (expected-row (make-array 10 :element-type 'bit))
          (valid-flat-world (make-array 100 :element-type 'bit))
          (invalid-world (make-array '(10 2) :element-type 'bit))
          (invalid-flat-world (make-array 10 :element-type 'bit))
          (invalid-square-world (make-array '(10 10) :element-type 'bit)))
      ; Set up valid-world to have x number of active cells for x rows
      (dotimes (i 10)
        (dotimes (j 10)
          (if (<= j i)
              (set-world valid-flat-world (+ j (* i 10)) 1))))
      (ng (copy-world-row nil-world))
      (ng (copy-world-row invalid-world))
      (ng (copy-world-row invalid-flat-world))
      (ng (copy-world-row invalid-square-world))
      (dotimes (i 10)
        (setf expected-row (copy-world-row valid-flat-world i))
        (ok (row-equal valid-flat-world expected-row i))))))

(deftest test-insert-row
  (testing "Testing insert-row function"
    (let ((row (make-array 10 :element-type 'bit))
          (world (make-array 100 :element-type 'bit)))
      (dotimes (i 10)
        (setf (aref row i) 1))
      (insert-row world row 0)
      (insert-row world row 1)
      (ok (row-equal world row 0))
      (ok (row-equal world row 1)))))

(deftest test-world-equal
  (testing "Testing world equal function"
    (let ((nil-world nil)
          (valid-world (make-array 100 :element-type 'bit))
          (valid-world2 (make-array 100 :element-type 'bit))
          (invalid-flat-world (make-array 10 :element-type 'bit))
          (invalid-world (make-array '(10 2) :element-type 'bit)))
      (init-glider-pattern valid-world)
      (init-glider-pattern valid-world2)
      (ng (world-equal nil-world invalid-flat-world))
      (ng (world-equal invalid-flat-world nil-world))
      (ok (world-equal valid-world valid-world2))
      (ng (world-equal invalid-world valid-world))
      (ng (world-equal valid-world invalid-world)))))

(deftest test-access-world
  (testing "Testing access world function"
    (let ((nil-world nil)
          (flat-world (make-array 10 :element-type 'bit))
          (valid-world (make-array 100 :element-type 'bit))
          (invalid-world (make-array '(10 2) :element-type 'bit)))
      (set-world valid-world 0 1)
      (ng (access-world nil-world 0))
      (ng (access-world flat-world 0))
      (ng (access-world valid-world -1))
      (ng (access-world invalid-world 0))
      (ng (access-world valid-world 100))
      (ok (eql (access-world valid-world 0) 1)))))

(deftest test-neighbors
  (testing "Testing the computation of determining active neighbor cells"
    (let ((test-world (make-array 100 :element-type 'bit)))
      (init-glider-pattern test-world)
      (ok (eql (active-neighbors test-world 0) 1))
      (ok (eql (active-neighbors test-world 1) 1))
      (ok (eql (active-neighbors test-world 10) 3))
      (ok (eql (active-neighbors test-world 11) 5))
      (ok (eql (active-neighbors test-world 12) 3))
      (ok (eql (active-neighbors test-world 31) 3)))))

(deftest test-glider
  (testing "Testing of glider pattern for first and last generation."
    (let ((actual-world (make-array 100 :element-type 'bit))
          (expected-world (make-array 100 :element-type 'bit)))
      (set-world expected-world 10 1)
      (set-world expected-world 12 1)
      (set-world expected-world 21 1)
      (set-world expected-world 22 1)
      (set-world expected-world 31 1)
      (init-glider-pattern actual-world)
      (game-of-life actual-world 1 0 nil nil)
      (ok (world-equal actual-world expected-world))
      (clear-world expected-world)
      (set-world expected-world 88 1)
      (set-world expected-world 89 1)
      (set-world expected-world 98 1)
      (set-world expected-world 99 1)
      ; move glider to edge of flat 10x10 world where it terminates to a still block life
      (game-of-life actual-world 30 0 nil nil)
      (ok (world-equal actual-world expected-world)))))
