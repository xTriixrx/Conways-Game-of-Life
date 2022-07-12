(defpackage conways-game-of-life
  (:use :cl))
(in-package :conways-game-of-life)

(defun copy-array (array &key
                   (element-type (array-element-type array))
                   (fill-pointer (and (array-has-fill-pointer-p array)
                                      (fill-pointer array)))
                   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

(defun square-p (world)
  "Predicate for determining if array is square"
  (let ((dimensions (array-dimensions world)))
    (and (eql (length dimensions) 2)
	 (eql (car dimensions) (cadr dimensions)))))

(defun world-length (world)
  "Returns the length of the world if it is square, otherwise returns nil"
  (if (square-p world) (car (array-dimensions world)) nil))

(defun print-world (world)
  "Will print the current instance of the world"
  (let ((size (world-length world)))
    (dotimes (x size)
      (dotimes (y size)
        (if (eql (aref world x y) 1)
            (format t "* ")
            (format t "- ")))
      (format t "~%"))
    (format t "~%")))

(defun access-world (world x y)
  "Safe access world, returns nil if out of bounds"
  (if (and (>= x 0) (< x (world-length world))
	   (>= y 0) (< y (world-length world)))
      (aref world x y) nil))

(defun active-neighbors (world x y)
  "Traverses the neighbors around a position and returns the count of active neighbors in the provided generation."
  (let ((neighbor-count 0))
    (dotimes (i 3)
      (dotimes (j 3)
        ; If the traversed cell is active and is not the position, it is a neighbor
        (if (and (eql (access-world world (+ x (- i 1)) (+ y (- j 1))) 1)
                 (not (and (eql (+ x (- i 1)) x) (eql (+ y (- j 1)) y))))
            (incf neighbor-count))))
    neighbor-count))

(defun update (world snapshot x y)
  "Main update logic for a given position at (x, y). Will use snapshot copy to compare and update via world."
  (let* ((pos (access-world snapshot x y))
	 (neighbors (active-neighbors snapshot x y)))
    ; Main conditional check to determine if current cell should be active or not in the next generation.
    (cond ((and (eql pos 1)
		(or (eql neighbors 2) (eql neighbors 3)))
	   (setf (aref world x y) 1))
	  ((and (eql pos 0) (eql neighbors 3))
	   (setf (aref world x y) 1))
	  (t (setf (aref world x y) 0)))))

;; Needs to provide update function one row at a time, where line buffer 1 contains updates for s
(defun update-world (world)
  "Updates world by looping through each position and updating accordingly."
  (let ((size (world-length world))
        (snapshot (copy-array world)))
    (dotimes (x size)
      (dotimes (y size)
        (update world snapshot x y)))))

(defun clear-world (world)
  "Clears the provided world by setting all positions to zero."
  (let ((size (world-length world)))
    (dotimes (x size)
      (dotimes (y size)
        (setf (aref world x y) 0)))))

(defun game-of-life (world &optional (max-generation 100) (sleep-time 0.2))
  "Main game of life loop which will print out each generation and update to the next generation."
  (dotimes (x max-generation)
    (print-world world)
    (update-world world)
    (sleep sleep-time)))

(defun init-glider-pattern (world)
  "A basic glider that is placed at the beginning of the world."
  (setf (aref world 0 1) 1)
  (setf (aref world 1 2) 1)
  (setf (aref world 2 2) 1)
  (setf (aref world 2 1) 1)
  (setf (aref world 2 0) 1) t)

(defun init-c-pattern (world)
  "A basic c shape that becomes an oscillator. World should be 20x20 at a minimum."
  (let ((size (/ (world-length world) 2)))
    (setf (aref world (- size 2) size) 1)
    (setf (aref world (+ size 2) size) 1)
    (setf (aref world (- size 2) (- size 2)) 1)
    (setf (aref world (+ size 2) (- size 2)) 1)
    (setf (aref world (- size 1) (- size 3)) 1)
    (setf (aref world (+ size 1) (- size 3)) 1)
    (setf (aref world size (- size 4)) 1) t))

(defun init-random-pattern (world &optional (coverage-percentage 0.6))
  "Initializes a square world with some random pattern."
  (let* ((length (world-length world))
        (marks (round (* (expt length 2) coverage-percentage))))
    (if (not (eql length nil))
        (progn
          (dotimes (i marks)
            (let ((x (random length))
                  (y (random length)))
              (setf (aref world x y) 1))) t) nil)))

; http://large.stanford.edu/diversions/life/rules/
; https://en.wikipedia.org/wiki/Conway's_Game_of_Life
;(defvar test-world (make-array '(20 20)))
;(init-pos-2 test-world)
;(print-world test-world)
;(game-of-life test-world 20 1)
;(clear-world test-world)
