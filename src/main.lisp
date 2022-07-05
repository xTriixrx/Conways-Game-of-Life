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
  (let ((size (- (world-length world) 1)))
    (loop for x from 0 to size do
      (loop for y from 0 to size do
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

(defun update (world snapshot x y)
  "Main update logic for a given position at (x, y). Will use snapshot copy to compare and update via world."
  (let* ((pos (access-world snapshot x y))
	 (left (access-world snapshot x (- y 1)))
	 (right (access-world snapshot x (+ y 1)))
	 (top (access-world snapshot (- x 1) y))
	 (bottom (access-world snapshot (+ x 1) y))
	 (top-left (access-world snapshot (- x 1) (- y 1)))
	 (top-right (access-world snapshot (- x 1) (+ y 1)))
	 (bottom-left (access-world snapshot (+ x 1) (- y 1)))
	 (bottom-right (access-world snapshot (+ x 1) (+ y 1)))
     ; Neighbors will only contain a list of 1's representing active neighbors.
	 (neighbors (remove 0 (remove nil (list left right top bottom top-left top-right bottom-left bottom-right)))))
        ; Main conditional check to determine if
    (cond ((and (eql pos 1)
		(or (eql (length neighbors) 2) (eql (length neighbors) 3)))
	   (setf (aref world x y) 1))
	  ((and (eql pos 0) (eql (length neighbors) 3))
	   (setf (aref world x y) 1))
	  (t (setf (aref world x y) 0)))))

(defun update-world (world)
  "Updates world by looping through each position and updating accordingly."
  (let ((size (- (world-length world) 1))
	(snapshot (copy-array world)))
    (loop for x from 0 to size do
      (loop for y from 0 to size do
	(update world snapshot x y)))))

(defun clear-world (world)
  "Clears the provided world by setting all positions to zero."
  (let ((size (- (world-length world) 1)))
    (loop for x from 0 to size do
      (loop for y from 0 to size do
	(setf (aref world x y) 0)))))

(defun game-of-life (world &optional (max-generation 100) (sleep-time 0.2))
  "Main game of life loop which will print out each generation and update to the next generation."
  (loop for x from 0 to max-generation do
    (print-world world)
    (update-world world)
    (sleep sleep-time)))

(defun init-pos (world)
  ""
  (setf (aref world 0 1) 1)
  (setf (aref world 1 2) 1)
  (setf (aref world 2 2) 1)
  (setf (aref world 2 1) 1)
  (setf (aref world 2 0) 1) t)

(defun init-pos-2 (world)
  ""
  (setf (aref world 8 10) 1)
  (setf (aref world 12 10) 1)
  (setf (aref world 8 8) 1)
  (setf (aref world 12 8) 1)
  (setf (aref world 9 7) 1)
  (setf (aref world 11 7) 1)
  (setf (aref world 10 6) 1) t)
  
; http://large.stanford.edu/diversions/life/rules/
; https://en.wikipedia.org/wiki/Conway's_Game_of_Life
;(defvar test-world (make-array '(20 20)))
;(init-pos-2 test-world)
;(print-world test-world)
;(game-of-life test-world 20 1)
;(clear-world test-world)
