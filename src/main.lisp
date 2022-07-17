(defpackage conways-game-of-life
  (:use :cl)
  (:export #:copy-array #:square-p #:world-length #:world-equal
	   #:print-world #:row-access-world #:active-neighbors
	   #:update #:update-world #:clear-world #:game-of-life
	   #:init-glider-pattern #:init-c-pattern #:init-random-pattern))
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
  (if (arrayp world)
      (let ((dimensions (array-dimensions world)))
	(and (eql (length dimensions) 2)
	     (eql (car dimensions) (cadr dimensions))))
      nil))

(defun world-length (world)
  "Returns the length of the world if it is square, otherwise returns nil"
  (if (square-p world) (car (array-dimensions world)) nil))

(defun world-equal (world1 world2)
  "Returns whether the 2 worlds are equal or not."
  (if (and (square-p world1) (square-p world2))
      (let ((is-equal t)
	    (world-size-1 (array-total-size world1))
	    (world-size-2 (array-total-size world2)))
	(if (eql world-size-1 world-size-2)
	    (dotimes (i world-size-1)
	      (if (not (eql (row-major-aref world1 i)
			    (row-major-aref world2 i)))
		  (setf is-equal nil)))
	    nil)
	is-equal)
      nil))

(defun print-world (world)
  "Prints the world using row major indexing."
  (let ((row-count 0)
        (length (world-length world)))
    (dotimes (pos (array-total-size world))
      (if (eql (row-major-aref world pos) 1)
          (format t "* ")
          (format t "- "))
      (if (eql row-count (- length 1))
          (progn (format t "~%") (setf row-count 0))
          (incf row-count)))
    (format t "~%")))

(defun row-access-world (world pos)
  "Safe access world using row-major-aref for single position, returns nil if out of bounds"
  (if (and (square-p world) (>= pos 0) (< pos (array-total-size world)))
      (row-major-aref world pos) nil))

(defun active-neighbors (world pos)
  "Calculates and returns the number of active neighbor cell's to the given position."
  (let ((neighbor-count 0)
        (row-length (world-length world)))
    (dotimes (i 3)
      (dotimes (j 3)
        (if (and (not (and (eql j 1) (eql i 1)))
                 (eql (row-access-world world (+ pos (* row-length (- i 1)) (- j 1))) 1))
            (incf neighbor-count))))
    neighbor-count))

(defun update (world snapshot pos)
  "Main update logic for a given position. Will use snapshot copy to compare and update via world."
  (let* ((prev-val (row-major-aref snapshot pos))
         (neighbors (active-neighbors snapshot pos)))
    ; Main conditional check to determine if current cell should be active or not in the next generation.
    (cond ((and (eql prev-val 1)
                (or (eql neighbors 2) (eql neighbors 3)))
           (setf (row-major-aref world pos) 1))
          ((and (eql prev-val 0) (eql neighbors 3))
           (setf (row-major-aref world pos) 1))
          (t (setf (row-major-aref world pos) 0)))))

;; Needs to provide update function one row at a time, where line buffer 1 contains updates for s
(defun update-world (world)
  "Updates world by looping through each position and updating accordingly."
  (let ((snapshot (copy-array world)))
    (dotimes (pos (array-total-size world))
      (update world snapshot pos))))

(defun clear-world (world)
  "Clears the provided world by setting all positions to zero."
    (dotimes (pos (array-total-size world))
      (setf (row-major-aref world pos) 0)))

(defun game-of-life (world &optional (max-generation 100) (sleep-time 0.2) (print-generation t))
  "Main game of life loop which will print out each generation and update to the next generation."
  (dotimes (x max-generation)
    (if print-generation
	(print-world world))
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
