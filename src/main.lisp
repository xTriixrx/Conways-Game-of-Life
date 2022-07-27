(defpackage conways-game-of-life
  (:use :cl
        :ltk
        :cl-progress-bar)
  (:export #:copy-array #:start #:square-p #:world-length #:access-world
     #:set-world #:copy-world-row #:insert-row #:row-equal
     #:insert-row #:world-equal #:print-world #:active-neighbors
     #:update-position #:update-world #:clear-world #:game-of-life
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
  "Predicate for determining if flat 1D array is square"
  (if (arrayp world)
      (let* ((size (car (array-dimensions world)))
             (dimension (floor (sqrt size)))
             (calc-size (* dimension dimension)))
        (eql size calc-size))
      nil))

(defun world-length (world)
  "Returns the length of the world if it is square, otherwise returns nil"
  (if (square-p world) (floor (sqrt (car (array-dimensions world)))) nil))

(defun access-world (world pos)
  "Safe access world using row-major-aref for single position, returns nil if out of bounds"
  (if (and (square-p world) (>= pos 0) (< pos (array-total-size world)))
      (row-major-aref world pos) 0))

(defun set-world (world pos val)
  "Safe setting access to world, return nil if not set and t if set."
  (setf (row-major-aref world pos) val) t)

(defun copy-world-row (world &optional (row-delta 0))
  "Copies a row within the world and returns it as a new array."
  (if (square-p world)
    (let* ((length (world-length world))
           (row (make-array length :element-type 'bit)))
      (dotimes (i length)
        (setf (aref row i) (access-world world (+ i (* row-delta length)))))
      row)
    nil))

(defun row-equal (world row row-delta)
  "Returns whether the row provided is equal to the row within the world."
  (if (square-p world)
      (let* ((is-equal t)
             (row-size (array-total-size row))
             (world-row (copy-world-row world row-delta))
             (world-row-size (array-total-size world-row)))
        (if (eql row-size world-row-size)
            (dotimes (i row-size)
              (if (not (eql (aref row i)
                            (aref world-row i)))
                  (setf is-equal nil)))
            nil)
        is-equal)
      nil))

(defun insert-row (world row row-delta)
  "Inserts a provided row into the provided world at a given row delta"
  (if (square-p world)
      (let ((length (world-length world)))
        (dotimes (i length)
          (set-world world (+ i (* row-delta length)) (aref row i)))
        t)
      nil))

(defun row-empty-p (row)
  "Predicate function for whether the provided row is empty or not"
  (let ((is-empty t))
    (dotimes (i (array-total-size row))
      (if (not (eql (aref row i) 0))
          (setf is-empty nil)))
    is-empty))

(defun clear-row (row)
  "Clears the provided row array by setting all positions to zero."
  (dotimes (i (array-total-size row))
    (setf (aref row i) 0)))

(defun world-equal (world1 world2)
  "Returns whether the 2 worlds are equal or not."
  (if (and (square-p world1) (square-p world2))
      (let ((is-equal t)
            (world-size-1 (array-total-size world1))
            (world-size-2 (array-total-size world2)))
        (if (eql world-size-1 world-size-2)
            (dotimes (i world-size-1)
              (if (not (eql (access-world world1 i)
                            (access-world world2 i)))
                  (setf is-equal nil)))
            nil)
        is-equal)
      nil))

(defun print-world (world)
  "Prints the world using row major indexing."
  (let ((row-count 0)
        (length (world-length world)))
    (dotimes (pos (array-total-size world))
      (if (eql (access-world world pos) 1)
          (format t "* ")
          (format t "- "))
      (if (eql row-count (- length 1))
          (progn (format t "~%") (setf row-count 0))
          (incf row-count)))
    (format t "~%")))

(defun active-neighbors (world pos)
  "Calculates and returns the number of active neighbor cell's to the given position."
  (let ((neighbor-count 0)
        (row-length (world-length world)))
    (dotimes (i 3)
      (dotimes (j 3)
        (if (and (not (and (eql j 1) (eql i 1)))
                 (eql (access-world world (+ pos (* row-length (- i 1)) (- j 1))) 1))
            (incf neighbor-count))))
    neighbor-count))

(defun update-position (world update-buffer pos buffer-pos)
  "Main update logic for a given position. Will use snapshot copy to compare and update via world."
  (let ((prev-val (access-world world pos))
         (neighbors (active-neighbors world pos)))
    ; Main conditional check to determine if current cell should be active or not in the next generation.
    (cond ((and (eql prev-val 1)
                (or (eql neighbors 2) (eql neighbors 3)))
           (setf (aref update-buffer buffer-pos) 1))
          ((and (eql prev-val 0) (eql neighbors 3))
           (setf (aref update-buffer buffer-pos) 1))
          (t (setf (aref update-buffer buffer-pos) 0)))))

(defun update-world (world)
  "Updates world by looping through each position and updating a line buffer accordingly."
  (let* ((length (world-length world))
         (end-of-row (- length 1))
         (lbuffer1 (make-array length :element-type 'bit))
         (lbuffer2 (make-array length :element-type 'bit)))
    ; Create a local function binding to insert a buffer into a row of the provided world.
    (labels
        ((insert-and-clear (world buffer delta)
           (insert-row world buffer delta)
           (clear-row buffer)))
      ; Iterate through each position in the world and update a line buffer until the end of the line is reached
      (dotimes (pos (array-total-size world))
        (let* ((row-delta (floor (/ pos length)))
               (buffer-pos (- pos (* row-delta length))))
          ; If an even row, use the first line buffer, otherwise use the second line buffer
          (if (eql (mod row-delta 2) 0)
              (update-position world lbuffer1 pos buffer-pos)
              (update-position world lbuffer2 pos buffer-pos))
          ; If the current row is greater than the first row and the buffer position matches the end row position
          (if (and (>= row-delta 1)
                   (eql buffer-pos end-of-row))
              ; If the current row is even, then the first line buffer was populated; inserts latest buffer into
              ; the previous row in the world
              (if (eql (mod row-delta 2) 0)
                  (insert-and-clear world lbuffer2 (- row-delta 1))
                  (insert-and-clear world lbuffer1 (- row-delta 1)))))))
    ; For the last buffer, if the first buffer is not empty it contains the last update, otherwise the second.
    (if (not (row-empty-p lbuffer1))
        (insert-row world lbuffer1 end-of-row)
        (insert-row world lbuffer2 end-of-row))))

(defun clear-world (world)
  "Clears the provided world by setting all positions to zero."
  (if (square-p world)
      (dotimes (pos (array-total-size world))
        (set-world world pos 0))
      nil))

(defun game-of-life (world &optional (max-generation 100) (sleep-time 0.2) (print-generation t) (print-progress t))
  "Main game of life loop which will print out each generation and update to the next generation."
  (let ((length (world-length world)))
    (labels ((operate-generations (world max-generation sleep-time print-generation)
               (dotimes (generation max-generation)
                 (if print-generation
                     (print-world world))
                 (update-world world)
                 (sleep sleep-time))))
      ;; Operate on the generations with the progress printing
      (if print-progress
          (progn
            (setf cl-progress-bar:*progress-bar-enabled* t)
            (cl-progress-bar:with-progress-bar
                (max-generation "Performing Game of Life on ~ax~a world for ~a generations." length length max-generation)
              (operate-generations world max-generation sleep-time print-generation)
              (cl-progress-bar:update 1)))
          ;; Operate on generations without printing the progress
          (operate-generations world max-generation sleep-time print-generation)))))

(defun init-glider-pattern (world)
  "A basic glider that is placed at the beginning of the world."
  (let ((size (world-length world)))
    (if size
        (progn
          (setf (aref world 1) 1)
          (setf (aref world (+ 2 (* 1 size))) 1)
          (setf (aref world (+ 2 (* 2 size))) 1)
          (setf (aref world (+ 1 (* 2 size))) 1)
          (setf (aref world (* 2 size)) 1)
          t)
        nil)))

(defun init-c-pattern (world)
  "A basic c shape that becomes an oscillator. World should be 20x20 at a minimum."
  (let* ((size (world-length world))
        (delta (/ size 2)))
    (if (and size (>= size 20))
        (progn
          (setf (aref world (+ delta (* size (- delta 2)))) 1)
          (setf (aref world (+ delta (* size (+ delta 2)))) 1)
          (setf (aref world (+ (- delta 2) (* size (- delta 2)))) 1)
          (setf (aref world (+ (- delta 2) (* size (+ delta 2)))) 1)
          (setf (aref world (+ (- delta 3) (* size (- delta 1)))) 1)
          (setf (aref world (+ (- delta 3) (* size (+ delta 1)))) 1)
          (setf (aref world (+ (- delta 4) (* size delta))) 1)
          t)
        nil)))

(defun init-random-pattern (world &optional (coverage-percentage 0.6))
  "Initializes a square world with some random pattern."
  (let* ((length (world-length world))
        (marks (round (* (expt length 2) coverage-percentage))))
    (if (not (eql length nil))
        (progn
          (dotimes (i marks)
            (let ((x (random length))
                  (y (random length)))
              (setf (aref world (+ y (* x length))) 1)))
          t)
        nil)))

; http://large.stanford.edu/diversions/life/rules/
; https://en.wikipedia.org/wiki/Conway's_Game_of_Life
; https://www.jagregory.com/abrash-black-book/#chapter-17-the-game-of-life
; https://stackoverflow.com/questions/30552102/optimizing-neighbor-count-function-for-conways-game-of-life-in-c
