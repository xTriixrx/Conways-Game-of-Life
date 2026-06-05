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

(declaim (inline next-cell-state))
(defun next-cell-state (prev-val neighbors)
  "Returns the next state for one cell."
  (declare (optimize (speed 3) (safety 1) (debug 0))
           (type bit prev-val)
           (type fixnum neighbors))
  (the bit
       (if (or (and (= prev-val 1)
                    (or (= neighbors 2) (= neighbors 3)))
               (and (= prev-val 0) (= neighbors 3)))
           1
           0)))

(defun step-world (world &optional next-world changed-cells)
  "Updates WORLD by one generation and returns indexes whose values changed.
NEXT-WORLD can be supplied as a reusable bit-vector buffer with the same size as WORLD.
CHANGED-CELLS can be supplied as a reusable fixnum vector with a fill pointer."
  (unless (typep world 'simple-bit-vector)
    (error "WORLD must be a simple bit vector."))
  (let* ((world-size (cl:length world))
         (dimension (isqrt world-size))
         (next-world (or next-world
                         (make-array world-size :element-type 'bit)))
         (changed-cells (or changed-cells
                            (make-array world-size
                                        :element-type 'fixnum
                                        :fill-pointer 0))))
    (declare (optimize (speed 3) (safety 1) (debug 0))
             (type fixnum world-size dimension))
    (if (not (and (= world-size (* dimension dimension))
                  (typep next-world 'simple-bit-vector)
                  (typep changed-cells '(vector fixnum))
                  (eql world-size (array-total-size next-world))
                  (array-has-fill-pointer-p changed-cells)
                  (<= world-size (array-total-size changed-cells))))
        (error "WORLD and NEXT-WORLD must be simple bit vectors with matching square sizes, and CHANGED-CELLS must have enough fill-pointer capacity."))
    (setf (fill-pointer changed-cells) 0)
    (let ((world (the simple-bit-vector world))
          (next-world (the simple-bit-vector next-world))
          (changed-cells (the (vector fixnum) changed-cells))
          (dimension (the fixnum dimension)))
      (declare (type simple-bit-vector world next-world)
               (type (vector fixnum) changed-cells)
               (type fixnum dimension))
      (labels ((edge-neighbor-count (row col)
                 (declare (type fixnum row col))
                 (let ((count 0))
                   (declare (type fixnum count))
                   (loop for neighbor-row fixnum from (1- row) to (1+ row) do
                     (when (and (>= neighbor-row 0) (< neighbor-row dimension))
                       (loop for neighbor-col fixnum from (1- col) to (1+ col) do
                         (when (and (>= neighbor-col 0)
                                    (< neighbor-col dimension)
                                    (not (and (= neighbor-row row)
                                              (= neighbor-col col))))
                           (incf count (sbit world (+ neighbor-col
                                                       (* neighbor-row dimension))))))))
                   count))
               (update-cell (pos neighbors)
                 (declare (type fixnum pos neighbors))
                 (let* ((prev-val (sbit world pos))
                        (next-val (next-cell-state prev-val neighbors)))
                   (declare (type bit prev-val next-val))
                   (setf (sbit next-world pos) next-val)
                   (when (/= prev-val next-val)
                     (vector-push pos changed-cells)))))
        (declare (inline edge-neighbor-count update-cell))
        ;; Interior cells have eight valid neighbors, so avoid per-neighbor bounds checks.
        (loop for row fixnum from 1 below (1- dimension) do
          (let ((pos (+ 1 (* row dimension))))
            (declare (type fixnum pos))
            (loop for col fixnum from 1 below (1- dimension) do
              (update-cell pos
                           (+ (sbit world (- pos dimension 1))
                              (sbit world (- pos dimension))
                              (sbit world (- pos dimension -1))
                              (sbit world (1- pos))
                              (sbit world (1+ pos))
                              (sbit world (+ pos dimension -1))
                              (sbit world (+ pos dimension))
                              (sbit world (+ pos dimension 1))))
              (incf pos))))
        ;; Edges are a small fraction of the grid, so keep bounds checks isolated here.
        (dotimes (row dimension)
          (declare (type fixnum row))
          (dotimes (col dimension)
            (declare (type fixnum col))
            (when (or (= row 0)
                      (= col 0)
                      (= row (1- dimension))
                      (= col (1- dimension)))
              (let ((pos (+ col (* row dimension))))
                (declare (type fixnum pos))
                (update-cell pos (edge-neighbor-count row col)))))))
      (replace world next-world))
    changed-cells))

(defun step-world-full-frame (world &optional next-world)
  "Updates WORLD by one generation and returns the next generation live count.
This avoids changed-cell tracking for renderers that redraw from the full world."
  (unless (typep world 'simple-bit-vector)
    (error "WORLD must be a simple bit vector."))
  (let* ((world-size (cl:length world))
         (dimension (isqrt world-size))
         (next-world (or next-world
                         (make-array world-size :element-type 'bit)))
         (live-count 0))
    (declare (optimize (speed 3) (safety 1) (debug 0))
             (type fixnum world-size dimension live-count))
    (unless (and (= world-size (* dimension dimension))
                 (typep next-world 'simple-bit-vector)
                 (eql world-size (array-total-size next-world)))
      (error "WORLD and NEXT-WORLD must be simple bit vectors with matching square sizes."))
    (let ((world (the simple-bit-vector world))
          (next-world (the simple-bit-vector next-world))
          (dimension (the fixnum dimension)))
      (declare (type simple-bit-vector world next-world)
               (type fixnum dimension))
      (labels ((edge-neighbor-count (row col)
                 (declare (type fixnum row col))
                 (let ((count 0))
                   (declare (type fixnum count))
                   (loop for neighbor-row fixnum from (1- row) to (1+ row) do
                     (when (and (>= neighbor-row 0) (< neighbor-row dimension))
                       (loop for neighbor-col fixnum from (1- col) to (1+ col) do
                         (when (and (>= neighbor-col 0)
                                    (< neighbor-col dimension)
                                    (not (and (= neighbor-row row)
                                              (= neighbor-col col))))
                           (incf count (sbit world (+ neighbor-col
                                                       (* neighbor-row dimension))))))))
                   count))
               (update-cell (pos neighbors)
                 (declare (type fixnum pos neighbors))
                 (let ((next-val (next-cell-state (sbit world pos) neighbors)))
                   (declare (type bit next-val))
                   (setf (sbit next-world pos) next-val)
                   (incf live-count next-val))))
        (declare (inline edge-neighbor-count update-cell))
        (loop for row fixnum from 1 below (1- dimension) do
          (let ((pos (+ 1 (* row dimension))))
            (declare (type fixnum pos))
            (loop for col fixnum from 1 below (1- dimension) do
              (update-cell pos
                           (+ (sbit world (- pos dimension 1))
                              (sbit world (- pos dimension))
                              (sbit world (- pos dimension -1))
                              (sbit world (1- pos))
                              (sbit world (1+ pos))
                              (sbit world (+ pos dimension -1))
                              (sbit world (+ pos dimension))
                              (sbit world (+ pos dimension 1))))
              (incf pos))))
        (dotimes (row dimension)
          (declare (type fixnum row))
          (dotimes (col dimension)
            (declare (type fixnum col))
            (when (or (= row 0)
                      (= col 0)
                      (= row (1- dimension))
                      (= col (1- dimension)))
              (let ((pos (+ col (* row dimension))))
                (declare (type fixnum pos))
                (update-cell pos (edge-neighbor-count row col)))))))
      (replace world next-world))
    live-count))

(defun update-world (world)
  "Updates world by looping through each position and updating a line buffer accordingly."
  (step-world world))

(defun initialize-world (world function &optional coverage)
  "Initializes WORLD with FUNCTION. COVERAGE is passed when supplied."
  (if coverage
      (funcall function world coverage)
      (funcall function world)))

(defun live-cell-count (world)
  "Returns the number of live cells in WORLD."
  (loop for pos below (array-total-size world)
        count (not (zerop (row-major-aref world pos)))))

(defun changed-cell-live-delta (world changed-cells)
  "Returns live-cell count delta represented by CHANGED-CELLS after WORLD changed."
  (let ((delta 0))
    (dotimes (i (length changed-cells) delta)
      (if (zerop (row-major-aref world (aref changed-cells i)))
          (decf delta)
          (incf delta)))))

(defstruct generation-update
  "Renderer-neutral description of one generation transition."
  generation
  live-count
  dimension
  changed-cells
  world
  direction)

(defstruct game-state
  "Core simulation state independent of any renderer."
  world
  next-world
  dimension
  generation
  live-count
  history
  history-size
  history-limit
  reusable-changed-cells)

(defun create-game-state (&key function (dimension 20)
                               coverage (history-limit 100)
                               (track-changes t))
  "Creates and initializes renderer-independent Game of Life state."
  (unless function
    (error "FUNCTION is required to initialize a game state."))
  (let* ((world-size (* dimension dimension))
         (world (make-array world-size
                            :element-type 'bit
                            :initial-element 0))
         (next-world (make-array world-size
                                 :element-type 'bit
                                 :initial-element 0)))
    (initialize-world world function coverage)
    (make-game-state :world world
                     :next-world next-world
                     :dimension dimension
                     :generation 1
                     :live-count (live-cell-count world)
                     :history nil
                     :history-size 0
                     :history-limit history-limit
                     :reusable-changed-cells
                     (cond ((not track-changes) :full-frame)
                           ((eql history-limit 0)
                            (make-array world-size
                                        :element-type 'fixnum
                                        :fill-pointer 0))))))

(defun remember-changed-cells (state changed-cells)
  "Stores CHANGED-CELLS in STATE history when history is enabled."
  (let ((history-limit (game-state-history-limit state)))
    (when (or (null history-limit) (> history-limit 0))
      (push changed-cells (game-state-history state))
      (incf (game-state-history-size state))
      (when (and history-limit
                 (> (game-state-history-size state) history-limit))
        (setf (cdr (nthcdr (1- history-limit)
                           (game-state-history state)))
              nil)
        (setf (game-state-history-size state) history-limit)))))

(defun make-update-from-state (state changed-cells direction)
  "Builds a renderer-neutral update from STATE."
  (make-generation-update :generation (game-state-generation state)
                          :live-count (game-state-live-count state)
                          :dimension (game-state-dimension state)
                          :changed-cells changed-cells
                          :world (game-state-world state)
                          :direction direction))

(defun advance-game-state (state)
  "Advances STATE by one generation and returns a GENERATION-UPDATE."
  (if (eq (game-state-reusable-changed-cells state) :full-frame)
      (progn
        (setf (game-state-live-count state)
              (step-world-full-frame (game-state-world state)
                                     (game-state-next-world state)))
        (incf (game-state-generation state))
        (make-update-from-state state nil :forward))
      (let ((changed-cells (step-world (game-state-world state)
                                       (game-state-next-world state)
                                       (game-state-reusable-changed-cells state))))
        (remember-changed-cells state changed-cells)
        (incf (game-state-live-count state)
              (changed-cell-live-delta (game-state-world state) changed-cells))
        (incf (game-state-generation state))
        (make-update-from-state state changed-cells :forward))))

(defun toggle-world-changes (world changed-cells)
  "Reverts WORLD by toggling the cells in CHANGED-CELLS."
  (dotimes (i (length changed-cells) changed-cells)
    (let ((pos (aref changed-cells i)))
      (setf (aref world pos) (if (zerop (aref world pos)) 1 0)))))

(defun previous-game-state (state)
  "Moves STATE back one generation when history is available.
Returns a GENERATION-UPDATE, or NIL when there is no previous generation."
  (when (game-state-history state)
    (let ((changed-cells (pop (game-state-history state))))
      (decf (game-state-history-size state))
      (toggle-world-changes (game-state-world state) changed-cells)
      (incf (game-state-live-count state)
            (changed-cell-live-delta (game-state-world state) changed-cells))
      (when (> (game-state-generation state) 1)
        (decf (game-state-generation state)))
      (make-update-from-state state changed-cells :backward))))

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

; http://large.stanford.edu/diversions/life/rules/
; https://en.wikipedia.org/wiki/Conway's_Game_of_Life
; https://www.jagregory.com/abrash-black-book/#chapter-17-the-game-of-life
; https://stackoverflow.com/questions/30552102/optimizing-neighbor-count-function-for-conways-game-of-life-in-c
