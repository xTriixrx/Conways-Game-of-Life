(in-package :conways-game-of-life)

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

(defun init-pi-heptomino-pattern (world)
  "A centered pi-heptomino pattern."
  (let* ((size (world-length world))
         (start-row (and size (- (floor size 2) 1)))
         (start-col (and size (- (floor size 2) 1))))
    (if (and size (>= size 3))
        (progn
          (dotimes (row-offset 3)
            (setf (aref world (+ start-col (* size (+ start-row row-offset)))) 1)
            (setf (aref world (+ (+ start-col 2) (* size (+ start-row row-offset)))) 1))
          (setf (aref world (+ (+ start-col 1) (* size (+ start-row 2)))) 1)
          t)
        nil)))

(defun init-u-pattern (world)
  "Compatibility alias for INIT-PI-HEPTOMINO-PATTERN."
  (init-pi-heptomino-pattern world))

(defun init-pentadecathlon-on-snacker-pattern (world)
  "A centered pentadecathlon-on-snacker period-45 oscillator."
  (let* ((size (world-length world))
         (pattern-width 20)
         (pattern-height 17)
         (start-row (and size (floor (- size pattern-height) 2)))
         (start-col (and size (floor (- size pattern-width) 2)))
         (cells '((6 0) (11 0)
                  (4 1) (5 1) (7 1) (8 1) (9 1) (10 1) (12 1) (13 1)
                  (6 2) (11 2)
                  (0 6) (1 6) (18 6) (19 6)
                  (1 7) (18 7)
                  (1 8) (3 8) (16 8) (18 8)
                  (2 9) (3 9) (16 9) (17 9)
                  (7 10) (12 10)
                  (5 11) (6 11) (8 11) (9 11) (10 11) (11 11) (13 11) (14 11)
                  (7 12) (12 12)
                  (2 13) (3 13) (16 13) (17 13)
                  (1 14) (3 14) (16 14) (18 14)
                  (1 15) (18 15)
                  (0 16) (1 16) (18 16) (19 16))))
    (if (and size
             (>= size pattern-width)
             (>= size pattern-height))
        (progn
          (dolist (cell cells)
            (destructuring-bind (col row) cell
              (setf (aref world (+ start-col col (* size (+ start-row row)))) 1)))
          t)
        nil)))

(defun init-random-pattern (world &optional (coverage-percentage 0.25))
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
