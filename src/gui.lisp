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

(defun int-coords (index &optional (maxx 20))
  "Returns 2-D coordinates given a linear coordinate"
  (list (mod index maxx) (floor (/ index maxx))))

(defun grid-coords (index &optional (maxx 20) (scale 25) &aux (coords (int-coords index maxx)))
  "Returns screen coordinates given a linear coordinate"
  (mapcar #'1+
          (list (* (car coords) scale) (* (cadr coords) scale)
                (* (1+ (car coords)) scale) (* (1+ (cadr coords)) scale))))

(defun create-grid (canvas world length scale)
  "Draws a new grid representing the world using rectangles on the screen"
  (loop for i from 0 to (- (array-total-size world) 1)
        collect (apply #'create-rectangle canvas (grid-coords i length scale))))

(defun draw-world (canvas grid prev-world world)
  "Recolors the grid to reflect life values in each cell"
  (loop for i from 0 to (- (array-total-size world) 1)
        do (if (not (eql (row-major-aref world i) (row-major-aref prev-world i)))
               (itemconfigure canvas (nth i grid) :fill (if (zerop (row-major-aref world i)) 'gray85 'black)))))

(defun start (&key (function #'init-random-pattern) (dimension 20) (speed 4))
  "Creates a world and iterates through the progressions of generations using Ltk."
  (with-ltk ()
    (let* ((title nil)
           (draw-generation t)
           (generation-count 1)
           (display-size (screen-width-mm))
           (c (make-instance 'canvas :height display-size :width display-size))
           (world (make-array (list dimension dimension) :element-type 'bit))
           (grid (create-grid c world dimension (/ display-size dimension)))
           (prev-world (make-array (list dimension dimension) :element-type 'bit)))
      (labels ((perform-generation ()
                 (incf generation-count)
                 (setf title (format nil "Conways's Game of Life Generation ~a" generation-count))
                 (wm-title *tk* title)
                 (setf prev-world (copy-array world))
                 (game-of-life world 1 0 nil nil)
                 (draw-world c grid prev-world world)))
        ;; Pause, will stop drawing progression of iterations
        (bind c "<KeyPress-p>"
              (lambda (evt) (declare (ignore evt))
                (setf draw-generation nil)))
        ;; Start, will start drawing progression of iterations
        (bind c "<KeyPress-s>"
              (lambda (evt) (declare (ignore evt))
                (setf draw-generation t)))
        (bind c "<KeyPress-n>"
              (lambda (evt) (declare (ignore evt))
                (setf draw-generation nil)
                (perform-generation)))
        (setf title "Conway's Game of Life Generation 1")
        (pack c)
        (force-focus c)
        ;; Initializes the world based on the provided initaliation function
        (funcall function world)
        (wm-title *tk* title)
        (draw-world c grid prev-world world)
        (loop do
          ;; process events and perform 1 generation of growth
          (process-events)
          (if draw-generation
              (perform-generation))
          (sleep (/ 1 speed))))
      t)))
