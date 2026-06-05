(in-package :conways-game-of-life)

(defun int-coords (index &optional (maxx 20))
  "Returns 2-D coordinates given a linear coordinate"
  (list (mod index maxx) (floor (/ index maxx))))

(defun grid-coords (index &optional (maxx 20) (x-scale 25) (y-scale x-scale)
                    &aux (coords (int-coords index maxx)))
  "Returns screen coordinates given a linear coordinate"
  (list (* (car coords) x-scale) (* (cadr coords) y-scale)
        (* (1+ (car coords)) x-scale) (* (1+ (cadr coords)) y-scale)))

(defun live-cell-coords (index length x-scale y-scale)
  "Returns inset rectangle coordinates for a live cell."
  (destructuring-bind (x1 y1 x2 y2) (grid-coords index length x-scale y-scale)
    (let ((x-inset (if (> x-scale 2) 1/2 0))
          (y-inset (if (> y-scale 2) 1/2 0)))
      (list (+ x1 x-inset) (+ y1 y-inset)
            (- x2 x-inset) (- y2 y-inset)))))

(defun cell-tag (index)
  "Returns the stable canvas tag for a cell."
  (format nil "cell-~a" index))

(defun create-live-cell (canvas index length x-scale y-scale)
  "Creates a canvas rectangle for a live cell and returns its canvas tag."
  (destructuring-bind (x1 y1 x2 y2) (live-cell-coords index length x-scale y-scale)
    (let ((tag (cell-tag index)))
      (format-wish "~a create rectangle ~a ~a ~a ~a -fill black -outline {} -width 0 -tags ~a"
                   (widget-path canvas)
                   (ltk::tk-number x1)
                   (ltk::tk-number y1)
                   (ltk::tk-number x2)
                   (ltk::tk-number y2)
                   tag)
      tag)))

(defun delete-live-cell (canvas tag)
  "Deletes a live-cell canvas rectangle by tag."
  (format-wish "~a delete ~a"
                 (widget-path canvas)
                 tag))

(defun create-grid-lines (canvas dimension display-width display-height x-scale y-scale)
  "Draws cell boundary lines without creating dead-cell rectangles."
  (dotimes (i (1+ dimension))
    (let ((x (* i x-scale))
          (y (* i y-scale)))
      (format-wish "~a create line ~a 0 ~a ~a -fill gray70 -width 1 -tags grid-lines"
                   (widget-path canvas)
                   (ltk::tk-number x)
                   (ltk::tk-number x)
                   (ltk::tk-number display-height))
      (format-wish "~a create line 0 ~a ~a ~a -fill gray70 -width 1 -tags grid-lines"
                   (widget-path canvas)
                   (ltk::tk-number y)
                   (ltk::tk-number display-width)
                   (ltk::tk-number y)))))

(defun create-grid (canvas world length x-scale y-scale)
  "Creates canvas rectangles for live cells and stores nil for dead cells."
  (let ((grid (make-array (array-total-size world) :initial-element nil))
        (canvas-path (widget-path canvas))
        (batch-size 250)
        (batch-count 0))
    (labels ((flush-batch (commands)
               (let ((command-string (get-output-stream-string commands)))
                 (when (> (length command-string) 0)
                   (format-wish "~a" command-string)
                   (ltk::flush-wish)))))
      (let ((commands (make-string-output-stream)))
        (unwind-protect
             (progn
               (dotimes (i (array-total-size world))
                 (when (not (zerop (row-major-aref world i)))
                   (let ((tag (cell-tag i)))
                     (setf (aref grid i) tag)
                     (destructuring-bind (x1 y1 x2 y2)
                         (live-cell-coords i length x-scale y-scale)
                       (format commands
                               "~a create rectangle ~a ~a ~a ~a -fill black -outline {} -width 0 -tags ~a~%"
                               canvas-path
                               (ltk::tk-number x1)
                               (ltk::tk-number y1)
                               (ltk::tk-number x2)
                               (ltk::tk-number y2)
                               tag))
                     (incf batch-count)
                     (when (>= batch-count batch-size)
                       (flush-batch commands)
                       (setf batch-count 0)))))
               (flush-batch commands))
          (close commands))))
    grid))

(defun display-dimensions (display-size)
  "Returns display width and height. Nil means use full screen size."
  (cond ((null display-size)
         (values (screen-width) (screen-height)))
        ((numberp display-size)
         (values display-size display-size))
        ((and (listp display-size)
              (numberp (first display-size))
              (numberp (second display-size)))
         (values (first display-size) (second display-size)))
        (t (error "DISPLAY-SIZE must be NIL, a number, or a list of (width height)."))))

(defun centered-window-position (display-width display-height)
  "Returns screen coordinates that center a window of DISPLAY-WIDTH by DISPLAY-HEIGHT."
  (values (max 0 (round (- (screen-width) display-width) 2))
          (max 0 (round (- (screen-height) display-height) 2))))

(defun generation-title (generation-count live-count)
  "Returns the window title for the current generation and live-cell count."
  (format nil "Conways's Game of Life Generation ~a - ~a live cells"
          generation-count
          live-count))

(defclass tk-renderer ()
  ((canvas :initarg :canvas :reader tk-renderer-canvas)
   (dimension :initarg :dimension :reader tk-renderer-dimension)
   (display-width :initarg :display-width :reader tk-renderer-display-width)
   (display-height :initarg :display-height :reader tk-renderer-display-height)
   (x-scale :initarg :x-scale :reader tk-renderer-x-scale)
   (y-scale :initarg :y-scale :reader tk-renderer-y-scale)
   (validate-render :initarg :validate-render :reader tk-renderer-validate-render)
   (grid :accessor tk-renderer-grid)
   (title :accessor tk-renderer-title)))

(defun draw-changes (canvas grid world changed-cells length x-scale y-scale)
  "Recolors only the cells that changed in the latest generation."
  (let ((births 0)
        (deaths 0)
        (canvas-path (widget-path canvas))
        (batch-size 250))
    (dotimes (i (length changed-cells))
      (let ((pos (aref changed-cells i)))
        (cond ((zerop (row-major-aref world pos))
               (incf deaths)
               (let ((item (aref grid pos)))
                 (when item
                   (setf (aref grid pos) nil))))
              ((not (aref grid pos))
               (incf births)
               (setf (aref grid pos)
                     (cell-tag pos))))))
    (when (> deaths 0)
      (labels ((flush-batch (commands)
                 (let ((command-string (get-output-stream-string commands)))
                   (when (> (length command-string) 0)
                     (format-wish "~a" command-string)
                     (ltk::flush-wish)))))
        (let ((commands (make-string-output-stream))
              (tag-count 0))
          (unwind-protect
               (progn
                 (dotimes (i (length changed-cells))
                   (let ((pos (aref changed-cells i)))
                     (when (zerop (row-major-aref world pos))
                       (let ((tag (cell-tag pos)))
                         (when (zerop tag-count)
                           (format commands "~a delete" canvas-path))
                         (format commands " ~a" tag)
                         (incf tag-count)
                         (when (>= tag-count batch-size)
                           (format commands "~%")
                           (flush-batch commands)
                           (setf tag-count 0))))))
                 (when (> tag-count 0)
                   (format commands "~%"))
                 (flush-batch commands))
            (close commands)))))
    (when (> births 0)
      (labels ((flush-batch (commands)
                 (let ((command-string (get-output-stream-string commands)))
                   (when (> (length command-string) 0)
                     (format-wish "~a" command-string)
                     (ltk::flush-wish)))))
        (let ((commands (make-string-output-stream))
              (batch-count 0))
          (unwind-protect
               (progn
                 (dotimes (i (length changed-cells))
                   (let ((pos (aref changed-cells i)))
                     (when (and (not (zerop (row-major-aref world pos)))
                                (aref grid pos))
                       (destructuring-bind (x1 y1 x2 y2)
                           (live-cell-coords pos length x-scale y-scale)
                         (format commands
                                 "~a create rectangle ~a ~a ~a ~a -fill black -outline {} -width 0 -tags ~a~%"
                                 canvas-path
                                 (ltk::tk-number x1)
                                 (ltk::tk-number y1)
                                 (ltk::tk-number x2)
                                 (ltk::tk-number y2)
                                 (aref grid pos)))
                       (incf batch-count)
                       (when (>= batch-count batch-size)
                         (flush-batch commands)
                         (setf batch-count 0)))))
                 (flush-batch commands))
            (close commands)))))
    (if (> births 0)
        (itemraise canvas "grid-lines"))
    (values births deaths)))

(defun validate-render-state (grid world generation-count)
  "Signals an error if GRID live cells differ from WORLD live cells."
  (dotimes (pos (array-total-size world) t)
    (let ((render-live (not (null (aref grid pos))))
          (world-live (not (zerop (row-major-aref world pos)))))
      (if (not (eql render-live world-live))
          (error "Render state mismatch at generation ~a, index ~a. Render live: ~a, world live: ~a."
                 generation-count pos render-live world-live)))))

(defmethod renderer-initialize ((renderer tk-renderer) state)
  "Initializes the Tk renderer from game STATE."
  (let ((canvas (tk-renderer-canvas renderer)))
    (with-atomic
      (configure canvas :background 'gray85)
      (setf (tk-renderer-grid renderer)
            (create-grid canvas
                         (game-state-world state)
                         (tk-renderer-dimension renderer)
                         (tk-renderer-x-scale renderer)
                         (tk-renderer-y-scale renderer)))
      (create-grid-lines canvas
                         (tk-renderer-dimension renderer)
                         (tk-renderer-display-width renderer)
                         (tk-renderer-display-height renderer)
                         (tk-renderer-x-scale renderer)
                         (tk-renderer-y-scale renderer))
      (setf (tk-renderer-title renderer)
            (generation-title (game-state-generation state)
                              (game-state-live-count state)))
      (multiple-value-bind (window-x window-y)
          (centered-window-position (tk-renderer-display-width renderer)
                                    (tk-renderer-display-height renderer))
        (set-geometry *tk*
                      (tk-renderer-display-width renderer)
                      (tk-renderer-display-height renderer)
                      window-x window-y))
      (pack canvas)
      (force-focus canvas)
      (wm-title *tk* (tk-renderer-title renderer))
      (format-wish "update idletasks"))
    (when (tk-renderer-validate-render renderer)
      (validate-render-state (tk-renderer-grid renderer)
                             (game-state-world state)
                             (game-state-generation state)))
    renderer))

(defmethod renderer-apply-update ((renderer tk-renderer) update)
  "Applies a generation update to the Tk renderer."
  (let ((canvas (tk-renderer-canvas renderer)))
    (with-atomic
      (draw-changes canvas
                    (tk-renderer-grid renderer)
                    (generation-update-world update)
                    (generation-update-changed-cells update)
                    (generation-update-dimension update)
                    (tk-renderer-x-scale renderer)
                    (tk-renderer-y-scale renderer))
      (setf (tk-renderer-title renderer)
            (generation-title (generation-update-generation update)
                              (generation-update-live-count update)))
      (wm-title *tk* (tk-renderer-title renderer))
      (format-wish "update idletasks"))
    (when (tk-renderer-validate-render renderer)
      (validate-render-state (tk-renderer-grid renderer)
                             (generation-update-world update)
                             (generation-update-generation update)))
    renderer))

(defmethod renderer-close ((renderer tk-renderer))
  "Requests Ltk mainloop shutdown for the Tk renderer."
  (declare (ignore renderer))
  (setf *exit-mainloop* t))

(defun shutdown-condition-p (condition)
  "Returns true for Tk/Ltk shutdown races that can happen while closing."
  (let ((message (format nil "~a" condition)))
    (or (search "broken pipe" message :test #'char-equal)
        (and (search "not of type" message :test #'char-equal)
             (search "STREAM" message :test #'char-equal)))))

(defun start (&key (function #'init-random-pattern) (dimension 20) (speed 4)
                    display-size (coverage 0.25) (running t) (history-limit 100)
                    validate-render)
  "Creates a world and iterates through the progressions of generations using Ltk.
DISPLAY-SIZE may be nil for full screen, a number for a square window,
or a list of (width height). COVERAGE controls the default random initializer.
RUNNING controls whether generations advance immediately after startup.
HISTORY-LIMIT controls how many previous generations can be restored with b.
VALIDATE-RENDER checks sparse render state against world state after updates."
  (with-ltk ()
    (multiple-value-bind (display-width display-height)
        (display-dimensions display-size)
      (let* ((draw-generation running)
             (c (make-instance 'canvas :height display-height :width display-width))
             (state (create-game-state :function function
                                       :dimension dimension
                                       :coverage (and (or (eq function 'init-random-pattern)
                                                          (eq function #'init-random-pattern))
                                                      coverage)
                                       :history-limit history-limit))
             (renderer (make-instance 'tk-renderer
                                      :canvas c
                                      :dimension dimension
                                      :display-width display-width
                                      :display-height display-height
                                      :x-scale (/ display-width dimension)
                                      :y-scale (/ display-height dimension)
                                      :validate-render validate-render))
             (quit-requested nil)
             (tick-ms (max 1 (round (/ 1000 speed)))))
        (renderer-initialize renderer state)
        (let ()
          (labels ((perform-generation ()
                     (let ((update (advance-game-state state)))
                       (renderer-apply-update renderer update)))
                   (request-quit ()
                     (setf draw-generation nil)
                     (setf quit-requested t)
                     (renderer-close renderer))
                   (handle-shutdown-condition (condition)
                     (if (shutdown-condition-p condition)
                         (request-quit)
                         (error condition)))
                   (perform-previous-generation ()
                     (let ((update (previous-game-state state)))
                       (when update
                         (renderer-apply-update renderer update))))
                   (perform-tick ()
                     (unless quit-requested
                       (handler-case
                           (when draw-generation
                             (perform-generation))
                         (stream-error (condition)
                           (handle-shutdown-condition condition))
                         (type-error (condition)
                           (handle-shutdown-condition condition))
                         (ltk::tk-error (condition)
                           (handle-shutdown-condition condition)))
                       (unless quit-requested
                         (after tick-ms #'perform-tick)))))
            (on-close *tk* #'request-quit)
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
            (bind c "<KeyPress-b>"
                  (lambda (evt) (declare (ignore evt))
                    (setf draw-generation nil)
                    (perform-previous-generation)))
            (bind c "<KeyPress-q>"
                  (lambda (evt) (declare (ignore evt))
                    (request-quit)))
            (after tick-ms #'perform-tick))
          t)))))
