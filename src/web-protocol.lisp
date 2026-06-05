(in-package :conways-game-of-life)

(defconstant +web-frame-header-size+ 14)

(defun write-u16-be (buffer offset value)
  "Writes VALUE as an unsigned 16-bit big-endian integer."
  (setf (aref buffer offset) (ldb (byte 8 8) value)
        (aref buffer (1+ offset)) (ldb (byte 8 0) value)))

(defun write-u32-be (buffer offset value)
  "Writes VALUE as an unsigned 32-bit big-endian integer."
  (setf (aref buffer offset) (ldb (byte 8 24) value)
        (aref buffer (+ offset 1)) (ldb (byte 8 16) value)
        (aref buffer (+ offset 2)) (ldb (byte 8 8) value)
        (aref buffer (+ offset 3)) (ldb (byte 8 0) value)))

(defun pack-world-frame (world dimension generation live-count)
  "Packs WORLD into a binary frame for the web canvas renderer.
Frame format:
  bytes 0-3   magic CGL1
  bytes 4-7   generation, unsigned 32-bit big-endian
  bytes 8-11  live count, unsigned 32-bit big-endian
  bytes 12-13 dimension, unsigned 16-bit big-endian
  bytes 14+   row-major packed world bits, most-significant bit first"
  (let* ((world-size (array-total-size world))
         (payload-size (ceiling world-size 8))
         (frame (make-array (+ +web-frame-header-size+ payload-size)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (setf (aref frame 0) (char-code #\C)
          (aref frame 1) (char-code #\G)
          (aref frame 2) (char-code #\L)
          (aref frame 3) (char-code #\1))
    (write-u32-be frame 4 generation)
    (write-u32-be frame 8 live-count)
    (write-u16-be frame 12 dimension)
    (dotimes (pos world-size frame)
      (when (= (row-major-aref world pos) 1)
        (let ((byte-index (+ +web-frame-header-size+ (floor pos 8)))
              (bit-index (- 7 (mod pos 8))))
          (setf (aref frame byte-index)
                (logior (aref frame byte-index)
                        (ash 1 bit-index))))))))
