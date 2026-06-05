(defpackage conways-game-of-life
  (:use :cl
        :ltk
        :cl-progress-bar)
  (:export #:copy-array #:start #:square-p #:world-length #:access-world
     #:set-world #:copy-world-row #:insert-row #:row-equal
     #:insert-row #:world-equal #:print-world #:active-neighbors
     #:update-position #:step-world #:update-world #:clear-world #:game-of-life
     #:game-state #:game-state-world #:game-state-generation #:game-state-live-count
     #:generation-update #:generation-update-generation #:generation-update-live-count
     #:generation-update-dimension #:generation-update-changed-cells
     #:generation-update-world #:generation-update-direction
     #:initialize-world #:live-cell-count #:changed-cell-live-delta
     #:create-game-state #:advance-game-state #:previous-game-state
     #:renderer-initialize #:renderer-apply-update #:renderer-close
     #:pack-world-frame #:start-web #:stop-web
     #:init-glider-pattern #:init-c-pattern #:init-pi-heptomino-pattern
     #:init-u-pattern #:init-pentadecathlon-on-snacker-pattern
     #:init-random-pattern))
(in-package :conways-game-of-life)

(defgeneric renderer-initialize (renderer state)
  (:documentation "Initializes RENDERER from renderer-independent game STATE."))

(defgeneric renderer-apply-update (renderer update)
  (:documentation "Applies a renderer-neutral GENERATION-UPDATE to RENDERER."))

(defgeneric renderer-close (renderer)
  (:documentation "Closes or releases RENDERER resources."))
