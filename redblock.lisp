(asdf:load-system :platform)

(in-package :platform)

(defclass redblock (game) ((x :initform 320 :accessor x)
                           (y :initform 240 :accessor y)))

(defmethod step-game ((game redblock) keys-down)
  (when (member :up keys-down) (decf (y game)))
  (when (member :down keys-down) (incf (y game)))
  (when (member :left keys-down) (decf (x game)))
  (when (member :right keys-down) (incf (x game))))

(defmethod draw-game ((game redblock))
  (sdl:draw-box (sdl:rectangle-from-edges-*
                  (x game) (y game) (+ 20 (x game)) (+ 20 (y game)))
                  :color sdl:*red*))

(game-loop 'redblock)
