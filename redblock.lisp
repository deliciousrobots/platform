(ql:quickload :platform)

(in-package :cl-user)
(defpackage redblock
  (:use :cl :platform))
(in-package :redblock)

(defclass redblock (game) ((x :initform 320) (y :initform 240)))

(defmethod step-game ((game redblock) keys-down)
  (with-slots (x y) game
    (dolist (key keys-down)
      (case key
        (:up (decf y))
        (:down (incf y))
        (:left (decf x))
        (:right (incf x))))))

(defmethod draw-game ((game redblock))
  (with-slots (x y) game
    (sdl:draw-box (sdl:rectangle-from-edges-* x y (+ 20 x) (+ 20 y))
                  :color sdl:*red*)))

(game-loop 'redblock)
