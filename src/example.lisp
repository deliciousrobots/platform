
(in-package :cl-user)
(defpackage platform-example
  (:use :cl :platform))
(in-package :platform-example)

(declaim (optimize (debug 3)))

;; interfaces
(defgeneric draw (obj xform))

(defgeneric apply-xform (vector xform))

(defgeneric add (obj1 obj2))

;; get some class
(defclass vector-2d ()
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)))

(defun vector-2d (x y)
  (make-instance 'vector-2d :x x :y y))

(defclass xform-2d ()
  ((offset :initform (vector-2d 0 0)
           :initarg :offset :accessor offset)
   (scale :initform (vector-2d 1 1)
           :initarg :scale :accessor scale)))

(defclass obj-2d ()
  ((location :initform (make-instance 'vector-2d)
     :initarg :location :accessor location)))

(defclass has-color ()
  ((color :initform sdl:*green* :initarg :color :accessor color)))

(defclass has-velocity ()
  ((velocity :initform (make-instance 'vector-2d)
             :initarg :velocity :accessor velocity)))

(defclass obj-rect (obj-2d)
  ((dimensions :initform (make-instance 'vector-2d)
               :initarg :dimensions :accessor dimensions)))

(defclass obj-circle (obj-2d)
  ((radius :initform 1 :initarg :radius :accessor radius)))

(defclass color-rect (obj-rect has-color) ())

(defclass brick (color-rect) ())

(defclass paddle (color-rect) ())

(defclass ball (obj-circle has-color has-velocity) ())

(defmethod add ((obj1 vector-2d) (obj2 vector-2d))
  (vector-2d (+ (x obj1) (x obj2)) (+ (y obj1) (y obj2))))

(defmethod apply-xform ((vector vector-2d) (xform xform-2d))
  (let
    ((offset-vec (offset xform))
     (scale-vec (scale xform)))
      (vector-2d (+ (* (x vector) (x scale-vec)) (x offset-vec))
                 (+ (* (y vector) (y scale-vec)) (y offset-vec)))))

(defmethod draw ((obj color-rect) (xform xform-2d))
  (let ((loc (apply-xform (location obj) xform))
        (corner (apply-xform (add (location obj) (dimensions obj))
                             xform)))
    (sdl:draw-box
      (sdl:rectangle-from-edges-* (x loc) (y loc) (x corner) (y corner))
      :color (color obj))))

(defclass breaker (game)
  ((bricks :accessor bricks)
   (xform :accessor xform)
   (field-offset :accessor field-offset)
   (field-dim :accessor field-dim)
   (paddle :accessor paddle)
   (balls :accessor balls)))

(defun make-brick-grid (grid-width grid-height brick-dim offset)
  (let ((bricks nil))
    (dotimes (row grid-height)
      (dotimes (col grid-width)
        (push (make-instance 'brick
                             :dimensions brick-dim
                             :location (add offset
                                            (vector-2d
                                              (* col (x brick-dim))
                                              (* row (y brick-dim))))
                             :color (sdl:color :r (random 255)
                                               :g (random 255)
                                               :b (random 255)))
          bricks)))
    bricks))

(defmethod init-game ((game breaker))
  (setf (field-offset game) (vector-2d 20 20))
  (setf (field-dim game) (vector-2d 600 440))
  (setf (xform game)
        (make-instance 'xform-2d
                       :offset (field-offset game)))
  (let ((ideal-width (floor (x (field-dim game)) 12)))
    (setf (bricks game)
          (make-brick-grid 10 10
                          (vector-2d ideal-width 20)
                          (vector-2d ideal-width 20))))
  (setf (paddle game)
        (make-instance 'paddle
                       :location (vector-2d 0 400)
                       :dimensions (vector-2d 50 10)
                       :color sdl:*white*))
  (setf (balls game) nil))

(defmethod step-game ((game breaker) keys-down)
  (dolist (key keys-down)
    (case key
      (:left)
      (:right))))

(defmethod draw-game ((game breaker))
  (dolist (brick (bricks game))
    (draw brick (xform game)))
  (sdl:draw-rectangle
    (sdl:rectangle-from-edges-*
      (x (field-offset game))
      (y (field-offset game))
      (+ (x (field-offset game)) (x (field-dim game)))
      (+ (y (field-offset game)) (y (field-dim game))))
    :color sdl:*green*)
  (draw (paddle game) (xform game)))

;; (game-loop 'breaker)
