
(in-package :cl-user)
(defpackage platform-example
  (:use :cl :platform))
(in-package :platform-example)

(declaim (optimize (debug 3) (speed 0)))

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

(defclass ball (obj-circle has-color has-velocity)
  ((attached :initform T :initarg :attached :accessor attached)))

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

(defmethod draw ((obj ball) (xform xform-2d))
  (let ((loc (apply-xform (location obj) xform))
        (rad (* (radius obj) (x (scale xform)))))
    (sdl:draw-filled-circle-* (x loc) (y loc) rad :color (color obj))))

;; parameters
(defparameter *paddle-speed* 8)
(defparameter *ball-radius* 5)
(defparameter *ball-init-velocity* (vector-2d 3 -3))

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

(defun do-collisions (ball game)
  ;; wall collisions
  (setf (location ball)
        (add (location ball)
             (velocity ball)))
  (let ((top (radius ball))
        (bottom (- (y (field-dim game)) (radius ball)))
        (left (radius ball))
        (right (- (x (field-dim game)) (radius ball)))
        (xpos (x (location ball)))
        (ypos (y (location ball)))
        (xvel (x (velocity ball)))
        (yvel (y (velocity ball))))
    (when (< xpos left)
      (let ((over (- left xpos)))
        (setf xpos (+ left over))
        (if (< xvel 0)
          (setf xvel (* -1 xvel)))))
    (when (> xpos right)
      (let ((over (- xpos right)))
        (setf xpos (- right over))
        (if (> xvel 0)
          (setf xvel (* -1 xvel)))))
    (when (< ypos top)
      (let ((over (- top ypos)))
        (setf ypos (+ top over))
        (if (< yvel 0)
          (setf yvel (* -1 yvel)))))
    (when (> ypos bottom)
      (let ((over (- ypos bottom)))
        (setf ypos (- bottom over))
        (if (> yvel 0)
          (setf yvel (* -1 yvel)))))
    (setf (x (location ball)) xpos)
    (setf (y (location ball)) ypos)
    (setf (x (velocity ball)) xvel)
    (setf (y (velocity ball)) yvel))
  ball)

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
  ;; update paddle
  (with-accessors ((x-paddle x)) (location (paddle game))
    (dolist (key keys-down)
      (case key
        (:left (decf x-paddle *paddle-speed*))
        (:right (incf x-paddle *paddle-speed*))
        (:up (when (count-if #'attached (balls game))
               (mapc (lambda (b)
                       (when (attached b)
                         (setf (attached b) nil)  
                         (setf (velocity b)
                               *ball-init-velocity*)))
                     (balls game))))
        ))
    (setf x-paddle (max x-paddle 0))
    (setf x-paddle (min x-paddle (- (x (field-dim game))
                                    (x (dimensions (paddle game)))))))
  ;; update balls
  (when (null (balls game))
    (push (make-instance 'ball :radius *ball-radius*) (balls game)))
  (setf (balls game)
        (mapcar (lambda (b)
                  (if (attached b)
                    (progn
                      (setf (location b)
                            (add (location (paddle game))
                                (vector-2d
                                  (floor (x (dimensions (paddle game))) 2)
                                  (* -1 (radius b)))))
                      b)
                    (do-collisions b game)
                    ))
                (balls game)))
  )

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
  (draw (paddle game) (xform game))
  (mapc (lambda (b) (draw b (xform game)))
        (balls game))
  )

;; (game-loop 'breaker)
