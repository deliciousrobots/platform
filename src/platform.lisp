#|
  This file is a part of platform project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage platform
  (:use :cl)
  (:export :game
           :init-game
           :input-map
           :settings 
           :step-game
           :draw-game
           :draw-string))
(in-package :platform)

;; utility functions

(defun aget (key alist &key default (test #'eq))
  (let ((result (assoc key alist :test test)))
    (if result
      (cdr result)
      default)))

;; generic game interface
(defclass game () ())

(defgeneric init-game (game)
  (:documentation "Initializes game."))

(defgeneric input-map (game)
  (:documentation "Return an alist. The keys are symbols representing the
                  various inputs that the game expects, and the values
                  are a list (possibly empty) of SDL symbols representing
                  the key presses that map to those inputs by default.
                  Example: '((:up :sdl-key-up :sdl-key-w)
                             (:down :sdl-key-down : sdl-key-s))"))

(defgeneric settings (game)
  (:documentation "Return an alist of optional settings."))

(defgeneric step-game (game keys-down)
  (:documentation "Step the game forward a single frame."))

(defgeneric draw-game (game)
  (:documentation "Draw the game on the default screen."))

;; helper functions
(defun draw-string (string x y &key (color sdl:*white*) (bg sdl:*black*))
  (sdl:draw-string-shaded-* string x y color bg))

;; default implementation
(defmethod init-game ((game game)))

(defmethod input-map ((game game))
  '((:up :sdl-key-up)
    (:down :sdl-key-down)
    (:left :sdl-key-left)
    (:right :sdl-key-right)
    (:b :sdl-key-z)
    (:a :sdl-key-x)
    (:select :sdl-key-ctrl)
    (:start :sdl-key-enter)))

(defmethod settings ((game game))
  '((:screen-width . 640)
    (:screen-height . 480)
    (:frame-rate . 60)
    (:full-screen . nil) ;; T, nil
    (:clear-screen . sdl:*black*) ;; color or nil to not clear screen
    ))

(defmethod step-game ((game game) keys-down))

(defmethod draw-game ((game game))
  (draw-string "Welcome To Platform" 20 20))

;; Internal machinations

(defun make-key-lookup-table (alist)
  (let ((table (make-hash-table)))
    (dolist (keymap alist)
      (destructuring-bind (name . sdlkeys) keymap
        (dolist (sdlkey sdlkeys)
          (setf (gethash sdlkey table) name))))
    table))

;; main game loop function

(defun game-loop (game-class)
  (let* ((game (make-instance game-class))
         (key-hash-table (make-key-lookup-table (input-map game)))
         (keys-down nil)
         (settings (settings game)))
    (sdl:with-init ()
      (sdl:initialise-default-font)
      (sdl:window (aget :screen-width settings :default 640)
                  (aget :screen-height settings :default 480)
                  :title-caption "platform")
      (setf (sdl:frame-rate) (aget :frame-rate settings :default 60))
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:key key)
          (case key
            (:sdl-key-escape (sdl:push-quit-event))
            (otherwise
              (multiple-value-bind
                (game-key found) (gethash key key-hash-table)
                (when found
                  (push game-key keys-down))))))
        (:key-up-event (:key key)
          (multiple-value-bind
            (game-key found) (gethash key key-hash-table)
            (when found
              (setf keys-down (remove game-key keys-down)))))
        (:idle ()
          ;; step game forward
          (step-game game keys-down)
          ;; Clear the display each game loop
          (sdl:clear-display sdl:*black*)
          ;; Draw game
          (draw-game game)
          ;; Redraw the display
          (sdl:update-display)
          )))))
