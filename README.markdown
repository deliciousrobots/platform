# Platform

A Very Simple Game Platform built on lispbuilder-sdl.

## Usage

This software is in ALPHA status under heavy development. You are free
to use it however you want given the liberal open source licensing.
However, THE API WILL CHANGE, YOU HAVE BEEN WARNED.

Subclass GAME and implement at least STEP-GAME and DRAW-GAME methods.

Example game that is a red block that moves around with arrow buttons.

```common-lisp
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
```

## Installation

Put this repo in your ~/quicklisp/local-projects folder.

## Author

* Stephen A. Goss (steveth45@gmail.com)

## Copyright

Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)

# License

Licensed under the Modified BSD License.

