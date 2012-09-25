# Platform

A Very Simple Game Platform built on lispbuilder-sdl.

## Usage

Subclass GAME and implement at least STEP-GAME and DRAW-GAME methods.

Example game that is a red block that moves around with arrow buttons.

```common-lisp
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
```

## Installation

Put this repo in your ~/quicklisp/local-projects folder.

## Author

* Stephen A. Goss (steveth45@gmail.com)

## Copyright

Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)

# License

Licensed under the Modified BSD License.

