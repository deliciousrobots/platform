#|
  This file is a part of platform project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

#|
  Author: Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage platform-example-asd
  (:use :cl :asdf))
(in-package :platform-example-asd)

(defsystem platform-example
  :version "0.1"
  :author "Stephen A. Goss"
  :license "Modified BSD"
  :depends-on (:platform)
  :components ((:module "src"
                :components
                ((:file "example")))))

