#|
  This file is a part of platform project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage platform-test-asd
  (:use :cl :asdf))
(in-package :platform-test-asd)

(defsystem platform-test
  :author "Stephen A. Goss"
  :license "Modified BSD"
  :depends-on (:platform
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "platform"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
