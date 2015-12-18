#|
  This file is a part of inlined-generic-function project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  MOP implementation of the fast inlinable generic functions dispatched in compile-time

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage inlined-generic-function-asd
  (:use :cl :asdf))
(in-package :inlined-generic-function-asd)


(defsystem inlined-generic-function
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia :closer-mop :alexandria :iterate)
  :components ((:module "src"
                :components
                ((:file "0.package"))))
  :description "MOP implementation of the fast inlinable generic functions dispatched in compile-time"
  :in-order-to ((test-op (load-op :inlined-generic-function.test))))
