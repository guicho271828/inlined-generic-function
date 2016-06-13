#|
  This file is a part of inlined-generic-function project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage inlined-generic-function.test-asd
  (:use :cl :asdf))
(in-package :inlined-generic-function.test-asd)


(defsystem inlined-generic-function.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of inlined-generic-function"
  :license "LLGPL"
  :depends-on (:inlined-generic-function
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c)
(eval
 (read-from-string
  "(let ((res (5am:run :inlined-generic-function)))
     (5am:explain! res)
     (every #'5am::TEST-PASSED-P res))"))))
