#|
  This file is a part of inlined-generic-function project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :inlined-generic-function.test
  (:use :cl
        :inlined-generic-function
        :fiveam
        :trivia :closer-mop :alexandria :iterate))
(in-package :inlined-generic-function.test)



(def-suite :inlined-generic-function)
(in-suite :inlined-generic-function)

;; run test with (run! test-name) 

(test inlined-generic-function

  )



