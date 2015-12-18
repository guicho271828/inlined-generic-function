#|
  This file is a part of inlined-generic-function project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :inlined-generic-function.test
  (:use :closer-common-lisp
        :inlined-generic-function
        :fiveam
        :trivia :alexandria :iterate))
(in-package :inlined-generic-function.test)



(def-suite :inlined-generic-function)
(in-suite :inlined-generic-function)

;; run test with (run! test-name) 

(trace make-method-lambda)

(test inlined-generic-function
      (defgeneric plus (a b)
        (:generic-function-class inlined-generic-function))
      (defmethod plus ((a fixnum) (b fixnum))
        (+ a b))
      (let ((m (first (generic-function-methods #'plus))))
        (is (typep m 'inlined-method))
        (is-true
         (ignore-errors
           (method-lambda-expression m))
         "~a" (with-output-to-string (s)
                (describe m s)))))



