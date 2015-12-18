#|
  This file is a part of inlined-generic-function project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :inlined-generic-function.test
  (:use :closer-common-lisp
        :inlined-generic-function
        :inlined-generic-function.impl
        :fiveam
        :trivia :alexandria :iterate))
(in-package :inlined-generic-function.test)



(def-suite :inlined-generic-function)
(in-suite :inlined-generic-function)

;; run test with (run! test-name) 

(trace make-method-lambda
       ensure-generic-function)

(defgeneric plus (a b)
  (:generic-function-class inlined-generic-function))

(defmethod plus :around ((a number) (b number))
  (format t "~&(plus ~a ~a) is called~&" (type-of a) (type-of b))
  (call-next-method))
(defmethod plus ((a fixnum) (b fixnum))
  (+ a b))
(defmethod plus ((a float) (b float))
  (+ a b))

(test inlined-generic-function
      (let ((m (first (generic-function-methods #'plus))))
        (is (typep m 'inlined-method))
        (is-true
         (ignore-errors
           (prog1
             (print
              (method-lambda-expression m))
             (fresh-line)))
         (with-output-to-string (s)
           (format s "~&Failed to get the inlining form!~&")
           (describe m s)))
        (is (= 3 (prog2
                   (trace compute-effective-method
                          compute-discriminating-function)
                   (plus 1 2)
                   (untrace compute-effective-method
                            compute-discriminating-function))))))



