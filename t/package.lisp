#|
  This file is a part of inlined-generic-function project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :inlined-generic-function.test
  (:use ;; :cl
        :closer-common-lisp
        :inlined-generic-function
        :inlined-generic-function.impl
        :fiveam
        :trivia :alexandria :iterate))
(in-package :inlined-generic-function.test)



(def-suite :inlined-generic-function)
(in-suite :inlined-generic-function)

;; run test with (run! test-name) 

;; (trace make-method-lambda
;;        ensure-generic-function)

(push :inline-generic-function *features*)

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
                   (trace compute-effective-method)
                   (plus 1 2)
                   (untrace compute-effective-method))))
        (is (= 3 (plus 1 2)))
        (is (= 3 (plus 1 2)))))

(defgeneric my-first (a)
  (:generic-function-class inlined-generic-function))
(defmethod my-first ((a list))
  (car a))
(defmethod my-first ((a vector))
  (aref a 0))

(test reader
  (let ((m (first (generic-function-methods #'my-first))))
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
    (let ((a (list 1 2)))
      (is (= 1 (my-first a)))
      (is (= 1 (my-first a)))
      (is (= 1 (funcall #'my-first a)))
      (is (= 1 (apply #'my-first (list a)))))))


(defgeneric (setf my-first) (newval a)
  (:generic-function-class inlined-generic-function))
(defmethod (setf my-first) (newval (a list))
  (setf (car a) newval))
(defmethod (setf my-first) (newval (a vector))
  (setf (aref a 0) newval))

(test writer
  (let ((m (first (generic-function-methods #'(setf my-first)))))
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
    (let ((a (list 1 2)))
      (is (= 1 (my-first a)))
      (finishes
        (setf (my-first a) 3))
      (is (= 3 (my-first a)))
      (funcall #'(setf my-first) 4 a)
      (is (= 4 (my-first a)))
      (apply #'(setf my-first) (list 5 a))
      (is (= 5 (my-first a))))))
