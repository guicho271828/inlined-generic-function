#|
  This file is a part of inlined-generic-function project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#
;;; setup
(in-package :cl-user)
(defpackage :inlined-generic-function.test
  (:use ;; :cl
        :closer-common-lisp
        :inlined-generic-function
        :inlined-generic-function.impl
        :fiveam
        :trivia :alexandria :iterate))
(in-package :inlined-generic-function.test)

;; #+sbcl
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (declaim (sb-ext:muffle-conditions style-warning)))

(def-suite :inlined-generic-function)
(in-suite :inlined-generic-function)

;; run test with (run! test-name) 

;; (trace make-method-lambda
;;        ensure-generic-function)

(push :inline-generic-function *features*)

;;; basic

(defgeneric normal-plus (a b))

(defmethod normal-plus :around ((a number) (b number))
  (format t "~&(normal-plus ~a ~a) is called~&" (type-of a) (type-of b))
  (call-next-method))
(defmethod normal-plus ((a fixnum) (b fixnum))
  (+ a b))
(defmethod normal-plus ((a float) (b float))
  (+ a b))


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
  (iter (for m in (generic-function-methods #'plus))
        (is (typep m 'inlined-method))
        (is-true
         (ignore-errors
           (prog1
             (print
              (method-lambda-expression m))
             (print
              (method-lambda-expression* m))
             (fresh-line)))
         (with-output-to-string (s)
           (format s "~&Failed to get the inlining form!~&")
           (describe m s))))
  (is (= 3 (prog2
             (trace compute-effective-method)
             (plus 1 2)
             (untrace compute-effective-method))))
  (is (= 3 (plus 1 2)))
  (is (= 3 (plus 1 2))))

;;; next

(defgeneric my-first (a)
  (:generic-function-class inlined-generic-function))
(defmethod my-first ((a list))
  (car a))
(defmethod my-first ((a vector))
  (aref a 0))

(test reader
  (iter (for m in (generic-function-methods #'my-first))
        (is (typep m 'inlined-method))
        (is-true
         (ignore-errors
           (prog1
             (print
              (method-lambda-expression m))
             (print
              (method-lambda-expression* m))
             (fresh-line)))
         (with-output-to-string (s)
           (format s "~&Failed to get the inlining form!~&")
           (describe m s))))
  (let ((a (list 1 2)))
    (is (= 1 (my-first a)))
    (is (= 1 (my-first a)))
    (is (= 1 (funcall #'my-first a)))
    (is (= 1 (apply #'my-first (list a))))))

;;; setf

(defgeneric (setf my-first) (newval a)
  (:generic-function-class inlined-generic-function))
(defmethod (setf my-first) (newval (a list))
  (setf (car a) newval))
(defmethod (setf my-first) (newval (a vector))
  (setf (aref a 0) newval))

(test writer
  (iter (for m in (generic-function-methods #'(setf my-first)))
        (is (typep m 'inlined-method))
        (is-true
         (ignore-errors
           (prog1
             (print
              (method-lambda-expression m))
             (print
              (method-lambda-expression* m))
             (fresh-line)))
         (with-output-to-string (s)
           (format s "~&Failed to get the inlining form!~&")
           (describe m s))))
  (let ((a (list 1 2)))
    (is (= 1 (my-first a)))
    (finishes
      (setf (my-first a) 3))
    (is (= 3 (my-first a)))
    (funcall #'(setf my-first) 4 a)
    (is (= 4 (my-first a)))
    (apply #'(setf my-first) (list 5 a))
    (is (= 5 (my-first a)))))

;;; basic

(defgeneric minus (a b)
  (:generic-function-class inlined-generic-function))

(defmethod minus :around ((a number) (b number))
  (print `(:around number ,a number ,b))
  (call-next-method))
(defmethod minus :around ((a fixnum) (b fixnum))
  (print `(:around fixnum ,a fixnum ,b))
  (call-next-method))

(defmethod minus ((a number) (b number))
  (- a b))
(defmethod minus ((a fixnum) (b fixnum))
  (- a b))
(defmethod minus ((a float) (b float))
  (- a b))

(defmethod minus ((a (eql :two)) (b (eql :one)))
  :one)

(test compiler
  (finishes
   (print (inline-generic-function '(minus 1 2) nil))))


;;; before and after methods

(defgeneric testgf (a)
  (:generic-function-class inlined-generic-function))
(defmethod testgf (a) (declare (ignorable a)) t)
(defmethod testgf :around (a) (declare (ignorable a)) :around (call-next-method))
(defmethod testgf :before (a) (declare (ignorable a)) :before)
(defmethod testgf :after (a)  (declare (ignorable a)) :after)
(defmethod testgf ((a fixnum)) (declare (ignorable a)) :fixnum)
(defmethod testgf :after ((a fixnum)) (declare (ignorable a))  :after)

(test before-after
  (finishes
   (let ((*features* (cons :inline-generic-function *features*)))
     (print (inline-generic-function '(testgf (1+ x))))))
  (is (eq t (testgf :aaa)))
  (is (eq t (testgf "string")))
  (is (eq t (testgf 0.0)))
  (is (eq :fixnum (testgf 5))))

;;; no-next-method

;; no primary method
(defgeneric ng1 (a)
  (:generic-function-class inlined-generic-function))

(defmethod ng1 :around ((a number)) (call-next-method))

;; no next primary method
(defgeneric ng2 (a)
  (:generic-function-class inlined-generic-function))

(defmethod ng2 ((a number)) (call-next-method))

;; call-next-method from before method
(defgeneric ng3 (a)
  (:generic-function-class inlined-generic-function))
(defmethod ng3 (a) :primary)
(defmethod ng3 :before (a) (call-next-method))

;; call-next-method from after method
(defgeneric ng4 (a)
  (:generic-function-class inlined-generic-function))
(defmethod ng4 (a) :primary)
(defmethod ng4 :after (a) (call-next-method))

(macrolet ((make-test (gf)
             `(test ,(symbolicate 'inline-error- gf)
                ;; Test if it signals a compile-time error when required method is missing
                (signals error
                  (print (inline-generic-function `(,',gf a) nil)))
                ;; Test if this compile-time error can be recovered by invoking no-next-method
                (finishes
                  (handler-bind ((simple-error (lambda (c) (continue c))))
                    (print (inline-generic-function `(,',gf a) nil)))))))
  ;; (make-test ng1)
  (make-test ng2)
  (make-test ng3)
  (make-test ng4))

;;; non-standard method combinations

(defmacro many ()
  `(progn
     ,@(iter outer
             (for comb in '(list progn + max min and or))
             (for inline = (symbolicate 'inline comb))
             (for std = (symbolicate 'std comb))
             (iter (for name in (list inline std))
                   (in outer
                       (appending
                        `((defgeneric ,name (a)
                            (:method-combination ,comb)
                            (:generic-function-class inlined-generic-function))
                          (defmethod ,name ,comb (a) 1)
                          (defmethod ,name ,comb ((a fixnum)) 2)
                          (defmethod ,name ,comb ((a float)) 3)))))
             (for func = (symbolicate 'func comb))
             (collecting
              `(defun ,func (a)
                 (,inline a)))
             (collecting
              `(test ,comb
                     (is (equal (,func "aaa")
                                (,std "aaa")))
                     (is (equal (,func :keyword)
                                (,std :keyword)))
                     (is (equal (,func 0)
                                (,std 0)))
                     (is (equal (,func 0.0)
                                (,std 0.0))))))))

(many)

