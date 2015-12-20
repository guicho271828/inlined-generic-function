#|
  This file is a part of inlined-generic-function project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :inlined-generic-function.playground
  (:use ;; :cl
        :closer-common-lisp
        :introspect-environment
        :inlined-generic-function
        :inlined-generic-function.impl
        :trivia :alexandria :iterate))
(in-package :inlined-generic-function.playground)

(defgeneric plus (a b)
  (:generic-function-class inlined-generic-function))

(defmethod plus :around ((a number) (b number))
  (format t "~&(plus ~a ~a) is called~&" (type-of a) (type-of b))
  (call-next-method))

(defmethod plus ((a fixnum) (b fixnum))
  (+ a b))
(defmethod plus ((a float) (b float))
  (+ a b))

(defun func-using-plus (a b)
  (plus a b))

(defun func-using-plus (a b)
  (declare (inline plus))
  (plus a b))

(let ((*features* (cons :inline-generic-function *features*)))
  (print (inline-generic-function '(plus (1+ a) (1- b)))))

(print (function-information 'plus))

;; (defgeneric types (a)
;;   (:generic-function-class inlined-generic-function))
;; 
;; (defmethod types :around ((a number))
;;   (call-next-method))
;; (defmethod types ((a fixnum))
;;   (+ a b))
;; (defmethod types ((a float))
;;   (+ a b))
;; 
;; (defun func-using-types (a b)
;;   (types a b))
;; 
;; (defun func-using-types (a b)
;;   (declare (inline types))
;;   (types a b))

