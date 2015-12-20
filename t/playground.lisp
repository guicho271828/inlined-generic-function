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
  (+ a b)
  (call-next-method))

(defmethod plus ((a fixnum) (b fixnum))
  (+ a b))
(defmethod plus ((a double-float) (b double-float))
  (+ a b))

(defun func-using-plus (a b)
  (declare (optimize (speed 3) (safety 0)))
  (plus a b))

(defun func-using-inlined-plus (a b)
  (declare (inline plus))
  (declare (optimize (speed 3) (safety 0)))
  (plus a b))

(let ((*features* (cons :inline-generic-function *features*)))
  (print (inline-generic-function '(plus (1+ a) (1- b)))))

;; benchmark

(defgeneric normal-plus (a b))

(defmethod normal-plus :around ((a number) (b number))
  (+ a b)
  (call-next-method))

(defmethod normal-plus ((a fixnum) (b fixnum))
  (+ a b))
(defmethod normal-plus ((a double-float) (b double-float))
  (+ a b))

(defun func-using-normal-plus (a b)
  (declare (optimize (speed 3) (safety 0)))
  (normal-plus a b))

(defun func-using-normal-inlined-plus (a b)
  (declare (inline plus))
  (declare (optimize (speed 3) (safety 0)))
  (normal-plus a b))

(defvar *input* (iter (repeat 1000)
                      (collect (cons (random 100.0d0) (random 100.0d0)))
                      (collect (cons (+ 20 (random 100)) (+ 20 (random 100))))))

(defun benchmark ()
  (time (iter (for (a . b) in *input*)
              (func-using-normal-plus a b)))
  (time (iter (for (a . b) in *input*)
              (func-using-normal-inlined-plus a b)))
  (time (iter (for (a . b) in *input*)
              (func-using-plus a b)))
  (time (iter (for (a . b) in *input*)
              (func-using-inlined-plus a b))))

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream))
      (*trace-output* (make-broadcast-stream)))
  (iter (repeat 1000)
        (benchmark)))
(sb-ext:gc :full t)

(benchmark)

