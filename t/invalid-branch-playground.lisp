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

(defmethod plus ((a fixnum) (b fixnum))
  (+ a b))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *invalid-branch-warning-level* 'simple-style-warning))
(defmethod plus (a b)
  (invalid-branch))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *invalid-branch-warning-level* 'error))

(defun plus2 (a b)
  (declare (inline plus))
  (plus a b))
