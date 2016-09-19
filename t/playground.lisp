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

(setf *features* (cons :inline-generic-function *features*))

(defgeneric plus (a b)
  (:generic-function-class inlined-generic-function))

;; there is only one method
(defmethod plus ((a fixnum) (b fixnum))
  (+ a b))

(introspect-environment:compiler-macroexpand '(plus a b))

'(LET ((#:A738 A) (#:B739 B))
  (EMATCH* (#:A738 #:B739)
    (((TYPE FIXNUM) (TYPE FIXNUM))
     (LET ((A #:A738) (B #:B739))
       (DECLARE (TYPE FIXNUM A))
       (DECLARE (TYPE FIXNUM B))
       (+ A B)))))

(defun maybe-compile-time-error ()
  (error "I signal an error in runtime"))

(defmethod plus (a b)
  (maybe-compile-time-error))

(introspect-environment:compiler-macroexpand '(plus a b))

'(LET ((#:A738 A) (#:B739 B))
  (EMATCH* (#:A738 #:B739)
    (((TYPE FIXNUM) (TYPE FIXNUM))
     (LET ((A #:A738) (B #:B739))
       (DECLARE (TYPE FIXNUM A))
       (DECLARE (TYPE FIXNUM B))
       (+ A B)))
    (((TYPE T) (TYPE T))
     (LET ((A #:A738) (B #:B739))
       (DECLARE (TYPE T A))
       (DECLARE (TYPE T B))
       (MAYBE-COMPILE-TIME-ERROR)))))


(defun compilation-fails (a b)
  (declare (inline plus))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum a b))
  (macrolet ((maybe-compile-time-error ()
               (error "I signal an error in compile time")))
    ;; Since all method forms are inlined,
    ;; maybe-compile-time-error still presents
    ;; regardless of the argument types.
    ;;
    ;; In other words, Compilation always fails
    ;; even if the arguments are guaranteed to be fixnums
    (plus a b)))


;; So what we can do?

;; lets hack in the SBCL internal --- we can give some information to
;; SBCL's IR.

;; now we want a form that signals a compile-time error only when it is reachable.

(sb-c:defknown ir1-compile-time-error () () (sb-c:always-translatable))
(sb-c:deftransform ir1-compile-time-error (())
  (restart-case
      (error "I signal an error in IR1 compile time")
    (continue ()
      (format *error-output* "~3%!!!!!!!! CONTINUED !!!!!!!!!~3%"))))

(handler-bind ((error #'continue))
  (eval 
   '(defmethod plus (a b)
     (ir1-compile-time-error))))

(defun compilation-ok (a b)
  (declare (inline plus))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum a b))
  (plus a b))

(defun compilation-fails (a b)
  (declare (inline plus))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type float a b))
  (plus a b))

SB-C:DEFINE-VOP
deftransform
define-source-transform
defknown
defoptimizer

(sb-c:defknown (+ *) (&rest number) number
    (movable foldable flushable commutative))
