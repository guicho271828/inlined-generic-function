#|
  This file is a part of inlined-generic-function project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage inlined-generic-function
  (:use :closer-common-lisp :trivia :alexandria :iterate)
  (:nicknames :inlined-gf))
(in-package :inlined-generic-function)

;; target: implementing an inlined gf, a subclass of standard-generic-function
;; cf.
;; http://metamodular.com/CLOS-MOP/chapter-6.html
;; http://metamodular.com/CLOS-MOP/readers-for-generic-function-metaobjects.html

#+nil
(defgeneric name (a b c)
  (:generic-function-class inlined-generic-function)
  (:method-class inlined-method))



(defclass inlined-generic-function (standard-generic-function)
     ()
  (:default-initargs :method-class (find-class 'inlined-method))
  (:metaclass funcallable-standard-class))

(defclass inlined-method (standard-method)
     (;; lambda-list
      (method-lambda-expression :initarg :method-lambda-expression
                                :accessor method-lambda-expression)))

(defmethod make-method-lambda :around ((gf inlined-generic-function)
                                       (m inlined-method)
                                       lambda-expression environment)
  (format t "~&setting ~A to ~a" lambda-expression m)
  (setf (method-lambda-expression m)
        (call-next-method)))

;; generic-function-methods
