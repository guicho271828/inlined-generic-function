#|
  This file is a part of inlined-generic-function project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage inlined-generic-function
  (:use :closer-common-lisp :trivia :alexandria :iterate)
  (:nicknames :inlined-gf)
  (:export
   #:inlined-generic-function
   #:inlined-method
   #:method-lambda-expression))
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
     ((method-lambda-expression :initarg :method-lambda-expression
                                :accessor method-lambda-expression
                                :documentation "store the function body for later inlining")))

(defmethod make-method-lambda ((gf inlined-generic-function)
                               (m inlined-method)
                               lambda-expression environment)
  (multiple-value-bind (form initargs) (call-next-method)
      (values form
              (list* ;; this is passed to make-instance, which results in setting the value
               :method-lambda-expression form
               initargs))))

;; (cl:defmethod make-instance :after ((m inlined-method) &rest initargs &key :)
;;   (setf (method-lambda-expression m)
;;         (make-method-lambda (method-generic-function m) m

;; generic-function-methods
