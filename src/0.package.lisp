#|
  This file is a part of inlined-generic-function project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)

(defpackage inlined-generic-function
  (:nicknames :inlined-gf)
  (:export
   #:inlined-generic-function
   #:freeze-inlined-generic-function))

(defpackage inlined-generic-function.impl
  (:use :closer-common-lisp :trivia :alexandria :iterate
        :inlined-generic-function)
  (:export
   #:inlined-method
   #:method-lambda-expression))

(in-package :inlined-generic-function.impl)

;; target: implementing an inlined gf, a subclass of standard-generic-function
;; cf.
;; http://metamodular.com/CLOS-MOP/chapter-6.html
;; http://metamodular.com/CLOS-MOP/readers-for-generic-function-metaobjects.html

(defclass inlined-generic-function (standard-generic-function)
     ()
  (:documentation "A metaobject representing inlinable generic function.")
  (:default-initargs :method-class (find-class 'inlined-method))
  (:metaclass funcallable-standard-class))

(defclass inlined-method (standard-method)
     ((method-lambda-expression :initarg :method-lambda-expression
                                :accessor method-lambda-expression
                                :documentation "method lambda expression (a form) for later inlining"))
  (:documentation "A metaobject representing inlinable method."))

(defmethod make-method-lambda ((gf inlined-generic-function)
                               (m inlined-method)
                               lambda-expression environment)
  "Appends an additional keyword argument to the secondary value,
 which is passed to make-instance and sets the value"
  (multiple-value-bind (form initargs) (call-next-method)
      (values form
              (list*
               :method-lambda-expression form
               initargs))))

(defmethod ensure-generic-function-using-class :after ((gf inlined-generic-function)
                                                       fun-name &rest rest &key &allow-other-keys)
  "This method is called while the compilation results of defgeneric form is being loaded.
It sets up the compiler macro for this generic function."
  (setf (compiler-macro-function fun-name)
        #'inline-generic-function))

(defun inline-generic-function (whole env)
  (declare (ignorable form env))
  (format t "~&Inlining a generic function ~a~&" (first whole))
  whole)

