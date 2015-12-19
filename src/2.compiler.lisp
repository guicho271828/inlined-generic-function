(in-package :inlined-generic-function.impl)

(defun inline-generic-function (whole env)
  (declare (ignorable form env))
  (format t "~&Inlining a generic function ~a~&" (first whole))
  whole)


