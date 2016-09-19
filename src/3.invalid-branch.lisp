
(in-package :inlined-generic-function.impl)

(declaim (type (member style-warning warning error) *invalid-branch-warning-level*))

(defparameter *invalid-branch-warning-level* 'warning
  "A flag controlling the level of compile-time warning signaled when
compiling a call to INVALID-BRANCH.
This does not affect the behavior of INVALID-BRANCH, which always signals an error.")

#+sbcl
(sb-c:defknown invalid-branch (&optional string) () ())

#+sbcl
(sb-c:deftransform invalid-branch ((&optional message))
  (funcall *invalid-branch-warning-level*
           "Failed to prune an INVALID-BRANCH.
The call to INVALID-BRANCH should be dead-code eliminated by the compiler.
~@[~a~]" message))

;; (declaim (notinline invalid-branch))
(defun invalid-branch (&optional (message
                                  "Consider e.g. adding type restriction, check for infinite loop/recursion"))
  "This function mark a specific part of the code to be invalid.
It MUST NOT be used in a valid code.

On supported implementations (currently SBCL only),
the compiler signals a warning,style-warning or error
 (depending on the value of *INVALID-BRANCH-WARNING-LEVEL*)
when the compiler fails to eliminiate it as a dead-code.

This is useful to detect a specific kind of errors,
e.g. type errors, infinite loops, infinite recursion and so on.
When it happens, you should fix the code, add a type restriction etc.
in order to make it compile.

On unsupported implementations, this function has no compile-time effect
and just signals an error.

Supported implementations:

SBCL:
This feature is tested against all patch versions in the latest minor version,
as well as the most recent patches in the past two minor versions.
"
  (error "Failed to prune an INVALID-BRANCH.
The call to INVALID-BRANCH should be dead-code eliminated by the compiler.
~@[~a~]" message))

