(in-package :inlined-generic-function.impl)

(defun inline-generic-function (gf-orig whole env)
  "This function should be partially evaluated to be a correct compiler function.
Returns an inlined form which is equivalent to calling the generic function."
  (declare (ignorable gf-orig whole env))
  (match* (whole (member :inline-generic-function *features*))
    (((list* 'apply _) forced)
     (when forced
       (simple-style-warning "Failed to inline ~a: APPLY form cannot be inlined." whole))
     whole)
    (((and (or (list* name args)
               (list* 'funcall (list (or 'function 'quote) name) args))
           (guard _ (fboundp name)
                  (fdefinition name)
                  (and gf (type inlined-generic-function))
                  (multiple-value-list
                   (function-information name env))
                  (list :function       ;The first indicates the type of function definition or binding
                        nil             ;The second value is true if NAME is bound locally.
                        (assoc 'inline inline)))) ;The third value is an alist describing the declarations
      forced)
     (if (or (eq inline 'inline)
             (and forced (not (eq inline 'notinline))))
         (progn
           (format t "~&Inlining a generic function ~a~&" name)
           (compile-generic-function gf args env whole))
         whole))
    (((and (or (list* name args)
               (list* 'funcall (list (or 'function 'quote) name) args))
           (guard _ (fboundp name)
                  (type-of (fdefinition name)) type))
      forced)
     (when forced
       (simple-style-warning "Failed to inline ~a: Inline information not available. ~a is a ~a, not ~a."
                             whole name type 'inlined-generic-function))
     whole)
    ((_ forced)
     (when forced
       (simple-style-warning "Failed to inline ~a: The form does not match any of our expected cases." whole))
     whole)))

(defun compile-generic-function (gf args env whole)
  (declare (ignorable gf args env))
  whole)
