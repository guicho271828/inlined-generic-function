(in-package :inlined-generic-function.impl)

(defpattern call (name args)
  `(or (list* (and (not 'funcall) ,name) ,args)
       (list* 'funcall (list (or 'function 'quote) ,name) ,args)))

(defgeneric dummy ())

(defun inline-generic-function (gf-orig whole env)
  "This function should be partially evaluated to be a correct compiler function.
Returns an inlined form which is equivalent to calling the generic function."
  (declare (ignorable gf-orig whole env)) 
  (let ((forced t #+nil (member :inline-generic-function *features*)))
    (flet ((s-s-w? (&rest args)
             (when forced (apply #'simple-style-warning args))))
      (match whole
        ((list* 'apply _)
         (s-s-w? "Failed to inline ~a: APPLY form cannot be inlined." whole))
        ((and (call name args)
              (guard _ (fboundp name)
                     (fdefinition name) fdef
                     (type-of fdef) type
                     (multiple-value-list
                      (function-information name env))
                     (list binding ;The first indicates the type of function definition or binding
                           local   ;The second value is true if NAME is bound locally.
                           (assoc 'inline inline))))
         (ematch* (fdef binding local inline)
           (((not (type inlined-generic-function)))
            (s-s-w? "Failed to inline ~a: ~a is a ~a, not ~a." whole name type 'inlined-generic-function))
           (((inlined-generic-function
              :method-combination
              ;; this is a standard method combination
              (and mc (not (eq (generic-function-method-combination #'dummy))))))
            (s-s-w? "Failed to inline ~a: ~a has ~a, not ~a." whole name (type-of mc) (type-of (generic-function-method-combination #'dummy))))
           (((generic-function :lambda-list (guard lambda-list (intersection lambda-list lambda-list-keywords))))
            (s-s-w? "Failed to inline ~a: Generic function contains lambda-list-keywords." whole))
           ((_ (not :function))
            (s-s-w? "Failed to inline ~a: ~a is a ~a." whole name binding))
           ((_ _ t)
            (s-s-w? "Failed to inline ~a: ~a is locally shadowed." whole name))
           ((_ _ _ 'notinline)
            (s-s-w? "Failed to inline ~a: ~a is declared notinline." whole name))
           (((type inlined-generic-function) :function nil)
            (if (or (eq inline 'inline)
                    (and forced (not (eq inline 'notinline))))
                (return-from inline-generic-function
                  (compile-generic-function fdef args env whole))
                (s-s-w? "Inlining not performed: did not match the inlining criteria")))))
        ((call name _)
         (s-s-w? "Failed to inline ~a: ~a is not fbound" whole name))
        (_
         (s-s-w? "Failed to inline ~a: The form does not match any of our expected cases." whole)))
      whole)))

(defun compile-generic-function (gf args env whole)
  (declare (ignorable gf args env whole))
  (block nil
    (ematch gf
      ((generic-function name
                         method-combination
                         lambda-list
                         argument-precedence-order)
       (format t "~&Inlining a generic function ~a~&" name)
       (let ((args (mapcar (lambda (sym) (gensym (symbol-name sym))) lambda-list)))
         ;; #+nil
         (step
          (mapcar (lambda (m)
                    (print
                     (improve-readability
                      (sb-cltl2:macroexpand-all
                       (print
                        (inline-discriminating-function
                         args
                         (compute-effective-method
                          gf method-combination
                          ;; collect all methods of the same specifiers
                          (compute-applicable-methods-using-classes
                           gf (method-specializers m)))))))))
                  (sort (primary-methods gf)
                        (curry #'specializer<
                               lambda-list
                               argument-precedence-order))))
         whole)))))

(defun primary-methods (gf)
  (ematch gf
    ((generic-function methods)
     (remove-if #'method-qualifiers methods))))

(defun reorder-specializers (lambda-list precedence-order specializers)
  (assert (= (length lambda-list) (length specializers) (length precedence-order)))
  (mapcar (lambda (arg)
            (elt specializers (position arg lambda-list)))
          precedence-order))

(defun specializer< (lambda-list precedence-order m1 m2)
  "return true if some specializer of m1, checked in an precedence order, is a subtype of the specializer of m2"
  (some (lambda (a b)
          (and (subtypep a b)
               (not (subtypep b a))))
        (reorder-specializers lambda-list precedence-order (method-specializers m1))
        (reorder-specializers lambda-list precedence-order (method-specializers m2))))

(defun inline-discriminating-function (args form)
  ;; something like:
  ;; (CALL-METHOD #<INLINED-METHOD INLINED-GENERIC-FUNCTION.TEST::MINUS :AROUND (NUMBER NUMBER) {1004ACD283}>
  ;;              ((MAKE-METHOD
  ;;                (CALL-METHOD #<INLINED-METHOD INLINED-GENERIC-FUNCTION.TEST::MINUS (FLOAT FLOAT) {1004F759D3}>
  ;;                             (#<INLINED-METHOD INLINED-GENERIC-FUNCTION.TEST::MINUS (NUMBER NUMBER) {1004D856A3}>))))) 
  (%call-method args `(make-method ,form) nil nil))

(defun %call-method (args method more-methods more-args)
  (ematch method
    ((list 'make-method body)
     `(lambda ,args
        (macrolet ((call-method (method more-methods &rest more-args)
                     (%call-method ',args method more-methods more-args)))
          ,body)))
    ((inlined-method lambda-expression*)
     `(,lambda-expression* (list ,@args)
                           ;;(list ,@more-methods)
                           (list ,@(mapcar (lambda (m) (%call-method args m nil nil)) more-methods))
                           ,@more-args))))

(defun improve-readability (form)
  (match form
    ((list 'progn form)
     (improve-readability form))
    ((list* 'macrolet _ body)
     (improve-readability
      `(progn ,@body)))
    ((list* (list* 'lambda lambda-args body) args)
     (improve-readability
     `(let ,(mapcar #'list lambda-args args)
        ,@body)))
    ((cons _ _)
     (mapcar #'improve-readability form)
     ;; (cons (improve-readability car)
     ;;       (improve-readability cdr))
     )
    ((type atom)
     form)))
