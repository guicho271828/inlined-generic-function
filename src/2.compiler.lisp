(in-package :inlined-generic-function.impl)

(defpattern call (name args)
  `(or (list* (and (not 'funcall) ,name) ,args)
       (list* 'funcall (list (or 'function 'quote) ,name) ,args)))

(defgeneric dummy ())

(defun inline-generic-function (whole &optional env)
  "Returns an inlined form which is equivalent to calling the generic function."
  (declare (ignorable whole env)) 
  (let ((forced (member :inline-generic-function *features*)))
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
                     (or (list binding ;The first indicates the type of function definition or binding
                               local   ;The second value is true if NAME is bound locally.
                               (assoc 'inline inline))
                         (and (list binding
                                    local)
                              (<> inline nil))
                         (and (list binding)
                              (<> local nil)
                              (<> inline nil))
                         (and (<> binding nil)
                              (<> local nil)
                              (<> inline nil)))))
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
  (restart-case
    (ematch gf
      ((generic-function name
                         method-combination
                         lambda-list
                         argument-precedence-order)
       (format t "~&Inlining a generic function ~a~&" name)
       (let ((gensyms (mapcar (lambda (sym) (gensym (symbol-name sym))) lambda-list)))
         `(let ,(mapcar #'list gensyms args)
            (ematch* ,(reorder-to-precedence lambda-list argument-precedence-order gensyms)
              ,@(mapcar (lambda (m)
                          (ematch m
                            ((method specializers)
                             `(,(mapcar (lambda (c) `(type ,(class-name c)))
                                        (reorder-to-precedence lambda-list argument-precedence-order specializers))
                                ,(improve-readability
                                  (#+sbcl
                                   sb-cltl2:macroexpand-all
                                   ;; the use of macroexpand-all is only for
                                   ;; the debugging purpose. the final
                                   ;; compilation results should be the same
                                   ;; for all implementations.
                                   #-sbcl
                                   progn
                                   (inline-discriminating-function
                                    whole
                                    gensyms
                                    (compute-effective-method
                                     gf method-combination
                                     ;; collect all methods of the same specifiers
                                     (compute-applicable-methods-using-classes
                                      gf (method-specializers m)))
                                    specializers)))))))
                        (sort (or (primary-methods gf)
                                  (error "Failed to inline ~a: ~A is missing the primary methods" whole name))
                              (curry #'specializer<
                                     lambda-list
                                     argument-precedence-order))))))))
    (continue ()
      :report "Decline inlining"
      whole)))

(defun primary-methods (gf)
  (ematch gf
    ((generic-function methods)
     (remove-if #'method-qualifiers methods))))

(defun reorder-to-precedence (lambda-list precedence-order specializers)
  (assert (= (length lambda-list) (length specializers) (length precedence-order)))
  (mapcar (lambda (arg)
            (elt specializers (position arg lambda-list)))
          precedence-order))

(defun specializer< (lambda-list precedence-order m1 m2)
  "return true if some specializer of m1, checked in an precedence order, is a subtype of the specializer of m2"
  (some (lambda (a b)
          (and (subtypep a b)
               (not (subtypep b a))))
        (reorder-to-precedence lambda-list precedence-order (method-specializers m1))
        (reorder-to-precedence lambda-list precedence-order (method-specializers m2))))

;; something like:
;; (CALL-METHOD #<INLINED-METHOD INLINED-GENERIC-FUNCTION.TEST::MINUS :AROUND (NUMBER NUMBER) {1004ACD283}>
;;              ((MAKE-METHOD
;;                (CALL-METHOD #<INLINED-METHOD INLINED-GENERIC-FUNCTION.TEST::MINUS (FLOAT FLOAT) {1004F759D3}>
;;                             (#<INLINED-METHOD INLINED-GENERIC-FUNCTION.TEST::MINUS (NUMBER NUMBER) {1004D856A3}>))))) 

(defvar *current-inline-form*) ;; only here for printing informative errors
(defun inline-discriminating-function (*current-inline-form* args form specs)
  "Corresponds to compute-discriminating-function in AMOP"
  (%call-method args `(make-method ,form) nil specs))

(defun %call-method (args method more-methods specs)
  (ematch method
    ((list 'make-method body)
     `(macrolet ((call-method (method more-methods)
                   (let ((*current-inline-form* ',*current-inline-form*))
                     (%call-method ',args method more-methods ',specs))))
        ,body))
    ((inlined-method :lambda-expression
                     (list* 'lambda l-args body))
     `(macrolet (;; since everything is supposed to work in compile-time,
                 ;; it can be a macrolet.
                 (call-next-method (&rest args)
                   (match ',more-methods
                     ((list* next rest)
                      (let ((*current-inline-form* ',*current-inline-form*))
                        ;; FIXME: check this.
                        ;; CLHS  Local Function CALL-NEXT-METHOD
                        ;;  Neither argument defaulting, nor using setq,
                        ;; nor rebinding variables with the same names as
                        ;; parameters of the method affects the values
                        ;; call-next-method passes to the method it calls.
                        (%call-method (if args args ',args) next rest ',specs)))
                     (nil
                      ;; This throws an compile-time error.
                      ;; fixme: call no-next-method
                      (error "Failed to inline ~a: no next method after ~a (~{~s~^ ~})!"
                             ',*current-inline-form*
                             ',(generic-function-name (method-generic-function method))
                             ',(method-specializers method)))))
                 (next-method-p ()
                   ,(if more-methods t nil)))
        (let ,(mapcar #'list l-args args)
          ,@(mapcar (lambda (spec arg)
                      `(declare (type ,(class-name spec) ,arg)))
                    specs
                    l-args)
          ,@body)))))

(defun improve-readability (form)
  (match form
    ((list 'progn form)
     (improve-readability form))
    ((list* 'macrolet _ body)
     (improve-readability
      `(progn ,@body)))
    ;; ((list* (list* 'lambda lambda-args body) args)
    ;;  (improve-readability
    ;;  `(let ,(mapcar #'list lambda-args args)
    ;;     ,@body)))
    ;; ((list* 'let nil body)
    ;;  (improve-readability
    ;;   `(progn ,@body)))
    ;; ((list* 'let (list* (list var (eq var)) rest) body)
    ;;  (improve-readability
    ;;   `(let ,rest ,@body)))
    ((cons _ _)
     (mapcar #'improve-readability form)
     ;; (cons (improve-readability car)
     ;;       (improve-readability cdr))
     )
    ((type atom)
     form)))
