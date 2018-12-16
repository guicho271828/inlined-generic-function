(in-package :inlined-generic-function.impl)

(defpattern call (name args)
  `(or (list* (and (not 'funcall) ,name) ,args)
       (list* 'funcall (list (or 'function 'quote) ,name) ,args)))

(defgeneric dummy ())

(defun lambda-list-atoms (lambda-list)
  (match (trivia.level2.impl::parse-lambda-list lambda-list)
    ((alist (:atom . atoms)) atoms)))

(defun bind-lambda-list (lambda-list args)
  (match (trivia.level2.impl::parse-lambda-list lambda-list)
    ((guard alst t
	    (cdr (assoc :atom alst)) atoms
	    (cdr (assoc :optional alst)) optionals
	    (cdr (assoc :rest alst)) rest
	    (cdr (assoc :keyword alst)) keywords
	    (cdr (assoc :aux alst)) aux)
     (let ((symbols (append atoms (mapcar #'first optionals) rest (mapcar #'caar keywords) (mapcar #'first aux))))
       (mapcar #'list
	symbols
	(eval `(match ',args
		 ((lambda-list ,@(mapcar #'(lambda (x)
					     (match x
					       ((lambda-list var default &optional defaultp) `(,var ',default ,@(if defaultp `(,defaultp))))
					       (var var)))
					 lambda-list))
		  (let ,(if rest `((,(first rest) `(list ,@,(first rest)))))
		    (list ,@symbols))))))))))

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
                               declinfo)
                         (and (list binding
                                    local)
                              (<> declinfo nil))
                         (and (list binding)
                              (<> local nil)
                              (<> declinfo nil))
                         (and (<> binding nil)
                              (<> local nil)
                              (<> declinfo nil)))))
         (let ((inline (cdr (assoc 'inline declinfo))))
           (ematch* (fdef binding local inline)
             (((not (type inlined-generic-function)))
              (s-s-w? "Failed to inline ~a: ~a is a ~a, not ~a." whole name type 'inlined-generic-function))
             ;; (((inlined-generic-function
             ;;    :method-combination
             ;;    ;; this is a standard method combination
             ;;    (and mc (not (eq (generic-function-method-combination #'dummy))))))
             ;;  (s-s-w? "Failed to inline ~a: ~a has ~a, not ~a." whole name (type-of mc) (type-of (generic-function-method-combination #'dummy))))
             ;; (((generic-function :lambda-list (guard lambda-list (intersection lambda-list lambda-list-keywords))))
             ;;  (s-s-w? "Failed to inline ~a: Generic function contains lambda-list-keywords." whole))
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
                  (s-s-w? "Inlining not performed: did not match the inlining criteria"))))))
        ((call name _)
         (s-s-w? "Failed to inline ~a: ~a is not fbound" whole name))
        (_
         (s-s-w? "Failed to inline ~a: The form does not match any of our expected cases." whole)))
      whole)))

(defvar *current-gf*)
(defun compile-generic-function (gf args env whole)
  (declare (ignorable gf args env whole))
  (let ((*current-gf* gf))
    (restart-case
        (%compile-generic-function gf args whole)
      (continue ()
        :report "Decline inlining"
        whole))))

(defun %compile-generic-function (gf args whole)
  (ematch gf
    ((generic-function name
                       methods
                       method-combination
                       lambda-list
                       argument-precedence-order)
     (format t "~&Inlining a generic function ~a~&" name)
     (let* ((lambda-atoms (lambda-list-atoms lambda-list))
	    (gensyms (mapcar (compose #'gensym #'symbol-name) lambda-atoms))
	    (partial-args (append gensyms (subseq args (length lambda-atoms)))))
       `(let ,(mapcar #'list gensyms args)
	  (ematch* ,(reorder-to-precedence lambda-atoms argument-precedence-order gensyms)
	    ,@(%matcher-clause partial-args gf method-combination whole argument-precedence-order lambda-atoms methods)))))))

(defun %matcher-clause (gensyms gf method-combination whole argument-precedence-order lambda-list methods)
  (iter (for m in (sort (copy-seq methods)
                        (curry #'specializer<
                               lambda-list
                               argument-precedence-order)))
        (match m
          ((method specializers (qualifiers (not (or :around :before :after))))
           (collect
               `(,(%matcher-pattern lambda-list argument-precedence-order specializers)
                  ,(improve-readability
                    (%matcher-body gensyms gf m method-combination specializers whole))))))))

(defun %matcher-pattern (lambda-list argument-precedence-order specializers)
  (mapcar (lambda (c)
            (ematch c
              ((class class)
               `(type ,(class-name c)))
              ((class eql-specializer)
               `(eql ',(eql-specializer-object c)))))
          (reorder-to-precedence lambda-list
                                 argument-precedence-order
                                 specializers)))

(defun %matcher-body (gensyms gf m method-combination specializers whole)
  (#+sbcl sb-cltl2:macroexpand-all
   ;; the use of macroexpand-all is only for the debugging purpose.
   ;; the final compilation results should be the same for all implementations.
   #+ccl ccl:macroexpand-all
   ;; umm CCL complains that it failed to dump a class object.
   ;; make-load-form for method objects are missing.
   ;; Expanding it first solves this.
   #-(or sbcl ccl) progn
   (inline-discriminating-function
    whole
    gensyms
    (handler-case
        (compute-effective-method ; <<---- now note that this may return '(SB-PCL::%NO-PRIMARY-METHOD '#<INLINED-GENERIC-FUNCTION> SB-PCL::.ARGS.)
         gf method-combination
         ;; collect all methods of the same specifiers.
         ;; We cannot use compute-applicable-methods-using-classes
         ;; because it may contain eql-specializers.
         (%compute-applicable-methods gf (method-specializers m)))
      #+ccl
      (error ()
        ;; closer-mop on ccl may throw
        ;; error when the given set of
        ;; methods do not have the
        ;; primary methods. This is not
        ;; specified in AMOP.
        (simple-style-warning "Skipping ~a (ccl specific)" m)))
    specializers)))

(defun %compute-applicable-methods (gf specializers)
  ;; able to handle eql-specializer gracefully
  (ematch gf
    ((generic-function :methods methods
                       lambda-list
                       argument-precedence-order)
     (sort (iter (for m in methods)
                 (when (iter (for spec1 in specializers)
                             (for spec2 in (method-specializers m))
                             (always
                              (match* (spec1 spec2)
                                (((eql-specializer :object o1)
                                  (eql-specializer :object o2))
                                 (eql o1 o2))
                                (((class class)
                                  (class class))
                                 (subtypep spec1 spec2))
                                (_ nil))))
                   (collect m)))
           (curry #'specializer< (lambda-list-atoms lambda-list) argument-precedence-order)))))

#+nil
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
          (match* (a b)
            (((eql-specializer) (eql-specializer))
             nil)
            (((eql-specializer :object o1) class)
             (typep o1 class))
            ((_ (eql-specializer)) nil)
            ((_ _)
             (and (subtypep a b)
                  (not (subtypep b a))))))
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
     `(macrolet ((call-method (method &optional more-methods)
                   (let ((*current-inline-form* ',*current-inline-form*))
                     (%call-method ',args method more-methods ',specs))))
        ,body))
    ((inlined-method :lambda-expression
                     (list* 'lambda l-args body))
     `(macrolet (;; since everything is supposed to work in compile-time,
                 ;; call-next-method and next-method-p can be a macrolet.
                 (call-next-method (&rest args)
                   (match ',more-methods
                     ((list* next rest)
                      (let ((*current-inline-form* ',*current-inline-form*)
                            (*current-gf* ',*current-gf*))
                        ;; FIXME: check this.
                        ;; CLHS  Local Function CALL-NEXT-METHOD
                        ;;  Neither argument defaulting, nor using setq,
                        ;; nor rebinding variables with the same names as
                        ;; parameters of the method affects the values
                        ;; call-next-method passes to the method it calls.
                        (%call-method (if args args ',args) next rest ',specs)))
                     (nil
                      ;; This throws an compile-time error.
                      (cerror "Continue with inserting NO-NEXT-METHOD"
                              'simple-error
                              :format-control "While inlining ~a: no next method after ~a (~{~s~^ ~})!"
                              :format-arguments
                              (list ',*current-inline-form*
                                    ',(generic-function-name (method-generic-function method))
                                    ',(method-specializers method)))
                      ;; fixme: call no-next-method
                      ;; call-next-method requires runtime args.
                      `(no-next-method ,',*current-gf* ,',method ,@args))))
                 (next-method-p ()
                   ,(if more-methods t nil)))
        (let* ,(bind-lambda-list l-args args)
          ,@(remove nil
                    (mapcar (lambda (spec arg)
                              (when (classp spec)
                                `(declare (type ,(class-name spec) ,arg))))
                            specs
                            l-args))
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
