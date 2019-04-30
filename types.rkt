(define (identifier? x) (symbol? x))
(define (reference? v) (integer? v))
(define (store? s) (list? s))
(define (method-env? e) (list? e))
(define (any? x) #t)
(define (method-tenv? e) (list? e))

(define-datatype env env?
  [empty-env]
  [extend-env
    [vars (list-of identifier?)]
    [vals (list-of any?)]
    [env  env?]]
  [extend-env-rec
    [names  (list-of identifier?)]
    [varss  (list-of (list-of identifier?))]
    [bodies (list-of expression?)]
    [env    env?]]
  )

(define-datatype proc proc?
  [procedure
    [vars (list-of identifier?)]
    [body expression?]
    [env  env?]]
  )

(define-datatype expval expval?
  [num-val  [val number?]]
  [bool-val [val boolean?]]
  [proc-val [val proc?]]
  [list-val [val list?]]
  )

(define-datatype type type?
  [int-type]
  [bool-type]
  [proc-type
    [vars-types (list-of type?)]
    [return-type type?]]
  [list-type
    [elem-type type?]]
  [void-type]
  [class-type
    [class-name identifier?]]
  )

(define-datatype class-decl class-decl?
  [a-class-decl
    [class-name identifier?]
    [super-name identifier?]
    [interface-names (list-of identifier?)]
    [field-types (list-of type?)]
    [field-names (list-of identifier?)]
    [method-decls (list-of method-decl?)]]
  [an-interface-decl
    [interface-name identifier?]
    [abs-method-decls (list-of abs-method-decl?)]])

(define-datatype method-decl method-decl?
  [a-method-decl
    [result-type type?]
    [method-name identifier?]
    [vars (list-of identifier?)]
    [vars-types (list-of type?)]
    [body expression?]])

(define-datatype abs-method-decl abs-method-decl?
  [an-abs-method-decl
    [result-type type?]
    [method-name identifier?]
    [method-vars (list-of identifier?)]
    [method-vars-types (list-of type?)]])

(define-datatype static-class static-class?
  [a-static-class
    [super-name (maybe identifier?)]
    [interface-names (list-of identifier?)]
    [field-names (list-of identifier?)]
    [field-types (list-of type?)]
    [method-tenv method-tenv?]]
  [an-interface
    [method-tenv method-tenv?]])

(define-datatype object object?
  [an-object
    [class-name identifier?]
    [fields (list-of reference?)]])

(define-datatype method method?
  [a-method
    [vars (list-of identifier?)]
    [body expression?]
    [super-name identifier?]
    [field-names (list-of identifier?)]])

(define-datatype class class?
  [a-class
    [super-name (maybe identifier?)]
    [field-names (list-of identifier?)]
    [method-env method-env?]])

(define-datatype program program?
  [a-program 
    [class-decls (list-of class-decl?)]
    [exp1 expression?]])

(define-datatype expression expression?
  [num-expr
    [num number?]]
  [diff-expr
    [exp1 expression?]
    [exp2 expression?]]
  [zero?-expr
    [exp1 expression?]]
  [if-expr
    [exp1 expression?]
    [exp2 expression?]
    [exp3 expression?]]
  [var-expr
    [var identifier?]]
  [let-expr
    [vars (list-of identifier?)]
    [exps (list-of expression?)]
    [body expression?]]
  [proc-expr
    [vars (list-of identifier?)]
    [vars-types (list-of type?)]
    [result-type type?]
    [body expression?]]
  [call-expr
    [rator expression?]
    [rands (list-of expression?)]]
  [letrec-expr
    [result-types (list-of type?)]
    [names  (list-of identifier?)]
    [varss  (list-of (list-of identifier?))]
    [varss-types (list-of (list-of type?))]
    [bodies (list-of expression?)]
    [exp1   expression?]]
  [assign-expr
    [var  identifier?]
    [exp1 expression?]]
  [begin-expr
    [exp1 expression?]
    [exps (list-of expression?)]]
  [plus-expr
    [exp1 expression?]
    [exp2 expression?]]
  [list-expr
    [exp1 expression?]
    [exps (list-of expression?)]]
  [print-expr
    [exp1 expression?]]
  [new-object-expr
    [class-name identifier?]
    [rands (list-of expression?)]]
  [method-call-expr
    [obj-exp expression?]
    [method-name identifier?]
    [rands (list-of expression?)]]
  [super-call-expr
    [method-name identifier?]
    [rands (list-of expression?)]]
  [self-expr]
  [cast-expr
    [exp1 expression?]
    [class-name identifier?]]
  [instanceof-expr
    [exp1 expression?]
    [class-name identifier?]]
  [fieldref-expr
    [exp1 expression?]
    [name identifier?]]
  [fieldset-expr
    [exp1 expression?]
    [name identifier?]
    [val1 expression?]]
  )
