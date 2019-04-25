(define (identifier? x) (symbol? x))
(define (reference? v) (integer? v))
(define (store? s) (list? s))
(define (method-env? e) (list? e))
(define (field-env? e) (list? e))
(define (any? x) #t)

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
  [extend-env-class-scope
    [class-name (maybe identifier?)]
    [env env?]]
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
  [obj-val  [val obj?]]
  )

(define-datatype program program?
  [a-program 
    [class-decls (list-of class-decl?)]
    [exp1 expression?]])

(define-datatype class-decl class-decl?
  [a-class-decl
    [class-name identifier?]
    [super-name identifier?]
    [field-names (list-of field-decl?)]
    [method-decls (list-of method-decl?)]])

(define-datatype field-decl field-decl?
  [a-field-decl
    [field-permission field-permission?]
    [field-name identifier?]])

(define-datatype field-permission field-permission?
  [private-field-permission]
  [protected-field-permission]
  [public-field-permission])

(define-datatype method-decl method-decl?
  [a-method-decl
    [method-permission method-permission?]
    [method-name identifier?]
    [vars (list-of identifier?)]
    [body expression?]])

(define-datatype method-permission method-permission?
  [private-method-permission]
  [protected-method-permission]
  [public-method-permission])

(define-datatype object object?
  [an-object
    [class-name identifier?]
    [fields (list-of reference?)]])

(define-datatype method method?
  [a-method
    [vars (list-of identifier?)]
    [body expression?]
    [permission method-permission?]
    [class-name identifier?]])

(define-datatype field field?
  [a-field
    [name identifier?]
    [permission field-permission?]
    [class-name identifier?]])

(define-datatype class class?
  [a-class
    [super-name (maybe identifier?)]
    [field-env (list-of field-env?)]
    [method-env method-env?]])

(define-datatype expression expression?
  [const-exp
    [num number?]]
  [diff-exp
    [exp1 expression?]
    [exp2 expression?]]
  [zero?-exp
    [exp1 expression?]]
  [if-exp
    [exp1 expression?]
    [exp2 expression?]
    [exp3 expression?]]
  [var-exp
    [var identifier?]]
  [let-exp
    [vars (list-of identifier?)]
    [exps (list-of expression?)]
    [body expression?]]
  [proc-exp
    [vars (list-of identifier?)]
    [body expression?]]
  [call-exp
    [rator expression?]
    [rands (list-of expression?)]]
  [letrec-exp
    [names  (list-of identifier?)]
    [varss  (list-of (list-of identifier?))]
    [bodies (list-of expression?)]
    [exp1   expression?]]
  [assign-exp
    [var  identifier?]
    [exp1 expression?]]
  [begin-exp
    [exp1 expression?]
    [exps (list-of expression?)]]
  [plus-exp
    [exp1 expression?]
    [exp2 expression?]]
  [list-exp
    [exps (list-of expression?)]]
  [print-exp
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
  )

