(define (identifier? x) (symbol? x))
(define (reference? v) (integer? v))
(define (store? s) (list? s))
(define (method-env? e) (list? e))
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
  [prop-val [val property?]]
  )

(define-datatype program program?
  [a-program 
    [exp1 expression?]])

(define-datatype property property?
  [a-property
    [field-names (list-of identifier?)]
    [field-refs  (list-of reference?)]
    [method-list (list-of pair?)]])

(define-datatype method method?
  [a-method
    [vars (list-of identifier?)]
    [body expression?]])

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
  [method-call-expr
    [prop-expr expression?]
    [method-name identifier?]
    [rands (list-of expression?)]]
  [self-expr]
  [property-expr
    [super-prop-expr expression?]
    [field-names (list-of identifier?)]
    [method-names (list-of identifier?)]
    [method-varss (list-of (list-of identifier?))]
    [method-procs (list-of expression?)]]
  [clone-expr
    [prop-expr expression?]]
  )

