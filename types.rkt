(define (identifier? x) (symbol? x))
(define (reference? v) (integer? v))
(define (store? s) (list? s))

(define-datatype env env?
  [empty-env]
  [extend-env
    [vars (list-of identifier?)]
    [vals (list-of (lambda (x) #t))]
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

(define-datatype thunk thunk?
  [a-thunk
    [exp1 expression?]
    [env  env?]])

(define-datatype expval expval?
  [num-val  [num number?]]
  [bool-val [bool boolean?]]
  [proc-val [proc proc?]]
  [ref-val  [ref reference?]]
  [arr-val  [arr (list-of reference?)]]
  )

(define-datatype program program?
  [a-program 
    [exp1 expression?]])

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
  [ref-exp
    [var1 identifier?]]
  [deref-exp
    [var1 identifier?]]
  [setref-exp
    [var1 identifier?]
    [exp1 expression?]]
  [newarray-exp 
    [exp1 expression?]
    [exp2 expression?]]
  [arrayref-exp
    [exp1 expression?]
    [exp2 expression?]]
  [arrayset-exp
    [exp1 expression?]
    [exp2 expression?]
    [exp3 expression?]]
  )

