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

(define-datatype cont cont?
  [end-cont]
  [zero?-cont
    [cont cont?]]
  [let-cont
    [vars (list-of identifier?)]
    [exps (list-of expression?)]
    [vals (list-of expval?)]
    [body expression?]
    [env  env?]
    [cont cont?]]
  [if-test-cont
    [exp2 expression?]
    [exp3 expression?]
    [env  env?]
    [cont cont?]]
  [diff1-cont
    [exp2 expression?]
    [env  env?]
    [cont cont?]]
  [diff2-cont
    [val1 expval?]
    [cont cont?]]
  [rator-cont
    [exps (list-of expression?)]
    [env  env?]
    [cont cont?]]
  [rands-cont
    [rator expval?]
    [exps  (list-of expression?)]
    [vals  (list-of expval?)]
    [env   env?]
    [cont  cont?]]
  )

(define-datatype proc proc?
  [procedure
    [vars (list-of identifier?)]
    [body expression?]
    [env  env?]]
  )

(define-datatype expval expval?
  [num-val  [num  number?]]
  [bool-val [bool boolean?]]
  [proc-val [proc proc?]]
  [list-val [lst  list?]]
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
  )

