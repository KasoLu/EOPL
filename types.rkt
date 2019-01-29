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

(define-datatype thread thread?
  [cont-thread
    [cont  cont?]
    [val   expval?]]
  [proc-thread
    [proc  proc?]
    [vals  (list-of reference?)]
    [cont  cont?]]
  )

(define-datatype mutex mutex?
  [a-mutex
    [ref-to-closed? reference?]
    [ref-to-wait-queue reference?]])

(define-datatype expval expval?
  [num-val   [val number?]]
  [bool-val  [val boolean?]]
  [proc-val  [val proc?]]
  [mutex-val [val mutex?]]
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
  [spawn-exp
    [exp1 expression?]]
  [mutex-exp]
  [wait-exp
    [exp1 expression?]]
  [signal-exp
    [exp1 expression?]]
  )

(define-datatype cont cont?
  [end-main-thread-cont]
  [end-subthread-cont]
  [diff1-cont 
    [exp2 expression?]
    [env  env?]
    [cont cont?]]
  [diff2-cont
    [val1 expval?]
    [env  env?]
    [cont cont?]]
  [rator-cont
    [rands (list-of expression?)]
    [env   env?]
    [cont  cont?]]
  [rands-cont
    [rator expval?]
    [rands (list-of expression?)]
    [vals  (list-of expval?)]
    [env   env?]
    [cont  cont?]]
  [zero?-cont
    [cont cont?]]
  [if-test-cont
    [exp2 expression?]
    [exp3 expression?]
    [env  env?]
    [cont cont?]]
  [let-cont
    [vars (list-of identifier?)]
    [exps (list-of expression?)]
    [vals (list-of expval?)]
    [body expression?]
    [env  env?]
    [cont cont?]]
  [assign-cont
    [ref  reference?]
    [cont cont?]]
  [begin-cont
    [exps (list-of expression?)]
    [env  env?]
    [cont cont?]]
  [spawn-cont
    [cont cont?]]
  [wait-cont
    [cont cont?]]
  [signal-cont
    [cont cont?]]
  )


