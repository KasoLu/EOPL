(define identifier? symbol?)
(define any?        (lambda (_) #t))

(define-datatype env env?
  [empty-env]
  [extend-env
    [vars (list-of identifier?)]
    [vals (list-of any?)]
    [env  env?]]
  [extend-env-rec
    [names (list-of identifier?)]
    [varss (list-of (list-of identifier?))]
    [procs (list-of any?)]
    [env   env?]]
  )

(define-datatype proc proc?
  [procedure
    [vars (list-of identifier?)]
    [body any?]
    [env  env?]]
  )

(define-datatype expval expval?
  [num-val  [val number?]]
  [bool-val [val boolean?]]
  [proc-val [val proc?]] 
  )

(define-datatype type type?
  [any-type]
  [int-type]
  [bool-type]
  [proc-type
    [args-type (list-of type?)]
    [ret-type type?]])

(define-datatype prgm prgm?
  [a-prgm
    [exp1 expr?]])

(define-datatype expr expr?
  [num-expr
    [num number?]]
  [var-expr
    [var identifier?]]
  [diff-expr
    [exp1 expr?]
    [exp2 expr?]]
  [zero?-expr
    [exp1 expr?]]
  [if-expr
    [exp1 expr?]
    [exp2 expr?]
    [exp3 expr?]]
  [let-expr
    [vars (list-of identifier?)]
    [exps (list-of expr?)]
    [body expr?]]
  [letrec-expr
    [names (list-of identifier?)]
    [varss (list-of (list-of identifier?))]
    [varss-type (list-of (list-of type?))]
    [procs-type (list-of type?)]
    [procs (list-of expr?)]
    [rbody expr?]]
  [proc-expr
    [vars (list-of identifier?)]
    [vars-type (list-of type?)]
    [body expr?]]
  [call-expr
    [rator expr?]
    [rands (list-of expr?)]]
  )

