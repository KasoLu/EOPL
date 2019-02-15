(define identifier? symbol?)
(define reference?  integer?)
(define store?      list?)
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
  [num-val  [num number?]]
  [bool-val [bool boolean?]]
  [proc-val [proc proc?]]
  )

(define-datatype inppgm inppgm?
  [a-inppgm
    [exp1 inpexp?]])

(define-datatype inpexp inpexp?
  [inp-const-exp
    [num number?]]
  [inp-var-exp
    [var identifier?]]
  [inp-diff-exp
    [exp1 inpexp?]
    [exp2 inpexp?]]
  [inp-zero?-exp
    [exp1 inpexp?]]
  [inp-if-exp
    [exp1 inpexp?]
    [exp2 inpexp?]
    [exp3 inpexp?]]
  [inp-let-exp
    [vars (list-of identifier?)]
    [exps (list-of inpexp?)]
    [body inpexp?]]
  [inp-letrec-exp
    [names (list-of identifier?)]
    [varss (list-of (list-of identifier?))]
    [procs (list-of inpexp?)]
    [rbody inpexp?]]
  [inp-proc-exp
    [vars (list-of identifier?)]
    [body inpexp?]]
  [inp-call-exp
    [rator inpexp?]
    [rands (list-of inpexp?)]]
  )

(define-datatype cpspgm cpspgm?
  [a-cpspgm
    [exp1 tsfexp?]])
(define-datatype smpexp smpexp?
  [smp-const-exp
    [num number?]]
  [smp-var-exp
    [var identifier?]]
  [smp-diff-exp
    [exp1 smpexp?]
    [exp2 smpexp?]]
  [smp-zero?-exp
    [exp1 smpexp?]]
  [smp-proc-exp
    [vars (list-of identifier?)]
    [body tsfexp?]]
  )

(define-datatype tsfexp tsfexp?
  [smpexp->tsfexp
    [exp1 smpexp?]]
  [tsf-let-exp
    [vars (list-of identifier?)]
    [exps (list-of smpexp?)]
    [body tsfexp?]]
  [tsf-letrec-exp
    [names (list-of identifier?)]
    [varss (list-of (list-of identifier?))]
    [procs (list-of tsfexp?)]
    [rbody tsfexp?]]
  [tsf-if-exp
    [exp1 smpexp?]
    [exp2 tsfexp?]
    [exp3 tsfexp?]]
  [tsf-call-exp
    [rator smpexp?]
    [rands (list-of smpexp?)]]
  )
