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
  [num-val  [val number?]]
  [bool-val [val boolean?]]
  [proc-val [val proc?]]
  [ref-val  [val reference?]]
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
  [inp-sum-exp
    [exps (list-of inpexp?)]]
  [inp-newref-exp
    [inp1 inpexp?]]
  [inp-deref-exp
    [inp1 inpexp?]]
  [inp-setref-exp
    [inp1 inpexp?]
    [inp2 inpexp?]]
  [inp-begin-exp
    [inp1 inpexp?]
    [inps (list-of inpexp?)]]
  )

(define-datatype outpgm outpgm?
  [a-outpgm
    [exp1 tpfexp?]])

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
    [body tpfexp?]]
  [smp-sum-exp
    [exps (list-of smpexp?)]]
  )

(define-datatype tpfexp tpfexp?
  [smpexp->tpfexp
    [smp1 smpexp?]]
  [tpf-let-exp
    [vars (list-of identifier?)]
    [smps (list-of smpexp?)]
    [body tpfexp?]]
  [tpf-letrec-exp
    [names (list-of identifier?)]
    [varss (list-of (list-of identifier?))]
    [procs (list-of tpfexp?)]
    [rbody tpfexp?]]
  [tpf-if-exp
    [smp1 smpexp?]
    [tpf2 tpfexp?]
    [tpf3 tpfexp?]]
  [tpf-call-exp
    [rator smpexp?]
    [rands (list-of smpexp?)]]
  [tpf-newrefk-exp
    [smp1 smpexp?]
    [smp2 smpexp?]]
  [tpf-derefk-exp
    [smp1 smpexp?]
    [smp2 smpexp?]]
  [tpf-setrefk-exp
    [smp1 smpexp?]
    [smp2 smpexp?]
    [tpf1 tpfexp?]]
  )
