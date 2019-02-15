(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")
(require racket/trace)

;run-cps : String -> FinalAnswer
(define (run-cps str)
  (value-of-program-cps
    (scan&parse-cps str)))

;value-of-program-cps : CpsPgm -> FinalAnswer
(define (value-of-program-cps pgm)
  (cases cpspgm pgm
    [a-cpspgm [expr]
      (value-of/k-cps expr (init-env))]))

;value-of/k-cps : TsfExp x Env x Cont -> FinalAnswer
(define (value-of/k-cps expr env)
  (cases tsfexp expr
    [smpexp->tsfexp [smpexp]
      (value-of-smpexp smpexp env)]
    [tsf-let-exp [vars exps body]
      (let ([vals (map (lambda (x) (value-of-smpexp x env)) exps)])
        (value-of/k-cps body (extend-env vars vals env)))]
    [tsf-letrec-exp [names varss procs rbody]
      (value-of/k-cps rbody (extend-env-rec names varss procs env))]
    [tsf-if-exp [smp1 exp2 exp3]
      (if (expval->bool (value-of-smpexp smp1 env))
        (value-of/k-cps exp2 env)
        (value-of/k-cps exp3 env))]
    [tsf-call-exp [rator rands]
      (let ([rator-proc (expval->proc (value-of-smpexp rator env))]
            [rands-vals (map (lambda (x) (value-of-smpexp x env)) rands)])
        (cases proc rator-proc
          [procedure [vars body env]
            (value-of/k-cps body (extend-env vars rands-vals env))]))]
    ))

;value-of-smpexp : SmpExp x Env -> FinalAnswer
(define (value-of-smpexp smp env)
  (cases smpexp smp
    [smp-const-exp [num]
      (num-val num)]
    [smp-var-exp [var]
      (apply-env env var)]
    [smp-diff-exp [exp1 exp2]
      (let ([val1 (value-of-smpexp exp1 env)] [val2 (value-of-smpexp exp2 env)])
        (num-val (- (expval->num val1) (expval->num val2))))]
    [smp-zero?-exp [exp1]
      (bool-val (zero? (expval->num (value-of-smpexp exp1 env))))]
    [smp-proc-exp [vars body]
      (proc-val (procedure vars body env))]
    ))

