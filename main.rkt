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
      (value-of/k-cps expr (init-env)
        (lambda (val) val))]))

;value-of/k-cps : TsfExp x Env x Cont -> FinalAnswer
(define (value-of/k-cps expr env cont)
  (cases tsfexp expr
    [smpexp->tsfexp [smpexp]
      (cont (value-of-smpexp smpexp env))]
    [tsf-let-exp [vars exps body]
      (let ([vals (map (lambda (x) (value-of-smpexp x env)) exps)])
        (value-of/k-cps body (extend-env vars vals env) cont))]
    [tsf-letrec-exp [names varss procs rbody]
      (value-of/k-cps rbody (extend-env-rec names varss procs env) cont)]
    [tsf-if-exp [smp1 exp2 exp3]
      (if (expval->bool (value-of-smpexp smp1 env))
        (value-of/k-cps exp2 env cont)
        (value-of/k-cps exp3 env cont))]
    [tsf-call-exp [rator rands]
      (let ([rator-proc (expval->proc (value-of-smpexp rator env))]
            [rands-vals (map (lambda (x) (value-of-smpexp x env)) rands)])
        (cases proc rator-proc
          [procedure [vars body env]
            (value-of/k-cps body (extend-env vars rands-vals env) cont)]))]
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

;run-inp : String -> FinalAnswer
(define (run-inp str)
  (value-of-program-inp
    (scan&parse-inp str)))

;value-of-program-inp : InpExp -> FinalAnswer
(define (value-of-program-inp pgm)
  (cases inppgm pgm
    [a-inppgm [expr]
      (value-of/k-inp expr (init-env)
        (lambda (val) val))]))

;value-of/k-inp : InpExp x Env -> FinalAnswer
(define (value-of/k-inp expr env cont)
  (cases inpexp expr
    [inp-const-exp [num]
      (cont (num-val num))]
    [inp-var-exp [var]
      (cont (apply-env env var))]
    [inp-diff-exp [inp1 inp2]
      (value-of/k-inp inp1 env
        (lambda (val1)
          (value-of/k-inp inp2 env
            (lambda (val2)
              (cont (num-val (- (expval->num val1) (expval->num val2))))))))]
    [inp-zero?-exp [inp1]
      (value-of/k-inp inp1 env
        (lambda (val1)
          (cont (bool-val (zero? (expval->num val1))))))]
    [inp-if-exp [inp1 inp2 inp3]
      (value-of/k-inp inp1 env
        (lambda (val1)
          (if (expval->bool val1)
            (value-of/k-inp inp2 env cont)
            (value-of/k-inp inp3 env cont))))]
    [inp-let-exp [vars inps body]
      (if (null? vars)
        (value-of/k-inp body env cont)
        (let loop([inps inps] [vals '()])
          (if (null? inps)
            (value-of/k-inp body (extend-env vars (reverse vals) env) cont)
            (value-of/k-inp (car inps) env
              (lambda (val)
                (loop (cdr inps) (cons val vals)))))))]
    [inp-letrec-exp [names varss procs rbody]
      (value-of/k-inp rbody (extend-env-rec names varss procs env) cont)]
    [inp-proc-exp [vars body]
      (cont (proc-val (procedure vars body env)))]
    [inp-call-exp [rator rands]
      (value-of/k-inp rator env
        (lambda (rator-val)
          (let loop([rands rands] [vals '()])
            (if (null? rands)
              (cases proc (expval->proc rator-val)
                [procedure [vars body env]
                  (value-of/k-inp body (extend-env vars (reverse vals) env) cont)])
              (value-of/k-inp (car rands) env
                (lambda (rand-val)
                  (loop (cdr rands) (cons rand-val vals))))))))]
    ))
