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

;inppgm->cpspgm : InpPgm x Cont -> CpsPgm
(define (inppgm->cpspgm pgm cont)
  (cases inppgm pgm
    [a-inppgm [inpexp]
      (inpexp->tsfexp inpexp
        (lambda (tsf) (cont (a-cpspgm tsf))))]))

;inpexp->smpexp : InpExp x Cont -> SmpExp
(define (inpexp->smpexp inp cont)
  (cases inpexp inp
    [inp-const-exp [num]
      (cont (smp-const-exp num))]
    [inp-var-exp [var]
      (cont (smp-var-exp var))]
    [inp-diff-exp [inp1 inp2]
      (inpexp->smpexp inp1
        (lambda (smp1)
          (inpexp->smpexp inp2
            (lambda (smp2)
              (cont (smp-diff-exp smp1 smp2))))))]
    [inp-zero?-exp [inp1]
      (inpexp->smpexp inp1
        (lambda (smp1)
          (cont (smp-zero?-exp smp1))))]
    [inp-proc-exp [vars body-inp]
      (inpexp->tsfexp body-inp
        (lambda (tsf1)
          (cont (smp-proc-exp vars tsf1))))]
    [else
      (eopl:printf "Error inpexp: ~a\n" inp)]
    ))

;inpexp->tsfexp : InpExp x Cont -> TsfExp
(define (inpexp->tsfexp inp cont)
  (cases inpexp inp
    [inp-if-exp [inp1 inp2 inp3]
      (inpexp->smpexp inp1
        (lambda (smp1)
          (inpexp->tsfexp inp2
            (lambda (tsf2)
              (inpexp->tsfexp inp3
                (lambda (tsf3)
                  (cont (tsf-if-exp smp1 tsf2 tsf3))))))))]
    [inp-let-exp [vars inps body-inp]
      (inpexp->tsfexp body-inp
        (lambda (body-tsf)
          (let loop([inps inps] [smps '()])
            (if (null? inps)
              (cont (tsf-let-exp vars (reverse smps) body-tsf))
              (inpexp->smpexp (car inps)
                (lambda (smp)
                  (loop (cdr inps) (cons smp smps))))))))]
    [inp-letrec-exp [names varss procs-inp rbody-inp]
      (inpexp->tsfexp rbody-inp
        (lambda (rbody-tsf)
          (let loop([procs-inp procs-inp] [procs-tsf '()])
            (if (null? procs-inp)
              (cont (tsf-letrec-exp names varss (reverse procs-tsf) rbody-tsf))
              (inpexp->tsfexp (car procs-inp)
                (lambda (proc-tsf)
                  (loop (cdr procs-inp) (cons proc-tsf procs-tsf))))))))]
    [inp-call-exp [rator-inp rands-inp]
      (inpexp->smpexp rator-inp
        (lambda (rator-smp)
          (let loop([rands-inp rands-inp] [rands-smp '()])
            (if (null? rands-inp)
              (cont (tsf-call-exp rator-smp (reverse rands-smp)))
              (inpexp->smpexp (car rands-inp)
                (lambda (rand-smp)
                  (loop (cdr rands-inp) (cons rand-smp rands-smp))))))))]
    [else
      (inpexp->smpexp inp
        (lambda (smp)
          (cont (smpexp->tsfexp smp))))]
    ))

;inp->cps-run : String -> FinalAnswer
(define (inp->cps-run str)
  (inppgm->cpspgm (scan&parse-inp str)
    (lambda (cpspgm)
      (value-of-program-cps cpspgm))))
