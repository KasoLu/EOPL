(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")
(require racket/trace)

(define r-pgm1 'uninit)
(define r-env1 'uninit)
(define r-cont 'uninit)
(define r-expr 'uninit)
(define r-val1 'uninit)
(define r-com1 'uninit)
(define r-com2 'uninit)
(define r-com3 'uninit)
(define r-com4 'uninit)
(define r-com5 'uninit)

;run-cps : String -> FinalAnswer
(define (run-cps str)
  (set! r-pgm1 (scan&parse-cps str))
  (value-of-program-cps))

;value-of-program-cps : CpsPgm -> FinalAnswer
(define (value-of-program-cps)
  (cases cpspgm r-pgm1
    [a-cpspgm [expr]
      (set! r-env1 (init-env))
      (set! r-cont (lambda () r-val1))
      (set! r-expr expr)
      (value-of/k-cps)]))

;value-of/k-cps : TsfExp x Env x Cont -> FinalAnswer
(define (value-of/k-cps)
  (cases tsfexp r-expr
    [smpexp->tsfexp [smpexp]
      (set! r-expr smpexp)
      (value-of-smpexp)
      (r-cont)]
    [tsf-let-exp [vars exps body]
      (set! r-com1 exps)
      (set! r-com2 '())
      (let loop()
        (if (null? r-com1)
          (begin (set! r-env1 (extend-env vars (reverse r-com2) r-env1))
                 (set! r-expr body)
                 (value-of/k-cps))
          (begin (set! r-expr (car r-com1))
                 (set! r-com3 r-com1)
                 (set! r-com4 r-com2)
                 (value-of-smpexp)
                 (set! r-com2 r-com4)
                 (set! r-com1 r-com3)
                 (set! r-com2 (cons r-val1 r-com2))
                 (set! r-com1 (cdr r-com1))
                 (loop))))]
    [tsf-letrec-exp [names varss procs rbody]
      (set! r-env1 (extend-env-rec names varss procs r-env1))
      (set! r-expr rbody)
      (value-of/k-cps)]
    [tsf-if-exp [smp1 tsf2 tsf3]
      (set! r-expr smp1)
      (value-of-smpexp)
      (if (expval->bool r-val1)
        (set! r-expr tsf2) 
        (set! r-expr tsf3))
      (value-of/k-cps)]
    [tsf-call-exp [rator rands]
      (set! r-expr rator)
      (value-of-smpexp)
      (set! r-com5 (expval->proc r-val1))
      (set! r-com1 rands)
      (set! r-com2 '())
      (let loop()
        (if (null? r-com1)
          (cases proc r-com5
            [procedure [vars body env]
              (set! r-env1 (extend-env vars (reverse r-com2) env))
              (set! r-expr body)
              (value-of/k-cps)])
          (begin (set! r-expr (car r-com1))
                 (set! r-com3 r-com1)
                 (set! r-com4 r-com2)
                 (value-of-smpexp)
                 (set! r-com2 r-com4)
                 (set! r-com1 r-com3)
                 (set! r-com2 (cons r-val1 r-com2))
                 (set! r-com1 (cdr r-com1))
                 (loop))))]
    ))

;value-of-smpexp : SmpExp x Env -> FinalAnswer
(define (value-of-smpexp)
  (cases smpexp r-expr
    [smp-const-exp [num]
      (set! r-val1 (num-val num))]
    [smp-var-exp [var]
      (set! r-val1 (apply-env r-env1 var))]
    [smp-diff-exp [smp1 smp2]
      (set! r-expr smp1)
      (value-of-smpexp)
      (set! r-com1 (expval->num r-val1))
      (set! r-expr smp2)
      (value-of-smpexp)
      (set! r-com2 (expval->num r-val1))
      (set! r-val1 (num-val (- r-com1 r-com2)))]
    [smp-zero?-exp [smp1]
      (set! r-expr smp1)
      (value-of-smpexp)
      (set! r-val1 (bool-val (zero? (expval->num r-val1))))]
    [smp-proc-exp [vars body]
      (set! r-val1 (proc-val (procedure vars body r-env)))]
    ))

