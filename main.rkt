(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")

;run-out : String -> FinalAnswer
(define (run-out str)
  (value-of-outpgm
    (scan&parse-out str)))

;value-of-outpgm : OutPgm -> FinalAnswer
(define (value-of-outpgm pgm)
  (init-store!)
  (cases outpgm pgm
    [a-outpgm [expr]
      (value-of-tpfexp expr (init-env)
        (lambda (val) val))]))

;value-of-tpfexp : TpfExp x Env x Cont -> FinalAnswer
(define (value-of-tpfexp expr env cont)
  (cases tpfexp expr
    [smpexp->tpfexp [smp]
      (cont (value-of-smpexp smp env))]
    [tpf-let-exp [vars smps body]
      (let ([vals (map (lambda (x) (value-of-smpexp x env)) smps)])
        (value-of-tpfexp body (extend-env vars vals env) cont))]
    [tpf-letrec-exp [names varss procs rbody]
      (value-of-tpfexp rbody (extend-env-rec names varss procs env) cont)]
    [tpf-if-exp [smp1 tpf2 tpf3]
      (if (expval->bool (value-of-smpexp smp1 env))
        (value-of-tpfexp tpf2 env cont)
        (value-of-tpfexp tpf3 env cont))]
    [tpf-call-exp [rator rands]
      (let ([rator-proc (expval->proc (value-of-smpexp rator env))]
            [rands-vals (map (lambda (x) (value-of-smpexp x env)) rands)])
        (apply-tpf-proc rator-proc rands-vals cont))]
    ))

;value-of-smpexp : SmpExp x Env -> FinalAnswer
(define (value-of-smpexp smp env)
  (cases smpexp smp
    [smp-const-exp [num]
      (num-val num)]
    [smp-var-exp [var]
      (apply-env env var)]
    [smp-diff-exp [smp1 smp2]
      (let ([val1 (value-of-smpexp smp1 env)] [val2 (value-of-smpexp smp2 env)])
        (num-val (- (expval->num val1) (expval->num val2))))]
    [smp-zero?-exp [smp1]
      (bool-val (zero? (expval->num (value-of-smpexp smp1 env))))]
    [smp-proc-exp [vars body]
      (proc-val (procedure vars body env))]
    [smp-sum-exp [smps]
      (num-val 
        (foldl (lambda (x res) (+ (expval->num (value-of-smpexp x env)) res)) 0 smps))]
    ))

;apply-tpf-proc : Proc x List(ExpVal) x Cont -> FinalAnswer
(define (apply-tpf-proc p args cont)
  (cases proc p
    [procedure [vars body env]
      (value-of-tpfexp body (extend-env vars args env) cont)]))

;cps-of-exps : List(InpExp) x List(SmpExp) x (List(SmpExp) -> TpfExp) -> TpfExp
(define (cps-of-exps inps e-exps ctx)
  (let cps-of-rest ([inps inps] [acc '()])
    (cond [(null? inps)
           (ctx (reverse acc))]
          [(inp-exp-simple? (car inps))
           (cps-of-rest (cdr inps) (cons (cps-of-simple-exp (car inps)) acc))]
          [else
           (let ([var (fresh-identifier 'var)])
             (cps-of-exp (car inps)
               (smp-proc-exp 
                 (list var) 
                 (cps-of-rest (cdr inps) (cons (smp-var-exp var) acc)))
               e-exps))])))

;inp-exp-simple? : InpExp -> Bool
(define (inp-exp-simple? expr)
  (cases inpexp expr
    [inp-const-exp [num] #t]
    [inp-var-exp [var] #t]
    [inp-diff-exp [exp1 exp2]
      (and (inp-exp-simple? exp1) (inp-exp-simple? exp2))]
    [inp-zero?-exp [exp1]
      (inp-exp-simple? exp1)]
    [inp-proc-exp [vars body] #t]
    [inp-sum-exp [exps] (every? inp-exp-simple? exps)]
    [else #f]
    ))

;cps-of-simple-exp : InpExp -> SmpExp
(define (cps-of-simple-exp expr)
  (cases inpexp expr
    [inp-const-exp [num] 
      (smp-const-exp num)]
    [inp-var-exp [var]
      (smp-var-exp var)]
    [inp-diff-exp [exp1 exp2]
      (smp-diff-exp
        (cps-of-simple-exp exp1)
        (cps-of-simple-exp exp2))]
    [inp-zero?-exp [exp1]
      (smp-zero?-exp (cps-of-simple-exp exp1))]
    [inp-proc-exp [vars body]
      (smp-proc-exp
        (append vars (list 'k%00))
        (cps-of-exp body (smp-var-exp 'k%00)))]
    [inp-sum-exp [exps]
      (smp-sum-exp (map cps-of-simple-exp exps))]
    [else
      (report-invalid-exp-to-cps-of-simple-exp expr)]))

;cps-of-exp : InpExp x SmpExp x List(SmpExp) -> TpfExp
(define (cps-of-exp expr k-exp e-exps)
  (cases inpexp expr
    [inp-const-exp [num]
      (make-send-to-cont k-exp (smp-const-exp num))]
    [inp-var-exp [var]
      (make-send-to-cont k-exp (smp-var-exp var))]
    [inp-proc-exp [vars body]
      (make-send-to-cont k-exp
        (smp-proc-exp 
          (append vars (list 'k%00)) 
          (cps-of-exp body (smp-var-exp 'k%00) e-exps)))]
    [inp-zero?-exp [exp1]
      (cps-of-exps (list exp1) e-exps
        (lambda (smps)
          (make-send-to-cont k-exp (smp-zero?-exp (car smps)))))]
    [inp-diff-exp [exp1 exp2]
      (cps-of-exps (list exp1 exp2) e-exps
        (lambda (smps)
          (make-send-to-cont k-exp (smp-diff-exp (car smps) (cadr smps)))))]
    [inp-if-exp [exp1 exp2 exp3]
      (cps-of-exps (list exp1) e-exps
        (lambda (smps)
          (tpf-if-exp (car smps)
            (cps-of-exp exp2 k-exp e-exps)
            (cps-of-exp exp3 k-exp e-exps))))]
    [inp-let-exp [vars exps body]
      (cps-of-exps exps e-exps
        (lambda (smps)
          (tpf-let-exp vars smps (cps-of-exp body k-exp e-exps))))]
    [inp-letrec-exp [names varss procs rbody]
      (tpf-letrec-exp
        names
        (map (lambda (vars) (append vars (list 'k%00))) varss)
        (map (lambda (proc) (cps-of-exp proc (smp-var-exp 'k%00) e-exps)) procs)
        (cps-of-exp rbody k-exp e-exps))]
    [inp-call-exp [rator rands]
      (cps-of-exps (cons rator rands) e-exps
        (lambda (smps)
          (tpf-call-exp
            (car smps)
            (append (cdr smps) (list k-exp)))))]
    [inp-sum-exp [exps]
      (cps-of-exps exps e-exps
        (lambda (smps)
          (make-send-to-cont k-exp (smp-sum-exp smps))))]
    [inp-try-exp [body var1 excp]
      (let ([excp-var (fresh-identifier 'excp)]
            [excp-smp (smp-proc-exp (list var1) (cps-of-exp excp k-exp e-exps))])
        (tpf-let-exp 
          (list excp-var)
          (list excp-smp)
          (cps-of-exp body k-exp (cons (smp-var-exp excp-var) e-exps))))]
    [inp-raise-exp [inp1]
      (cps-of-exps (list inp1) e-exps
        (lambda (smps)
          (make-send-to-cont (car e-exps) (car smps))))]
    ))

;cps-of-pgm : InpPgm -> CpsPgm
(define (cps-of-pgm pgm)
  (cases inppgm pgm
    [a-inppgm [expr]
      (a-outpgm 
        (cps-of-exps (list expr) '()
          (lambda (smps) (smpexp->tpfexp (car smps)))))]))

;run-cps : String -> FinalAnswer
(define (run-cps str)
  (init-store!)
  (value-of-outpgm
    (cps-of-pgm
      (scan&parse-inp str))))

;display-cps : String -> Void
(define (display-cps str)
  (pretty-display
    (cps-of-pgm
      (scan&parse-inp str))))

;make-send-to-cont : SmpExp x SmpExp -> TpfExp
(define (make-send-to-cont k-exp smp)
  (tpf-call-exp k-exp (list smp)))
