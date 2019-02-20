(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")

;run-out : String -> FinalAnswer
(define (run-out str)
  (value-of-pgm-out
    (scan&parse-out str)))

;value-of-pgm-out : OutPgm -> FinalAnswer
(define (value-of-pgm-out pgm)
  (cases outpgm pgm
    [a-outpgm [expr]
      (value-of-tsfexp expr (init-env)
        (lambda (val) val))]))

;value-of-tsfexp : TsfExp x Env x Cont -> FinalAnswer
(define (value-of-tsfexp expr env cont)
  (cases tsfexp expr
    [smpexp->tsfexp [smpexp]
      (cont (value-of-smpexp smpexp env))]
    [tsf-let-exp [vars exps body]
      (let ([vals (map (lambda (x) (value-of-smpexp x env)) exps)])
        (value-of-tsfexp body (extend-env vars vals env) cont))]
    [tsf-letrec-exp [names varss procs rbody]
      (value-of-tsfexp rbody (extend-env-rec names varss procs env) cont)]
    [tsf-if-exp [smp1 exp2 exp3]
      (if (expval->bool (value-of-smpexp smp1 env))
        (value-of-tsfexp exp2 env cont)
        (value-of-tsfexp exp3 env cont))]
    [tsf-call-exp [rator rands]
      (let ([rator-proc (expval->proc (value-of-smpexp rator env))]
            [rands-vals (map (lambda (x) (value-of-smpexp x env)) rands)])
        (cases proc rator-proc
          [procedure [vars body env]
            (value-of-tsfexp body (extend-env vars rands-vals env) cont)]))]
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
    [smp-sum-exp [exps]
      (let loop ([exps exps] [vals '()])
        (if (null? exps)
          (num-val (foldl (lambda (x sum) (+ (expval->num x) sum)) 0 vals))
          (value-of-tsfexp (car exps) env
            (lambda (val)
              (loop (cdr exps) (cons val vals))))))]
    ))

;cps-of-exps: Listof(InpExp) x (Listof(SmpExp) -> TsfExp) -> TsfExp
(define (cps-of-exps exps builder)
  ;cps-of-rest : Listof(InpExp) -> TsfExp
  (let cps-of-rest ([exps exps])
    (let ([pos (list-index (lambda (e) (not (inp-exp-simple? e))) exps)])
      (if (not pos)
        (builder (map cps-of-simple-exp exps))
        (let ([var (fresh-identifier 'var)])
          (cps-of-exp
            (list-ref exps pos)
            (smp-proc-exp (list var)
              (cps-of-rest (list-set exps pos (inp-var-exp var))))))))))

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

;cps-of-exp : InpExp x SmpExp -> TfsExp
(define (cps-of-exp expr k-exp)
  (cases inpexp expr
    [inp-const-exp [num]
      (make-send-to-cont k-exp (smp-const-exp num))]
    [inp-var-exp [var]
      (make-send-to-cont k-exp (smp-var-exp var))]
    [inp-proc-exp [vars body]
      (make-send-to-cont k-exp
        (smp-proc-exp 
          (append vars (list 'k%00)) 
          (cps-of-exp body (smp-var-exp 'k%00))))]
    [inp-zero?-exp [exp1]
      (cps-of-exps (list exp1)
        (lambda (smps)
          (make-send-to-cont k-exp (smp-zero?-exp (car smps)))))]
    [inp-diff-exp [exp1 exp2]
      (cps-of-exps (list exp1 exp2)
        (lambda (smps)
          (make-send-to-cont k-exp (smp-diff-exp (car smps) (cadr smps)))))]
    [inp-if-exp [exp1 exp2 exp3]
      (cps-of-exps (list exp1)
        (lambda (smps)
          (tsf-if-exp (car smps)
            (cps-of-exp exp2 k-exp)
            (cps-of-exp exp3 k-exp))))]
    [inp-let-exp [vars exps body]
      (cps-of-exps exps
        (lambda (smps)
          (tsf-let-exp vars smps (cps-of-exp body k-exp))))]
    [inp-letrec-exp [names varss procs rbody]
      (tsf-letrec-exp
        names
        (map (lambda (vars) (append vars (list 'k%00))) varss)
        (map (lambda (proc) (cps-of-exp proc (smp-var-exp 'k%00))) procs)
        (cps-of-exp rbody k-exp))]
    [inp-call-exp [rator rands]
      (cps-of-exps (cons rator rands)
        (lambda (smps)
          (tsf-call-exp
            (car smps)
            (append (cdr smps) (list k-exp)))))]
    [inp-sum-exp [exps]
      (cps-of-exps exps
        (lambda (smps)
          (make-send-to-cont k-exp (smp-sum-exp (map smpexp->tsfexp smps)))))]
    ))

;cps-of-pgm : InpPgm -> CpsPgm
(define (cps-of-pgm pgm)
  (cases inppgm pgm
    [a-inppgm [expr]
      (a-outpgm 
        (cps-of-exps (list expr)
          (lambda (smps) (smpexp->tsfexp (car smps)))))]))

;run-cps : String -> FinalAnswer
(define (run-cps str)
  (value-of-pgm-out
    (cps-of-pgm
      (scan&parse-inp str))))

;make-send-to-cont : SmpExp x SmpExp -> TsfExp
(define (make-send-to-cont k-exp smp)
  (cases smpexp k-exp
    [smp-proc-exp [vars body]
      (tsf-replace-var-exp body vars (list smp))]
    [else
      (tsf-call-exp k-exp (list smp))]))

;tsf-replace-var-exp : TsfExp x List(Var) x SmpExp -> TsfExp
(define (tsf-replace-var-exp body vars targets)
  (cases tsfexp body
    [smpexp->tsfexp [smp]
      (smpexp->tsfexp (smp-replace-var-exp smp vars targets))]
    [tsf-let-exp [let-vars smp-exps tsf-body]
      (tsf-let-exp
        let-vars
        (map (lambda (x) (smp-replace-var-exp x vars targets)) smp-exps)
        tsf-body)]
    [tsf-if-exp [smp1 tsf2 tsf3]
      (tsf-if-exp
        (smp-replace-var-exp smp1 vars targets)
        tsf2
        tsf3)]
    [tsf-call-exp [rator rands]
      (tsf-call-exp
        (smp-replace-var-exp rator vars targets)
        (map (lambda (x) (smp-replace-var-exp x vars targets)) rands))]
    [else
      body]
    ))

;smp-replace-var-exp : SmpExp x List(Var) x SmpExp -> SmpExp
(define (smp-replace-var-exp body vars targets)
  (cases smpexp body
    [smp-var-exp [var]
      (let loop ([vars vars] [targets targets])
        (cond [(null? vars) body]
              [(eqv? (car vars) var) (car targets)]
              [else (loop (cdr vars) (cdr targets))]))]
    [smp-diff-exp [smp1 smp2]
      (smp-diff-exp
        (smp-replace-var-exp smp1 vars targets)
        (smp-replace-var-exp smp2 vars targets))]
    [smp-zero?-exp [smp1]
      (smp-zero?-exp
        (smp-replace-var-exp smp1 vars targets))]
    [smp-sum-exp [tsfs]
      (smp-sum-exp
        (map (lambda (x) (tsf-replace-var-exp x vars targets)) tsfs))]
    [else
      body]
    ))
