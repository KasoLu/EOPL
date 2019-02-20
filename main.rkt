(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")

;run-inp : String -> FinalAnswer
(define (run-inp str)
  (value-of-inppgm
    (scan&parse-inp str)))

;value-of-pgm-inp : InpPgm -> FinalAnswer
(define (value-of-inppgm pgm)
  (cases inppgm pgm
    [a-inppgm [expr]
      (value-of-inpexp expr (init-env)
        (lambda (val) val))]))

;value-of-inpexp : InpExp x Env x Cont -> FinalAnswer
(define (value-of-inpexp expr env cont)
  (cases inpexp expr
    [inp-const-exp [num]
      (cont (num-val num))]
    [inp-var-exp [var]
      (cont (apply-env env var))]
    [inp-diff-exp [inp1 inp2]
      (value-of-inpexp inp1 env
        (lambda (val1)
          (value-of-inpexp inp2 env
            (lambda (val2)
              (cont (num-val (- (expval->num val1) (expval->num val2))))))))]
    [inp-zero?-exp [inp1]
      (value-of-inpexp inp1 env
        (lambda (val1)
          (cont (bool-val (zero? (expval->num val1))))))]
    [inp-if-exp [inp1 inp2 inp3]
      (value-of-inpexp inp1 env
        (lambda (val1)
          (if (expval->bool val1)
            (value-of-inpexp inp2 env cont)
            (value-of-inpexp inp3 env cont))))]
    [inp-let-exp [vars inps body]
      (let loop ([inps inps] [vals '()])
        (if (null? inps)
          (let ([ext-env (if (null? vars) env (extend-env vars (reverse vals) env))])
            (value-of-inpexp body ext-env cont))
          (value-of-inpexp (car inps) env
            (lambda (val)
              (loop (cdr inps) (cons val vals))))))]
    [inp-letrec-exp [names varss procs rbody]
      (value-of-inpexp rbody (extend-env-rec names varss procs env) cont)]
    [inp-proc-exp [vars body]
      (cont (proc-val (procedure vars body env)))]
    [inp-call-exp [rator rands]
      (value-of-inpexp rator env
        (lambda (rator-val)
          (let loop ([rands rands] [vals '()])
            (if (null? rands)
              (cases proc (expval->proc rator-val)
                [procedure [vars body saved-env]
                  (value-of-inpexp body (extend-env vars (reverse vals) saved-env) cont)])
              (value-of-inpexp (car rands) env
                (lambda (rand-val)
                  (loop (cdr rands) (cons rand-val vals))))))))]
    [inp-sum-exp [inps]
      (let loop ([inps inps] [vals '()])
        (if (null? inps)
          (cont (num-val (foldl (lambda (x res) (+ (expval->num x) res)) 0 vals)))
          (value-of-inpexp (car inps) env
            (lambda (val)
              (loop (cdr inps) (cons val vals))))))]
    ))

;inp-is-simple? : InpExp -> Bool
(define (inp-is-simple? expr)
  (cases inpexp expr
    [inp-const-exp [num] #t]
    [inp-var-exp [var] #t]
    [inp-diff-exp [exp1 exp2] (and (inp-is-simple? exp1) (inp-is-simple? exp2))]
    [inp-zero?-exp [exp1] (inp-is-simple? exp1)]
    [inp-proc-exp [vars body] #t]
    [inp-sum-exp [exps] (every? inp-is-simple? exps)]
    [else #f]
    ))

;anf-of-inps : List(InpExp) x (List(InpExp) -> InpExp) -> InpExp
(define (anf-of-inps inps ctx)
  (let anf-of-rest ([inps inps] [acc '()])
    (cond [(null? inps)
           (ctx (reverse acc))]
          [(inp-is-simple? (car inps))
           (anf-of-rest (cdr inps) (cons (anf-of-inp (car inps)) acc))]
          [else
           (let ([val-var (fresh-identifier 'val)])
             (inp-let-exp
               (list val-var)
               (list (anf-of-inp (car inps)))
               (anf-of-rest (cdr inps) (cons (inp-var-exp val-var) acc))))])))

;anf-of-inp : InpExp -> InpExp
(define (anf-of-inp inp)
  (cases inpexp inp
    [inp-diff-exp [inp1 inp2]
      (anf-of-inps (list inp1 inp2)
        (lambda (smps)
          (inp-diff-exp (car smps) (cadr smps))))]
    [inp-zero?-exp [inp1]
      (anf-of-inps (list inp1)
        (lambda (smps)
          (inp-zero?-exp (car smps))))]
    [inp-if-exp [inp1 inp2 inp3]
      (anf-of-inps (list inp1)
        (lambda (smps)
          (inp-if-exp (car smps) (anf-of-inp inp2) (anf-of-inp inp3))))]
    [inp-let-exp [vars inps body]
      (anf-of-inps inps
        (lambda (smps)
          (inp-let-exp vars smps (anf-of-inp body))))]
    [inp-letrec-exp [names varss procs rbody]
      (inp-letrec-exp names varss (map anf-of-inp procs) (anf-of-inp rbody))]
    [inp-proc-exp [vars body]
      (inp-proc-exp vars (anf-of-inp body))]
    [inp-call-exp [rator rands]
      (anf-of-inps (cons rator rands)
        (lambda (smps)
          (inp-call-exp (car smps) (cdr smps))))]
    [inp-sum-exp [inps]
      (anf-of-inps inps
        (lambda (smps)
          (inp-sum-exp smps)))]
    [else inp]))

;run-anf : String -> FinalAnswer
(define (run-anf str)
  (value-of-anfpgm
    (scan&parse-inp str)))

;value-of-anfpgm : InpPgm -> FinalAnswer
(define (value-of-anfpgm pgm)
  (cases inppgm pgm
    [a-inppgm [inp]
      (value-of-inpexp (anf-of-inp inp) (init-env)
        (lambda (val) val))]))
