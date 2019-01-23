(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")
(require racket/trace)

(define (run str)
  (value-of-program
    (scan&parse str)))

; value-of-program : Program -> ExpVal
(define (value-of-program pgm)
  (cases program pgm
    [a-program [expr]
      (value-of/k expr (init-env) (end-cont) (end-cont))]))

; value-of/k : Exp x Env x Cont x Cont -> ExpVal
(define (value-of/k expr env cont excp-cont)
  (cases expression expr
    [const-exp [num]
      (apply-cont cont (num-val num) excp-cont)]
    [var-exp [var]
      (apply-cont cont (apply-env env var) excp-cont)]
    [proc-exp [vars body]
      (apply-cont cont (proc-val (procedure vars body env)) excp-cont)]
    [letrec-exp [names varss bodies letrec-body]
      (value-of/k letrec-body (extend-env-rec names varss bodies env) cont excp-cont)]
    [zero?-exp [exp1]
      (value-of/k exp1 env
        (zero?-cont cont) excp-cont)]
    [let-exp [vars exps body]
      (if (null? vars)
        (value-of/k body env cont excp-cont)
        (value-of/k (car exps) env
          (let-cont vars (cdr exps) '() body env cont) excp-cont))]
    [if-exp [exp1 exp2 exp3]
      (value-of/k exp1 env
        (if-test-cont exp2 exp3 env cont) excp-cont)]
    [diff-exp [exp1 exp2]
      (value-of/k exp1 env
        (diff1-cont exp2 env cont) excp-cont)]
    [multi-exp [exp1 exp2]
      (value-of/k exp1 env
        (multi1-cont exp2 env cont) excp-cont)]
    [call-exp [rator rands]
      (value-of/k rator env
        (rator-cont rands env cont) excp-cont)]
    [list-exp [exps]
      (if (null? exps)
        (apply-cont cont (list-val '()) excp-cont)
        (value-of/k (car exps) env
          (list-cont (cdr exps) '() env cont) excp-cont))]
    [car-exp [exp1]
      (value-of/k exp1 env
        (car-cont cont) excp-cont)]
    [cdr-exp [exp1]
      (value-of/k exp1 env
        (cdr-cont cont) excp-cont)]
    [null?-exp [exp1]
      (value-of/k exp1 env
        (null?-cont cont) excp-cont)]
    [try-exp [exp1 var handler-exp]
      (let ([curr-cont (try-cont var handler-exp env cont)])
        (value-of/k exp1 env
          curr-cont (excp1-cont curr-cont excp-cont)))]
    [raise-exp [exp1]
      (value-of/k exp1 env
        (raise1-cont cont) excp-cont)]
    ))

; apply-cont : Cont x ExpVal x Cont -> ExpVal
(define (apply-cont cont1 val excp-cont)
  (cases cont cont1
    [end-cont []
      (begin (eopl:printf "End of computation.~%") val)]
    [zero?-cont [saved-cont]
      (apply-cont saved-cont
        (bool-val (zero? (expval->num val))) excp-cont)]
    [let-cont [vars exps vals body saved-env saved-cont]
      (let ([vals (cons val vals)])
        (if (null? exps)
          (value-of/k body (extend-env vars (reverse vals) saved-env) saved-cont excp-cont)
          (value-of/k (car exps) saved-env
            (let-cont vars (cdr exps) vals body saved-env saved-cont) excp-cont)))]
    [if-test-cont [exp2 exp3 saved-env saved-cont]
      (if (expval->bool val)
        (value-of/k exp2 saved-env saved-cont excp-cont)
        (value-of/k exp3 saved-env saved-cont excp-cont))]
    [diff1-cont [exp2 saved-env saved-cont]
      (value-of/k exp2 saved-env
        (diff2-cont val saved-cont) excp-cont)]
    [diff2-cont [val1 saved-cont]
      (let ([num1 (expval->num val1)] [num2 (expval->num val)])
        (apply-cont saved-cont (num-val (- num1 num2)) excp-cont))]
    [multi1-cont [exp2 saved-env saved-cont]
      (value-of/k exp2 saved-env
        (multi2-cont val saved-cont) excp-cont)]
    [multi2-cont [val1 saved-cont]
      (let ([num1 (expval->num val1)] [num2 (expval->num val)])
        (apply-cont saved-cont (num-val (* num1 num2)) excp-cont))]
    [rator-cont [rands saved-env saved-cont]
      (if (null? rands)
        (apply-proc/k (expval->proc val) '() saved-cont excp-cont)
        (value-of/k (car rands) saved-env
          (rands-cont val (cdr rands) '() saved-env saved-cont) excp-cont))]
    [rands-cont [rator rands vals saved-env saved-cont]
      (let ([vals (cons val vals)])
        (if (null? rands)
          (apply-proc/k (expval->proc rator) (reverse vals) saved-cont excp-cont)
          (value-of/k (car rands) saved-env
            (rands-cont rator (cdr rands) vals saved-env saved-cont) excp-cont)))]
    [list-cont [exps vals saved-env saved-cont]
      (let ([vals (cons val vals)])
        (if (null? exps)
          (apply-cont saved-cont (list-val (reverse vals)) excp-cont)
          (value-of/k (car exps) saved-env
            (list-cont (cdr exps) vals saved-env saved-cont) excp-cont)))]
    [car-cont [saved-cont]
      (apply-cont saved-cont (car (expval->list val)) excp-cont)]
    [cdr-cont [saved-cont]
      (apply-cont saved-cont (list-val (cdr (expval->list val))) excp-cont)]
    [null?-cont [saved-cont]
      (apply-cont saved-cont (bool-val (null? (expval->list val))) excp-cont)]
    [try-cont [var handler-exp env saved-cont]
      (apply-cont saved-cont val
        (cases cont excp-cont
          [excp1-cont [curr-cont rest-cont] rest-cont]
          [else (report-invalid-exception excp-cont)]))]
    [raise1-cont [saved-cont]
      (apply-handler val excp-cont)]
    [else
      (report-invalid-cont 'apply-cont cont1 val1)]
    ))

; apply-proc/k : Proc x List(ExpVal) x Cont x Cont -> ExpVal
(define (apply-proc/k proc1 vals cont excp-cont)
  (cases proc proc1
    [procedure [vars body saved-env]
      (value-of/k body (extend-env vars vals saved-env) cont excp-cont)]))

; apply-handler : ExpVal x Cont -> ExpVal
(define (apply-handler val cont1)
  (cases cont cont1
    [excp1-cont [curr-cont rest-cont]
      (cases cont curr-cont
        [try-cont [var handler-exp saved-env saved-cont]
          (value-of/k handler-exp (extend-env (list var) (list val) saved-env)
             saved-cont rest-cont)]
        [else (report-invalid-exception-continuation cont1)])]
    [end-cont []
      (report-uncaught-exception)]
    [else (report-invalid-exception-continuation cont1)]
    ))

;(trace apply-env)
;(trace apply-proc/k)
;(trace value-of/k)
;(trace apply-cont)

; res = (num-val 1)
(define p1
  "letrec
     even(x) = if zero?(x) then 1 else (odd  -(x,1))
     odd(x)  = if zero?(x) then 0 else (even -(x,1))
   in (odd 13)")

; res = (list-val (1 2))
(define p2
  "let x = list(1, 2, 3)
   in let a = car(x) b = cdr(x)
      in let c = car(b)
         in list(a, c)")

; res = (num-val -1)
(define p3
  "let index = proc(n)
                 letrec inner(lst)
                   = if null?(lst)
                     then raise 99
                     else if zero?(-(car(lst), n))
                          then 0
                          else -((inner cdr(lst)), -1)
                 in proc(lst)
                      try (inner lst)
                      catch (x) -1
   in ((index 5) list(2, 3))")

; res = (list-val (list (num-val 10) (num-val 20)))
(define p4
  "try
     list(
       try 10 catch(e2) e2,
       try 20 catch(e3) e3
     )
   catch(e1)
     e1")
