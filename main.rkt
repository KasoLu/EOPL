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
      (value-of/k expr (init-env) (end-cont))]))

; value-of/k : Exp x Env x Cont -> ExpVal
(define (value-of/k expr env cont)
  (cases expression expr
    [const-exp [num]
      (apply-cont cont (num-val num))]
    [var-exp [var]
      (apply-cont cont (apply-env env var))]
    [proc-exp [vars body]
      (apply-cont cont (proc-val (procedure vars body env)))]
    [letrec-exp [names varss bodies letrec-body]
      (value-of/k letrec-body (extend-env-rec names varss bodies env) cont)]
    [zero?-exp [exp1]
      (value-of/k exp1 env
        (zero?-cont cont))]
    [let-exp [vars exps body]
      (if (null? vars)
        (value-of/k body env cont)
        (value-of/k (car exps) env
          (let-cont vars (cdr exps) '() body env cont)))]
    [if-exp [exp1 exp2 exp3]
      (value-of/k exp1 env
        (if-test-cont exp2 exp3 env cont))]
    [diff-exp [exp1 exp2]
      (value-of/k exp1 env
        (diff1-cont exp2 env cont))]
    [multi-exp [exp1 exp2]
      (value-of/k exp1 env
        (multi1-cont exp2 env cont))]
    [call-exp [rator rands]
      (value-of/k rator env
        (rator-cont rands env cont))]
    [list-exp [exps]
      (if (null? exps)
        (apply-cont cont (list-val '()))
        (value-of/k (car exps) env
          (list-cont (cdr exps) '() env cont)))]
    [car-exp [exp1]
      (value-of/k exp1 env
        (car-cont cont))]
    [cdr-exp [exp1]
      (value-of/k exp1 env
        (cdr-cont cont))]
    [null?-exp [exp1]
      (value-of/k exp1 env
        (null?-cont cont))]
    [try-exp [exp1 var handler-exp]
      (value-of/k exp1 env
        (try-cont var handler-exp env cont))]
    [raise-exp [exp1]
      (value-of/k exp1 env
        (raise1-cont cont))]
    [div-exp [exp1 exp2]
      (value-of/k exp1 env
        (div1-cont exp2 env cont))]
    ))

; apply-cont : Cont x ExpVal -> ExpVal
(define (apply-cont cont1 val)
  (cases cont cont1
    [end-cont []
      (begin (eopl:printf "End of computation.~%") val)]
    [zero?-cont [saved-cont]
      (apply-cont saved-cont
        (bool-val (zero? (expval->num val))))]
    [let-cont [vars exps vals body saved-env saved-cont]
      (let ([vals (cons val vals)])
        (if (null? exps)
          (value-of/k body (extend-env vars (reverse vals) saved-env) saved-cont)
          (value-of/k (car exps) saved-env
            (let-cont vars (cdr exps) vals body saved-env saved-cont))))]
    [if-test-cont [exp2 exp3 saved-env saved-cont]
      (if (expval->bool val)
        (value-of/k exp2 saved-env saved-cont)
        (value-of/k exp3 saved-env saved-cont))]
    [diff1-cont [exp2 saved-env saved-cont]
      (value-of/k exp2 saved-env
        (diff2-cont val saved-cont))]
    [diff2-cont [val1 saved-cont]
      (let ([num1 (expval->num val1)] [num2 (expval->num val)])
        (apply-cont saved-cont (num-val (- num1 num2))))]
    [multi1-cont [exp2 saved-env saved-cont]
      (value-of/k exp2 saved-env
        (multi2-cont val saved-cont))]
    [multi2-cont [val1 saved-cont]
      (let ([num1 (expval->num val1)] [num2 (expval->num val)])
        (apply-cont saved-cont (num-val (* num1 num2))))]
    [rator-cont [rands saved-env saved-cont]
      (cases proc (expval->proc val)
        [procedure [vars body saved-env2]
          (cond [(not (= (length vars) (length rands)))
                 (apply-handler (num-val -1) saved-cont saved-cont)]
                [(null? rands)
                 (value-of/k body saved-env2 saved-cont)]
                [else
                 (value-of/k (car rands) saved-env
                   (rands-cont val (cdr rands) '() saved-env saved-cont))])])]
    [rands-cont [rator rands vals saved-env saved-cont]
      (let ([vals (cons val vals)])
        (if (null? rands)
          (cases proc (expval->proc rator)
            [procedure [vars body saved-env2]
              (value-of/k body (extend-env vars (reverse vals) saved-env2) saved-cont)])
          (value-of/k (car rands) saved-env
            (rands-cont rator (cdr rands) vals saved-env saved-cont))))]
    [list-cont [exps vals saved-env saved-cont]
      (let ([vals (cons val vals)])
        (if (null? exps)
          (apply-cont saved-cont (list-val (reverse vals)))
          (value-of/k (car exps) saved-env
            (list-cont (cdr exps) vals saved-env saved-cont))))]
    [car-cont [saved-cont]
      (apply-cont saved-cont (car (expval->list val)))]
    [cdr-cont [saved-cont]
      (apply-cont saved-cont (list-val (cdr (expval->list val))))]
    [null?-cont [saved-cont]
      (apply-cont saved-cont (bool-val (null? (expval->list val))))]
    [try-cont [var handler-exp env saved-cont]
      (apply-cont saved-cont val)]
    [raise1-cont [saved-cont]
      (apply-handler val saved-cont saved-cont)]
    [div1-cont [exp2 saved-env saved-cont]
      (value-of/k exp2 saved-env
        (div2-cont val saved-cont))]
    [div2-cont [val1 saved-cont]
      (let ([num1 (expval->num val1)] [num2 (expval->num val)])
        (if (zero? num2)
          (apply-handler (num-val -1) saved-cont saved-cont)
          (apply-cont saved-cont (num-val (/ num1 num2)))))]
    [else
      (report-invalid-cont 'apply-cont cont1 val1)]
    ))

; apply-handler : ExpVal x Cont -> ExpVal
(define (apply-handler val cont1 curr-cont)
  (cases cont cont1
    [try-cont [var handler-exp saved-env saved-cont]
      (value-of/k handler-exp (extend-env (list var) (list val) saved-env) curr-cont)]
    [end-cont []
      (report-uncaught-exception)]
    [zero?-cont [saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [let-cont [vars exps vals body saved-env saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [if-test-cont [exp2 exp3 saved-env saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [diff1-cont [exp2 saved-env saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [diff2-cont [val1 saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [multi1-cont [exp2 saved-env saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [multi2-cont [val1 saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [rator-cont [rands saved-env saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [rands-cont [rator rands vals saved-env saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [list-cont [exps vals saved-env saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [car-cont [saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [cdr-cont [saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [null?-cont [saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [raise1-cont [saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [div1-cont [exp2 saved-env saved-cont]
      (apply-handler val saved-cont curr-cont)]
    [div2-cont [val1 saved-cont]
      (apply-handler val saved-cont curr-cont)]
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

; res = (num-val -1)
(define p4
  "let f = proc(x y z) *(x, *(y, z)) in 
   try
     (f 1 2)
   catch(e)
     e")

; res = (num-val -1)
(define p5
  "try
     /(10, 0)
   catch(e)
     e")

; res = (num-val 30)
(define p6
  "try
     let x = 10 y = raise 20 in y
   catch(e)
     -(e, -10)")

