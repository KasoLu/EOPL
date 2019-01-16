(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")
(require racket/trace)

(define (run str)
  (value-of-program
    (scan&parse str)))

; value-of-program : Program -> FinalAnswer
(define (value-of-program pgm)
  (cases program pgm
    [a-program [expr]
      (value-of/k expr (init-env) (end-cont))]))

; value-of/k : Exp x Env x Cont -> FinalAnswer
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
      (value-of/k (car exps) env
        (let-cont vars body env cont))]
    [if-exp [exp1 exp2 exp3]
      (value-of/k exp1 env
        (if-test-cont exp2 exp3 env cont))]
    [diff-exp [exp1 exp2]
      (value-of/k exp1 env
        (diff1-cont exp2 env cont))]
    [call-exp [rator rands]
      (value-of/k rator env
        (rator-cont rands env cont))]
    ))

; FinalAnswer = ExpVal
; apply-cont : Cont x ExpVal -> FinalAnswer
(define (apply-cont cont1 val)
  (cases cont cont1
    [end-cont []
      (begin (eopl:printf "End of computation.~%")
             val)]
    [zero?-cont [saved-cont]
      (apply-cont saved-cont
        (bool-val (zero? (expval->num val))))]
    [let-cont [vars body saved-env saved-cont]
      (value-of/k body
        (extend-env vars (list val) saved-env) saved-cont)]
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
    [rator-cont [rands saved-env saved-cont]
      (value-of/k (car rands) saved-env
        (rands-cont val saved-cont))]
    [rands-cont [val1 saved-cont]
      (let ([proc1 (expval->proc val1)])
        (apply-proc/k proc1 (list val) saved-cont))]
    ))

(define (apply-proc/k proc1 vals cont)
  (cases proc proc1
    [procedure [vars body saved-env]
      (value-of/k body (extend-env vars vals saved-env) cont)]))

;(trace apply-env)
;(trace apply-proc/k)
;(trace value-of/k)

; res = (num-val 1)
(define p
  "letrec
     even(x) = if zero?(x) then 1 else (odd  -(x,1))
     odd(x)  = if zero?(x) then 0 else (even -(x,1))
   in (odd 13)")

