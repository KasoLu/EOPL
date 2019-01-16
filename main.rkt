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
(define (apply-cont cont val)
  (cont val))

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

