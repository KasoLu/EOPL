(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")
(require racket/trace)

; run : String -> ExpVal
(define (run str)
  (value-of-program
    (scan&parse str)))

; value-of-program : Program -> ExpVal
(define (value-of-program pgm)
  (cases program pgm
    [a-program [expr]
      (value-of/k expr (init-env)
        (lambda (val)
          (begin (eopl:printf "End of computation.~%") val)))]))

; value-of/k : Exp x Env x Cont -> ExpVal
(define (value-of/k expr env cont)
  (cases expression expr
    [const-exp [num]
      (cont (num-val num))]
    [var-exp [var]
      (cont (apply-env env var))]
    [proc-exp [vars body]
      (cont (proc-val (procedure vars body env)))]
    [letrec-exp [names varss bodies letrec-body]
      (value-of/k letrec-body (extend-env-rec names varss bodies env) cont)]
    [zero?-exp [exp1]
      (value-of/k exp1 env
        (lambda (val)
          (cont (bool-val (zero? (expval->num val))))))]
    [let-exp [vars exps body]
      (if (null? vars)
        (value-of/k body env cont)
        (let loop([exps exps] [vals '()])
          (if (null? exps)
            (value-of/k body (extend-env vars (reverse vals) env) cont)
            (value-of/k (car exps) env
              (lambda (val)
                (loop (cdr exps) (reverse val vals)))))))]
    [if-exp [exp1 exp2 exp3]
      (value-of/k exp1 env
        (lambda (val1)
          (if (expval->bool val1)
            (value-of/k exp2 env cont)
            (value-of/k exp3 env cont))))]
    [diff-exp [exp1 exp2]
      (value-of/k exp1 env
        (lambda (val1)
          (value-of/k exp2 env
            (lambda (val2)
              (cont (num-val (- (expval->num val1) (expval->num val2))))))))]
    [call-exp [rator rands]
      (value-of/k rator env
        (lambda (rator-val)
          (let loop([rands rands] [vals '()])
            (if (null? rands)
              (apply-proc/k (expval->proc rator-val) (reverse vals) cont)
              (value-of/k (car rands) env
                (lambda (rand-val)
                  (loop (cdr rands) (cons rand-val vals))))))))]))

; apply-proc/k : Proc x List(ExpVal) x Cont -> ExpVal
(define (apply-proc/k proc1 vals cont)
  (cases proc proc1
    [procedure [vars body saved-env]
      (value-of/k body (extend-env vars vals saved-env) cont)]))

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

