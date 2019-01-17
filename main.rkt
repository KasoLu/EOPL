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
    [call-exp [rator rands]
      (value-of/k rator env
        (rator-cont rands env cont))]
    ))

(define (end-cont) '())
(define (zero?-cont saved-cont)
  (cons (lambda (val cont)
          (apply-cont cont (bool-val (zero? (expval->num val)))))
        saved-cont))
(define (let-cont vars exps vals body env saved-cont)
  (cons (lambda (val cont)
          (let ([vals (cons val vals)])
            (if (null? exps)
              (value-of/k body (extend-env vars (reverse vals) env) cont)
              (value-of/k (car exps) env
                (let-cont vars (cdr exps) vals body env cont)))))
        saved-cont))
(define (if-test-cont exp2 exp3 env saved-cont)
  (cons (lambda (val1 cont)
          (if (expval->bool val1)
            (value-of/k exp2 env cont)
            (value-of/k exp3 env cont)))
        saved-cont))
(define (diff1-cont exp2 env saved-cont)
  (cons (lambda (val1 cont)
          (value-of/k exp2 env
            (diff2-cont val1 cont)))
        saved-cont))
(define (diff2-cont val1 saved-cont)
  (cons (lambda (val2 cont)
          (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
            (apply-cont cont (num-val (- num1 num2)))))
        saved-cont))
(define (rator-cont rands env saved-cont)
  (cons (lambda (rator cont)
          (if (null? rands)
            (apply-proc/k (expval->proc rator) '() cont)
            (value-of/k (car rands) env
              (rands-cont rator (cdr rands) '() env cont))))
        saved-cont))
(define (rands-cont rator rands vals env saved-cont)
  (cons (lambda (val cont)
          (let ([vals (cons val vals)])
            (if (null? rands)
              (apply-proc/k (expval->proc rator) (reverse vals) cont)
              (value-of/k (car rands) env
                (rands-cont rator (cdr rands) vals env cont)))))
        saved-cont))

; FinalAnswer = ExpVal
; apply-cont : Cont x ExpVal -> FinalAnswer
(define (apply-cont cont val)
  (if (null? cont)
    (begin (eopl:printf "End of computation ~%") val)
    ((car cont) val (cdr cont))))

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

; res = (num-val 15)
(define p2
  "letrec add(x r) = if zero?(x) then r else (add -(x, 1) -(r, -(0, x)))
   in (add 5 0)")

; res = (num-val 2)
(define p3
  "let x = 1
       y = 2
       z = 3
   in -(z, -(y, x))")

; res = (num-val 1)
(define p6
  "let x = 2
       y = 1
       f = proc(x y) -(x, y)
   in (f x y)")
