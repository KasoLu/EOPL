(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")
(require racket/trace)

(define max-size 0)
(define cont-size 0)
(define get-cont-size
  (lambda () cont-size))
(define inc-cont-size!
  (lambda () 
    (set! cont-size (+ cont-size 1))
    (if (> cont-size max-size)
      (set! max-size cont-size)
      #f)))
(define dec-cont-size!
  (lambda () 
    (set! cont-size (- cont-size 1))))

(define (run str)
  (value-of-program
    (scan&parse str)))

; value-of-program : Program -> FinalAnswer
(define (value-of-program pgm)
  (cases program pgm
    [a-program [expr]
      (value-of/k expr (init-env) 
        (begin (inc-cont-size!)
               (end-cont)))]))

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
        (begin (inc-cont-size!) 
               (zero?-cont cont)))]
    [let-exp [vars exps body]
      (if (null? vars)
        (value-of/k body env cont)
        (value-of/k (car exps) env
          (begin (inc-cont-size!) 
                 (let-cont vars (cdr exps) '() body env cont))))]
    [if-exp [exp1 exp2 exp3]
      (value-of/k exp1 env
        (begin (inc-cont-size!)
               (if-test-cont exp2 exp3 env cont)))]
    [diff-exp [exp1 exp2]
      (value-of/k exp1 env
        (begin (inc-cont-size!)
               (diff1-cont exp2 env cont)))]
    [call-exp [rator rands]
      (value-of/k rator env
        (begin (inc-cont-size!)
               (rator-cont rands env cont)))]
    [list-exp [exps]
      (if (null? exps)
        (apply-cont cont (list-val '()))
        (apply-cont (list-first-cont exps env cont) '()))]
    [mul-exp [exp1 exp2]
      (value-of/k exp1 env
        (begin (inc-cont-size!)
               (mul1-cont exp2 env cont)))]
    ))

; FinalAnswer = ExpVal
; apply-cont : Cont x ExpVal -> FinalAnswer
(define (apply-cont cont1 val)
  (dec-cont-size!)
  (cases cont cont1
    [end-cont []
      (begin (eopl:printf "End of computation.~%")
             (eopl:printf "Max of cont: ~a~%" max-size)
             val)]
    [zero?-cont [saved-cont]
      (apply-cont saved-cont
        (bool-val (zero? (expval->num val))))]
    [let-cont [vars exps vals body saved-env saved-cont]
      (let ([vals (cons val vals)])
        (if (null? exps)
          (value-of/k body (extend-env vars (reverse vals) saved-env) saved-cont)
          (value-of/k (car exps) saved-env
            (begin (inc-cont-size!)
                   (let-cont vars (cdr exps) vals body saved-env saved-cont)))))]
    [if-test-cont [exp2 exp3 saved-env saved-cont]
      (if (expval->bool val)
        (value-of/k exp2 saved-env saved-cont)
        (value-of/k exp3 saved-env saved-cont))]
    [diff1-cont [exp2 saved-env saved-cont]
      (value-of/k exp2 saved-env
        (begin (inc-cont-size!)
               (diff2-cont val saved-cont)))]
    [diff2-cont [val1 saved-cont]
      (let ([num1 (expval->num val1)] [num2 (expval->num val)])
        (apply-cont saved-cont (num-val (- num1 num2))))]
    [rator-cont [rands saved-env saved-cont]
      (if (null? rands)
        (apply-proc/k (expval->proc val) '() saved-cont)
        (value-of/k (car rands) saved-env
          (begin (inc-cont-size!)
                 (rands-cont val (cdr rands) '() saved-env saved-cont))))]
    [rands-cont [rator rands vals saved-env saved-cont]
      (let ([vals (cons val vals)])
        (if (null? rands)
          (apply-proc/k (expval->proc rator) (reverse vals) saved-cont)
          (value-of/k (car rands) saved-env
            (begin (inc-cont-size!)
                   (rands-cont rator (cdr rands) vals saved-env saved-cont)))))]
    [list-first-cont [exps saved-env saved-cont]
      (value-of/k (car exps) saved-env
        (begin (inc-cont-size!)
               (list-rests-cont val (cdr exps) saved-env saved-cont)))]
    [list-rests-cont [vals exps saved-env saved-cont]
      (let ([vals (append vals (list val))])
        (if (null? exps)
          (apply-cont saved-cont (list-val vals))
          (value-of/k (car exps) saved-env
            (begin (inc-cont-size!)
                   (list-rests-cont vals (cdr exps) saved-env saved-cont)))))]
    [mul1-cont [exp2 saved-env saved-cont]
      (value-of/k exp2 saved-env
        (begin (inc-cont-size!)
               (mul2-cont val saved-cont)))]
    [mul2-cont [val1 saved-cont]
      (let ([num1 (expval->num val1)] [num2 (expval->num val)])
        (apply-cont saved-cont (num-val (* num1 num2))))]
    [else
      (report-invalid-cont 'apply-cont cont1 val1)]
    ))

(define (apply-proc/k proc1 vals cont)
  (cases proc proc1
    [procedure [vars body saved-env]
      (value-of/k body (extend-env vars vals saved-env) cont)]))

;(trace apply-env)
;(trace apply-proc/k)
;(trace value-of/k)
;(trace apply-cont)

; res = (num-val 1)
(define p
  "letrec
     even(x) = if zero?(x) then 1 else (odd  -(x,1))
     odd(x)  = if zero?(x) then 0 else (even -(x,1))
   in (odd 13)")

; res = (num-val 6)
(define p2
  "let x = 2
       y = 3
   in *(x, y)")

; res = (num-val 24)
(define p3
  "letrec fact(x) = if zero?(x) then 1 else *(x, (fact -(x, 1)))
   in (fact 4)")

(define p4
  "letrec fact-iter(x r) = if zero?(x) then r else (fact-iter -(x, 1) *(x, r))
   in (fact-iter 4 1)")

