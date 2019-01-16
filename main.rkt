(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")
(require racket/trace)

(define (run str)
  (value-of-program
    (scan&parse str)))

(define (value-of-program pgm)
  (init-store!)
  (cases program pgm
    [a-program [expr]
      (value-of/k expr (init-env) (end-cont))]))

(define (value-of/k expr env cont)
  (cases expression expr
    [const-exp [num] 
      (apply-cont cont (num-val num))]
    [var-exp [var]
      (apply-cont cont (deref (apply-env env var)))]
    [proc-exp [vars body]
      (apply-cont cont (proc-val (procedure vars body env)))]
    [letrec-exp [names varss bodies letrec-body]
      (value-of/k letrec-body (extend-env-rec names varss bodies env) cont)]
    [zero?-exp [exp1]
      (value-of/k exp1 env
        (zero?-cont cont))]
    [if-exp [exp1 exp2 exp3]
      (value-of/k exp1 env
        (if-test-cont exp2 exp3 env cont))]
    [let-exp [vars exps body]
      (if (null? vars)
        (value-of/k body env cont)
        (value-of/k (car exps) env
          (let-cont vars (cdr exps) '() body env cont)))]
    [diff-exp [exp1 exp2]
      (value-of/k exp1 env
        (diff1-cont exp2 env cont))]
    [call-exp [rator rands]
      (value-of/k rator env
        (rator-cont rands env cont))]
    [assign-exp [var exp1]
      (value-of/k exp1 env
        (assign-cont (apply-env env var) cont))]
    [begin-exp [exp1 exps]
      (value-of/k exp1 env
        (begin-cont exps env cont))]
    [else
      (report-invalid-expression expr)]
    ))

(define (apply-cont cont1 val)
  (cases cont cont1
    [end-cont []
      (begin (eopl:printf "End of computation.~%")
             val)]
    [diff1-cont [exp2 env cont]
      (value-of/k exp2 env
        (diff2-cont val env cont))]
    [diff2-cont [val1 env cont]
      (let ([num1 (expval->num val1)] [num2 (expval->num val)])
        (apply-cont cont (num-val (- num1 num2))))]
    [rator-cont [rands env cont]
      (if (null? rands)
        (apply-proc/k (expval->proc val) '() cont)
        (value-of/k (car rands) env
          (rands-cont val (cdr rands) '() env cont)))]
    [rands-cont [rator rands vals env cont]
      (let ([vals (cons (newref val) vals)])
        (if (null? rands)
          (apply-proc/k (expval->proc rator) (reverse vals) cont)
          (value-of/k rator (car rands) vals env cont)))]
    [zero?-cont [cont]
      (apply-cont cont (bool-val (zero? (expval->num val))))]
    [if-test-cont [exp2 exp3 env cont]
      (if (expval->bool val)
        (value-of/k exp2 env cont)
        (value-of/k exp3 env cont))]
    [let-cont [vars exps vals body env cont]
      (let ([vals (cons (newref val) vals)])
        (if (null? exps)
          (value-of/k body (extend-env vars (reverse vals) env) cont)
          (value-of/k (car exps) env
            (let-cont vars (cdr exps) vals body env cont))))]
    [assign-cont [ref cont]
      (begin (setref! ref val)
             (apply-cont cont (num-val 27)))]
    [begin-cont [exps env cont]
      (if (null? exps)
        (apply-cont cont val)
        (value-of/k (car exps) env
          (begin-cont (cdr exps) env cont)))]
    ))

(define (apply-proc/k proc1 refs cont)
  (cases proc proc1
    [procedure [vars body saved-env]
      (value-of/k body (extend-env vars refs saved-env) cont)]))

;(trace value-of-program)
;(trace value-of/k)
;(trace apply-env)
;(trace apply-proc/k)
;(trace apply-cont)

; res = (num-val 1)
(define p
  "let x = 0
   in letrec
      even()
        = if zero?(x) then 1 else begin set x = -(x,1); (odd) end
      odd()
        = if zero?(x) then 0 else begin set x = -(x,1); (even) end
      in begin set x = 13; (odd) end")

; res = (num-val 12)
(define p5
  "let f = proc(x)
             proc(y)
               begin
                 set x = -(x, -1);
                 -(x, y)
               end
   in ((f 44) 33)")

; res = (num-val 12)
(define p6
  "let times4 = 0
   in begin
       set times4 = proc(x) if zero?(x) then 0 else -((times4 -(x,1)), -4);
       (times4 3)
      end")

