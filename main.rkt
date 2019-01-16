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
      (value-of/k expr (init-env)
        (lambda (val)
          (begin (eopl:printf "End of computation.~%")
                 val)))]))

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
        (lambda (val1)
          (apply-cont cont (bool-val (zero? (expval->num val1))))))]
    [if-exp [exp1 exp2 exp3]
      (value-of/k exp1 env
        (lambda (val1)
          (if (expval->bool val1)
            (value-of/k exp2 env cont)
            (value-of/k exp3 env cont))))]
    [let-exp [vars exps body]
      (if (null? vars)
        (value-of/k body env cont)
        (let loop([exps exps] [vals '()])
          (if (null? exps)
            (value-of/k body (extend-env vars (reverse vals) env) cont)
            (value-of/k (car exps) env
              (lambda (val)
                (loop (cdr exps) (cons (newref val) vals)))))))]
    [diff-exp [exp1 exp2]
      (value-of/k exp1 env
        (lambda (val1)
          (value-of/k exp2 env
            (lambda (val2)
              (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
                (apply-cont cont (num-val (- num1 num2))))))))]
    [call-exp [rator rands]
      (value-of/k rator env
        (lambda (rator-val)
          (let loop([rands rands] [vals '()])
            (if (null? rands)
              (apply-proc/k (expval->proc rator-val) (reverse vals) cont)
              (value-of/k (car rands) env
                (lambda (val)
                  (loop (cdr rands) (cons (newref val) vals))))))))]
    [assign-exp [var exp1]
      (value-of/k exp1 env
        (lambda (val1)
          (begin (setref! (apply-env env var) val1)
                 (apply-cont cont (num-val 27)))))]
    [begin-exp [exp1 exps]
      (value-of/k exp1 env
        (lambda (val1)
          (let loop([exps exps] [val val1])
            (if (null? exps)
              (apply-cont cont val)
              (value-of/k (car exps) env
                (lambda (val)
                  (loop (cdr exps) val)))))))]
    [else
      (report-invalid-expression expr)]
    ))

(define (apply-proc/k proc1 refs cont)
  (cases proc proc1
    [procedure [vars body saved-env]
      (value-of/k body (extend-env vars refs saved-env) cont)]))

(define (apply-cont cont val)
  (cont val))

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

