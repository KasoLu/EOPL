(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")
(require racket/trace)

(define (apply-proc proc1 args)
  (cases proc proc1
    [procedure [vars body saved-env]
      (value-of body (extend-env vars args saved-env))]))

(define (value-of-program pgm)
  (init-store!)
  (cases program pgm
    [a-program [expr]
      (value-of expr (init-env))]))

(define (value-of expr env)
  (cases expression expr
    [const-exp [num] 
      (num-val num)]
    [var-exp [var] 
      (apply-env env var)]
    [diff-exp [exp1 exp2]
      (let ([val1 (value-of exp1 env)] [val2 (value-of exp2 env)])
        (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
          (num-val (- num1 num2))))]
    [zero?-exp [exp1]
      (let ([val1 (value-of exp1 env)])
        (let ([num1 (expval->num val1)])
          (if (zero? num1)
            (bool-val #t)
            (bool-val #f))))]
    [if-exp [exp1 exp2 exp3]
      (let ([val1 (value-of exp1 env)])
        (if (expval->bool val1)
          (value-of exp2 env)
          (value-of exp3 env)))]
    [let-exp [vars exps body]
      (let ([vals (map (lambda (e) (value-of e env)) exps)])
        (value-of body (extend-env vars vals env)))]
    [proc-exp [vars body]
      (proc-val (procedure vars body env))]
    [call-exp [rator rands]
      (let ([proc (expval->proc (value-of rator env))]
            [args (map (lambda (x) (value-of x env)) rands)])
        (apply-proc proc args))]
    [letrec-exp [names varss bodies letrec-body]
      (value-of letrec-body (extend-env-rec names varss bodies env))]
    [newref-exp [exp1]
      (let ([v1 (value-of exp1 env)])
        (ref-val (newref v1)))]
    [deref-exp [exp1]
      (let ([v1 (value-of exp1 env)])
        (let ([ref1 (expval->ref v1)])
          (deref ref1)))]
    [setref-exp [exp1 exp2]
      (let ([ref (expval->ref (value-of exp1 env))])
        (let ([val2 (value-of exp2 env)])
          (begin
            (setref! ref val2)
            (num-val 23))))]
    [begin-exp [exp1 exps]
      (let ([val1 (value-of exp1 env)])
        (foldl (lambda (e v) (value-of e env)) val1 exps))]
    [else
      (report-invalid-expression expr)]
    ))

(define (run str)
  (value-of-program
    (scan&parse str)))

;(trace value-of-program)
;(trace value-of)
;(trace apply-env)
;(trace apply-proc)

; res = (num-val 1)
(define p
  "let x = newref(0)
   in letrec
      even()
        = if zero?(deref(x)) then 1 else begin setref(x, -(deref(x),1)); (odd) end
      odd()
        = if zero?(deref(x)) then 0 else begin setref(x, -(deref(x),1)); (even) end
      in begin setref(x,13); (odd) end")

; res = (num-val -1)
(define p2
  "let g = let counter = newref(0)
           in proc()
              begin setref(counter, -(deref(counter), -1));
                    deref(counter)
              end
   in let a = (g)
      in let b = (g)
         in -(a,b)")

; res = (num-val 11)
(define p3
  "let x = newref(newref(0))
   in begin
        setref(deref(x), 11);
        deref(deref(x))
      end")

; res = (num-val 0)
(define p4
  "let g = proc()
           let counter = newref(0)
           in begin
                setref(counter, -(deref(counter), -1));
                deref(counter)
              end
   in let a = (g)
      in let b = (g)
         in -(a,b)")

