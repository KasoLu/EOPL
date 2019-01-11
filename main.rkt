(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")
(require racket/trace)

(define (apply-proc proc1 args)
  (cases proc proc1
    [procedure [vars body saved-env]
      (value-of body (extend-env vars (car args) saved-env) (cdr args))]))

(define (value-of-program pgm)
  (cases program pgm
    [a-program [expr]
      (value-of expr (init-env) '())]))

(define (value-of exp1 env store)
  (cases expression exp1
    [const-exp [num]
      (an-answer (num-val num) store)]
    [var-exp [var]
      (an-answer (apply-env env var) store)]
    [diff-exp [exp1 exp2]
      (cases answer (value-of exp1 env store)
        [an-answer [val1 new-store1]
          (cases answer (value-of exp2 env new-store1)
            [an-answer [val2 new-store2]
              (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
                (an-answer (num-val (- num1 num2)) new-store2))])])]
    [zero?-exp [exp1]
      (cases answer (value-of exp1 env store)
        [an-answer [val1 new-store]
          (let ([num (expval->num val1)])
            (if (zero? num)
              (an-answer (bool-val #t) new-store)
              (an-answer (bool-val #f) new-store)))])]
    [if-exp [exp1 exp2 exp3]
      (cases answer (value-of exp1 env store)
        [an-answer [val new-store]
          (if (expval->bool val)
            (value-of exp2 env new-store)
            (value-of exp3 env new-store))])]
    [let-exp [vars exps body]
      (define (loop exps sto vals)
        (if (null? exps)
          (cons vals sto)
          (cases answer (value-of (car exps) env sto)
            [an-answer [val new-store]
              (loop (cdr exps) new-store (append vals (list val)))])))
      (let ([res (loop exps store '())])
        (value-of body (extend-env vars (car res) env) (cdr res)))]
    [proc-exp [vars body]
      (an-answer (proc-val (procedure vars body env)) store)]
    [call-exp [rator rands]
      (define (loop exps sto vals)
        (if (null? exps)
          (cons vals sto)
          (cases answer (value-of (car exps) env sto)
            [an-answer [val new-store]
              (loop (cdr exps) new-store (append vals (list val)))])))
      (cases answer (value-of rator env store)
        [an-answer [val new-store]
          (let ([args (loop rands new-store '())])
            (apply-proc (expval->proc val) args))])]
    [letrec-exp [names varss bodies letrec-body]
      (value-of letrec-body (extend-env-rec names varss bodies env) store)]
    [newref-exp [exp1]
      (cases answer (value-of exp1 env store)
        [an-answer [val new-store]
          (let ([res (newref val new-store)])
            (an-answer (ref-val (car res)) (cdr res)))])]
    [deref-exp [exp1]
      (cases answer (value-of exp1 env store)
        [an-answer [v1 new-store]
          (let ([ref1 (expval->ref v1)])
            (an-answer (deref ref1 store) new-store))])]
    [setref-exp [exp1 exp2]
      (cases answer (value-of exp1 env store)
        [an-answer [val1 new-store1]
          (cases answer (value-of exp2 env new-store1)
            [an-answer [val2 new-store2]
              (let ([new-store3 (setref! (expval->ref val1) val2 new-store2)])
                (an-answer (num-val 23) new-store3))])])]
    [begin-exp [exp1 exps]
      (define (func expr ans)
        (cases answer ans
          [an-answer [val new-store]
            (value-of expr env new-store)]))
      (let ([val1 (value-of exp1 env store)])
        (foldl func val1 exps))]
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

