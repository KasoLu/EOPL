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
      (value-of expr (init-env))]))

(define (value-of expr env)
  (cases expression expr
    [const-exp [num] 
      (num-val num)]
    [var-exp [var]
      (deref (apply-env env var))]
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
        (let ([refs (map (lambda (v) (newref v)) vals)])
          (value-of body (extend-env vars refs env))))]
    [proc-exp [vars body]
      (proc-val (procedure vars body env))]
    [call-exp [rator rands]
      (let ([proc1 (expval->proc (value-of rator env))]
            [args (map (lambda (x) (value-of x env)) rands)])
        (cases proc proc1
          [procedure [vars body saved-env]
            (define (map-vals-to-refs vals)
              (if (null? vals)
                (list)
                (cases expval (car vals)
                  [ref-val [ref]
                    (cons ref (map-vals-to-refs (cdr vals)))]
                  [else
                    (cons (newref (car vals)) (map-vals-to-refs (cdr vals)))])))
            (let ([refs (map-vals-to-refs args)])
              (value-of body (extend-env vars refs saved-env)))]))]
    [letrec-exp [names varss bodies letrec-body]
      (value-of letrec-body (extend-env-rec names varss bodies env))]
    [assign-exp [var exp1]
      (begin (setref! (apply-env env var) (value-of exp1 env))
             (num-val 27))]
    [begin-exp [exp1 exps]
      (let ([val1 (value-of exp1 env)])
        (foldl (lambda (e v) (value-of e env)) val1 exps))]
    [ref-exp [var1]
      (ref-val (apply-env env var1))]
    [deref-exp [var1]
      (deref (apply-env env var1))]
    [setref-exp [var1 exp1]
      (setref! (apply-env env var1) (value-of exp1 env))]
    [else
      (report-invalid-expression expr)]
    ))

;(trace value-of-program)
;(trace value-of)
;(trace apply-env)
;(trace apply-proc)

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
       set times4 = proc(x)
                     if zero?(x)
                     then 0
                     else -((times4 -(x,1)), -4);
       (times4 3)
      end")

; res = (num-val 1)
(define p7
  "let a = 3
   in let b = 4
      in let swap = proc(x)
                      proc(y)
                        let temp = deref(x)
                        in begin
                             setref(x, deref(y));
                             setref(y, temp)
                           end
         in begin ((swap ref a) ref b); -(a, b) end")
