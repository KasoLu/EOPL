(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")
(require racket/trace)

(define r-expr 'uninit)
(define r-env  'uninit)
(define r-cont 'uninit)
(define r-val  'uninit)
(define r-proc 'uninit)
(define r-vals 'uninit)
(define r-com1 'uninit)
(define r-com2 'uninit)

; FinalAnswer = ExpVal
(define (run str)
  (value-of-program
    (scan&parse str)))

; value-of-program : Program -> FinalAnswer
(define (value-of-program pgm)
  (cases program pgm
    [a-program [exp1]
      (set! r-cont (end-cont))
      (set! r-env (init-env))
      (set! r-expr exp1)
      (value-of/k)]))

; value-of/k : Exp x Env x Cont -> FinalAnswer
(define (value-of/k)
  ;(eopl:printf "expr: ~a\nenv: ~a\ncont: ~a\n" r-expr r-env r-cont)
  (cases expression r-expr
    [const-exp [num]
      (set! r-val (num-val num))
      (apply-cont)]
    [var-exp [var]
      (set! r-val (apply-env r-env var))
      (apply-cont)]
    [proc-exp [vars body]
      (set! r-val (proc-val (procedure vars body r-env)))
      (apply-cont)]
    [letrec-exp [names varss bodies letrec-body]
      (set! r-env (extend-env-rec names varss bodies r-env))
      (set! r-expr letrec-body)
      (value-of/k)]
    [zero?-exp [exp1]
      (set! r-cont (zero?-cont r-cont))
      (set! r-expr exp1)
      (value-of/k)]
    [let-exp [vars exps body]
      (cond [(null? vars)
             (set! r-expr body)
             (value-of/k)]
            [else
             (set! r-vals '())
             (set! r-cont (let-cont vars (cdr exps) body r-env r-cont))
             (set! r-expr (car exps))
             (value-of/k)])]
    [if-exp [exp1 exp2 exp3]
      (set! r-cont (if-test-cont exp2 exp3 r-env r-cont))
      (set! r-expr exp1)
      (value-of/k)]
    [diff-exp [exp1 exp2]
      (set! r-cont (diff1-cont exp2 r-env r-cont))
      (set! r-expr exp1)
      (value-of/k)]
    [multi-exp [exp1 exp2]
      (set! r-cont (multi1-cont exp2 r-env r-cont))
      (set! r-expr exp1)
      (value-of/k)]
    [call-exp [rator rands]
      (set! r-cont (rator-cont rands r-env r-cont))
      (set! r-expr rator)
      (value-of/k)]
    ))

; apply-cont : Cont x ExpVal -> FinalAnswer
(define (apply-cont)
  ;(eopl:printf "cont: ~a\nval: ~a\n" r-cont r-val)
  (cases cont r-cont
    [end-cont []
      (begin (eopl:printf "End of computation.~%") r-val)]
    [zero?-cont [saved-cont]
      (set! r-val (bool-val (zero? (expval->num r-val))))
      (set! r-cont saved-cont)
      (apply-cont)]
    [let-cont [vars exps body saved-env saved-cont]
      (set! r-vals (cons r-val r-vals))
      (cond [(null? exps)
             (set! r-cont saved-cont)
             (set! r-env (extend-env vars (reverse r-vals) saved-env))
             (set! r-expr body)
             (value-of/k)]
            [else
             (set! r-cont (let-cont vars (cdr exps) body saved-env saved-cont))
             (set! r-env saved-env)
             (set! r-expr (car exps))
             (value-of/k)])]
    [if-test-cont [exp2 exp3 saved-env saved-cont]
      (set! r-cont saved-cont)
      (set! r-env saved-env)
      (if (expval->bool r-val)
        (set! r-expr exp2)
        (set! r-expr exp3)) 
      (value-of/k)]
    [diff1-cont [exp2 saved-env saved-cont]
      (set! r-cont (diff2-cont r-val saved-cont))
      (set! r-env saved-env)
      (set! r-expr exp2)
      (value-of/k)]
    [diff2-cont [val1 saved-cont]
      (set! r-com1 (expval->num val1))
      (set! r-com2 (expval->num r-val))
      (set! r-val (num-val (- r-com1 r-com2)))
      (set! r-cont saved-cont)
      (apply-cont)]
    [multi1-cont [exp2 saved-env saved-cont]
      (set! r-cont (multi2-cont r-val saved-cont))
      (set! r-env saved-env)
      (set! r-expr exp2)
      (value-of/k)]
    [multi2-cont [val1 saved-cont]
      (set! r-com1 (expval->num val1))
      (set! r-com2 (expval->num r-val))
      (set! r-val (num-val (* r-com1 r-com2)))
      (set! r-cont saved-cont)
      (apply-cont)]
    [rator-cont [rands saved-env saved-cont]
      (set! r-vals '())
      (cond [(null? rands)
             (set! r-cont saved-cont)
             (set! r-proc (expval->proc r-val))
             (apply-proc/k)]
            [else
             (set! r-cont (rands-cont r-val (cdr rands) saved-env saved-cont))
             (set! r-env saved-env)
             (set! r-expr (car rands))
             (value-of/k)])]
    [rands-cont [rator rands saved-env saved-cont]
      (set! r-vals (cons r-val r-vals))
      (cond [(null? rands)
             (set! r-cont saved-cont)
             (set! r-vals (reverse r-vals))
             (set! r-proc (expval->proc rator))
             (apply-proc/k)]
            [else
             (set! r-cont (rands-cont rator (cdr rands) saved-env saved-cont))
             (set! r-env saved-env)
             (set! r-expr (car rands))
             (value-of/k)])]
    [else
      (report-invalid-cont 'apply-cont cont1 val1)]
    ))

; apply-proc/k : Proc x ExpVal x Cont -> FinalAnswer
(define (apply-proc/k)
  ;(eopl:printf "proc: ~a\nvals: ~a\ncont: ~a\n" r-proc r-vals r-cont)
  (cases proc r-proc
    [procedure [vars body saved-env]
      (if (pair? vars) (set! r-env (extend-env vars r-vals saved-env)) #f)
      (set! r-expr body)
      (value-of/k)]))

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

; res = (num-val 24)
(define p7
  "letrec fact-iter(n) = (fact-iter-acc n 1)
          fact-iter-acc(n a) = if zero?(n) then a else (fact-iter-acc -(n, 1) *(n, a))
   in (fact-iter 4)")
