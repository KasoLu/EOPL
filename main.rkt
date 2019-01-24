(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")
(require racket/trace)

(define (run str)
  (value-of-program
    (scan&parse str)))

; value-of-program : Program -> ExpVal
(define (value-of-program pgm)
  (cases program pgm
    [a-program [expr]
      (value-of/k expr (init-env) (end-cont) '())]))

; value-of/k : Exp x Env x Cont x List(Cont) -> ExpVal
(define (value-of/k expr env cont excp-conts)
  (cases expression expr
    [const-exp [num]
      (apply-cont cont (num-val num) excp-conts)]
    [var-exp [var]
      (apply-cont cont (apply-env env var) excp-conts)]
    [proc-exp [vars body]
      (apply-cont cont (proc-val (procedure vars body env)) excp-conts)]
    [letrec-exp [names varss bodies letrec-body]
      (value-of/k letrec-body (extend-env-rec names varss bodies env) cont excp-conts)]
    [zero?-exp [exp1]
      (value-of/k exp1 env
        (zero?-cont cont) excp-conts)]
    [let-exp [vars exps body]
      (if (null? vars)
        (value-of/k body env cont excp-conts)
        (value-of/k (car exps) env
          (let-cont vars (cdr exps) '() body env cont) excp-conts))]
    [if-exp [exp1 exp2 exp3]
      (value-of/k exp1 env
        (if-test-cont exp2 exp3 env cont) excp-conts)]
    [diff-exp [exp1 exp2]
      (value-of/k exp1 env
        (diff1-cont exp2 env cont) excp-conts)]
    [multi-exp [exp1 exp2]
      (value-of/k exp1 env
        (multi1-cont exp2 env cont) excp-conts)]
    [call-exp [rator rands]
      (value-of/k rator env
        (rator-cont rands env cont) excp-conts)]
    [list-exp [exps]
      (if (null? exps)
        (apply-cont cont (list-val '()) excp-conts)
        (value-of/k (car exps) env
          (list-cont (cdr exps) '() env cont) excp-conts))]
    [car-exp [exp1]
      (value-of/k exp1 env
        (car-cont cont) excp-conts)]
    [cdr-exp [exp1]
      (value-of/k exp1 env
        (cdr-cont cont) excp-conts)]
    [null?-exp [exp1]
      (value-of/k exp1 env
        (null?-cont cont) excp-conts)]
    [try-exp [exp1 var handler-exp]
      (let ([curr-cont (try-cont var handler-exp env cont)])
        (value-of/k exp1 env curr-cont (cons curr-cont excp-conts)))]
    [raise-exp [exp1 key1]
      (value-of/k exp1 env
        (raise1-cont key1 cont) excp-conts)]
    ))

; apply-cont : Cont x ExpVal x List(Cont) -> ExpVal
(define (apply-cont cont1 val excp-conts)
  (cases cont cont1
    [end-cont []
      (begin (eopl:printf "End of computation.~%") val)]
    [zero?-cont [saved-cont]
      (apply-cont saved-cont
        (bool-val (zero? (expval->num val))) excp-conts)]
    [let-cont [vars exps vals body saved-env saved-cont]
      (let ([vals (cons val vals)])
        (if (null? exps)
          (value-of/k body (extend-env vars (reverse vals) saved-env) saved-cont excp-conts)
          (value-of/k (car exps) saved-env
            (let-cont vars (cdr exps) vals body saved-env saved-cont) excp-conts)))]
    [if-test-cont [exp2 exp3 saved-env saved-cont]
      (if (expval->bool val)
        (value-of/k exp2 saved-env saved-cont excp-conts)
        (value-of/k exp3 saved-env saved-cont excp-conts))]
    [diff1-cont [exp2 saved-env saved-cont]
      (value-of/k exp2 saved-env
        (diff2-cont val saved-cont) excp-conts)]
    [diff2-cont [val1 saved-cont]
      (let ([num1 (expval->num val1)] [num2 (expval->num val)])
        (apply-cont saved-cont (num-val (- num1 num2)) excp-conts))]
    [multi1-cont [exp2 saved-env saved-cont]
      (value-of/k exp2 saved-env
        (multi2-cont val saved-cont) excp-conts)]
    [multi2-cont [val1 saved-cont]
      (let ([num1 (expval->num val1)] [num2 (expval->num val)])
        (apply-cont saved-cont (num-val (* num1 num2)) excp-conts))]
    [rator-cont [rands saved-env saved-cont]
      (if (null? rands)
        (apply-proc/k (expval->proc val) '() saved-cont excp-conts)
        (value-of/k (car rands) saved-env
          (rands-cont val (cdr rands) '() saved-env saved-cont) excp-conts))]
    [rands-cont [rator rands vals saved-env saved-cont]
      (let ([vals (cons val vals)])
        (if (null? rands)
          (apply-proc/k (expval->proc rator) (reverse vals) saved-cont excp-conts)
          (value-of/k (car rands) saved-env
            (rands-cont rator (cdr rands) vals saved-env saved-cont) excp-conts)))]
    [list-cont [exps vals saved-env saved-cont]
      (let ([vals (cons val vals)])
        (if (null? exps)
          (apply-cont saved-cont (list-val (reverse vals)) excp-conts)
          (value-of/k (car exps) saved-env
            (list-cont (cdr exps) vals saved-env saved-cont) excp-conts)))]
    [car-cont [saved-cont]
      (apply-cont saved-cont (car (expval->list val)) excp-conts)]
    [cdr-cont [saved-cont]
      (apply-cont saved-cont (list-val (cdr (expval->list val))) excp-conts)]
    [null?-cont [saved-cont]
      (apply-cont saved-cont (bool-val (null? (expval->list val))) excp-conts)]
    [try-cont [var handler-exp env saved-cont]
      (apply-cont saved-cont val (cdr excp-conts))]
    [raise1-cont [key1 saved-cont]
      (apply-handler key1 val saved-cont excp-conts)]
    [continue-cont [try-cont saved-cont]
      (apply-cont saved-cont val (cons try-cont excp-conts))]
    [else
      (report-invalid-cont 'apply-cont cont1 val1)]
    ))

; apply-proc/k : Proc x List(ExpVal) x Cont x List(Cont) -> ExpVal
(define (apply-proc/k proc1 vals cont excp-conts)
  (cases proc proc1
    [procedure [vars body saved-env]
      (value-of/k body (extend-env vars vals saved-env) cont excp-conts)]))

; apply-handler : Keyword x ExpVal x Cont x List(Cont) -> ExpVal
(define (apply-handler key1 val next-cont excp-conts)
  (if (null? excp-conts)
    (report-uncaught-exception)
    (cases cont (car excp-conts)
      [try-cont [var handler-exp saved-env jump-cont]
        (let ([ext-env (extend-env (list var) (list val) saved-env)])
          (cases keyword key1
            [continue-keyword []
              (value-of/k handler-exp ext-env
                (continue-cont (car excp-conts) next-cont) (cdr excp-conts))]
            [break-keyword []
              (value-of/k handler-exp ext-env jump-cont (cdr excp-conts))]))]
      [else
        (report-invalid-exception-cont excp-conts)])))

;(trace apply-env)
;(trace apply-proc/k)
;(trace value-of/k)
;(trace apply-cont)
;(trace apply-handler)

; res = (num-val 1)
(define p1
  "letrec
     even(x) = if zero?(x) then 1 else (odd  -(x,1))
     odd(x)  = if zero?(x) then 0 else (even -(x,1))
   in (odd 13)")

; res = (list-val (1 2))
(define p2
  "let x = list(1, 2, 3)
   in let a = car(x) b = cdr(x)
      in let c = car(b)
         in list(a, c)")

; res = (num-val -1)
(define p3
  "let index = proc(n)
                 letrec inner(lst)
                   = if null?(lst)
                     then raise 99 break
                     else if zero?(-(car(lst), n))
                          then 0
                          else -((inner cdr(lst)), -1)
                 in proc(lst)
                      try (inner lst)
                      catch (x) -1
   in ((index 5) list(2, 3))")

; res = (num-val 50)
(define p4
  "try
     try
       let x = raise 100 continue
           y = raise 150 continue in
       -(y, x)
     catch(e2)
       raise -(e2, -10) continue
   catch(e1)
     e1")
