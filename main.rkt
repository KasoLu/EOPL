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
      (value-of/k expr (init-env) (end-cont))]))

; value-of/k : Exp x Env x Cont -> ExpVal
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
    [multi-exp [exp1 exp2]
      (value-of/k exp1 env
        (multi1-cont exp2 env cont))]
    [call-exp [rator rands]
      (value-of/k rator env
        (rator-cont rands env cont))]
    [list-exp [exps]
      (if (null? exps)
        (apply-cont cont (list-val '()))
        (value-of/k (car exps) env
          (list-cont (cdr exps) '() env cont)))]
    [car-exp [exp1]
      (value-of/k exp1 env
        (car-cont cont))]
    [cdr-exp [exp1]
      (value-of/k exp1 env
        (cdr-cont cont))]
    [null?-exp [exp1]
      (value-of/k exp1 env
        (null?-cont cont))]
    [try-exp [exp1 var handler-exp]
      (value-of/k exp1 env
        (try-cont var handler-exp env cont))]
    [raise-exp [exp1]
      (value-of/k exp1 env
        (raise1-cont cont))]
    ))

(define (end-cont)
  (cons (lambda (val) (begin (eopl:printf "End of computation.~%") val))
        (lambda () (report-uncaught-exception))))
(define (zero?-cont saved-cont)
  (cons (lambda (val) (apply-cont saved-cont (bool-val (zero? (expval->num val)))))
        (lambda () ((cdr saved-cont)))))
(define (let-cont vars exps vals body saved-env saved-cont)
  (cons (lambda (val)
          (let ([vals (cons val vals)])
            (if (null? exps)
              (value-of/k body (extend-env vars (reverse vals) saved-env) saved-cont)
              (value-of/k (car exps) saved-env
                (let-cont vars (cdr exps) vals body saved-env saved-cont)))))
        (lambda () ((cdr saved-cont)))))
(define (if-test-cont exp2 exp3 saved-env saved-cont)
  (cons (lambda (val)
          (if (expval->bool val)
            (value-of/k exp2 saved-env saved-cont)
            (value-of/k exp3 saved-env saved-cont)))
        (lambda () ((cdr saved-cont)))))
(define (diff1-cont exp2 saved-env saved-cont)
  (cons (lambda (val)
          (value-of/k exp2 saved-env
            (diff2-cont val saved-cont)))
        (lambda () ((cdr saved-cont)))))
(define (diff2-cont val1 saved-cont)
  (cons (lambda (val)
          (apply-cont saved-cont (num-val (- (expval->num val1) (expval->num val)))))
        (lambda () ((cdr saved-cont)))))
(define (multi1-cont exp2 saved-env saved-cont)
  (cons (lambda (val)
          (value-of/k2 exp2 saved-env
            (multi2-cont val saved-cont)))
        (lambda () ((cdr saved-cont)))))
(define (multi2-cont val1 saved-cont)
  (cons (lambda (val)
          (apply-cont saved-cont (num-val (* (expval->num val1) (expval->num val)))))
        (lambda () ((cdr saved-cont)))))
(define (rator-cont rands saved-env saved-cont)
  (cons (lambda (val)
          (if (null? rands)
            (apply-proc/k (expval->proc val) '() saved-cont)
            (value-of/k (car rands) saved-env
              (rands-cont val (cdr rands) '() saved-env saved-cont))))
        (lambda () ((cdr saved-cont)))))
(define (rands-cont rator rands vals saved-env saved-cont)
  (cons (lambda (val)
          (let ([vals (cons val vals)])
            (if (null? rands)
              (apply-proc/k (expval->proc rator) (reverse vals) saved-cont)
              (value-of/k (car rands) saved-env
                (rands-cont rator (cdr rands) vals saved-env saved-cont)))))
        (lambda () ((cdr saved-cont)))))
(define (list-cont exps vals saved-env saved-cont)
  (cons (lambda (val)
          (let ([vals (cons val vals)])
            (if (null? exps)
              (apply-cont saved-cont (list-val (reverse vals)))
              (value-of/k (car exps) saved-env
                (list-cont (cdr exps) vals saved-env saved-cont)))))
        (lambda () ((cdr saved-cont)))))
(define (car-cont saved-cont)
  (cons (lambda (val) (apply-cont saved-cont (car (expval->list val))))
        (lambda () ((cdr saved-cont)))))
(define (cdr-cont saved-cont)
  (cons (lambda (val) (apply-cont saved-cont (list-val (cdr (expval->list val)))))
        (lambda () ((cdr saved-cont)))))
(define (null?-cont saved-cont)
  (cons (lambda (val) (apply-cont saved-cont (bool-val (null? (expval->list val)))))
        (lambda () ((cdr saved-cont)))))
(define (try-cont var handler-exp saved-env saved-cont)
  (cons (lambda (val) (apply-cont saved-cont val))
        (lambda ()
          (lambda (val)
            (let ([ext-env (extend-env (list var) (list val) saved-env)])
              (value-of/k handler-exp ext-env saved-cont))))))
(define (raise1-cont saved-cont)
  (cons (lambda (val) (apply-handler val saved-cont))
        (lambda () ((cdr saved-cont)))))

; apply-cont : Cont x ExpVal -> ExpVal
(define (apply-cont cont1 val)
  (if (pair? cont1)
    ((car cont1) val)
    (report-invalid-continuation cont1 val)))

; apply-proc/k : Proc x List(ExpVal) x Cont -> ExpVal
(define (apply-proc/k proc1 vals cont)
  (cases proc proc1
    [procedure [vars body saved-env]
      (value-of/k body (extend-env vars vals saved-env) cont)]))

; apply-handler : ExpVal x Cont -> ExpVal
(define (apply-handler val cont1)
  (if (pair? cont1)
    (((cdr cont1)) val)
    (report-invalid-continuation cont1 val)))

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
                     then raise 99
                     else if zero?(-(car(lst), n))
                          then 0
                          else -((inner cdr(lst)), -1)
                 in proc(lst)
                      try (inner lst)
                      catch (x) -1
   in ((index 5) list(2, 3))")

; res = (num-val 100)
(define p4
  "try 
     let a = raise 100
         b = raise 150 in
     -(b, a)
   catch(e)
     e")
