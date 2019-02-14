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
      (value-of/k expr (init-env)
        (lambda (type val)
          (if (eqv? type 'cont)
            (begin (eopl:printf "End of computation.~%") val)
            (report-uncaught-exception val))))]))

; value-of/k : Exp x Env x Cont -> ExpVal
(define (value-of/k expr env cont)
  (cases expression expr
    [const-exp [num]
      (cont 'cont (num-val num))]
    [var-exp [var]
      (cont 'cont (apply-env env var))]
    [proc-exp [vars body]
      (cont 'cont (proc-val (procedure vars body env)))]
    [letrec-exp [names varss bodies letrec-body]
      (value-of/k letrec-body (extend-env-rec names varss bodies env) cont)]
    [zero?-exp [exp1]
      (value-of/k exp1 env
        (lambda (type val)
          (if (eqv? type 'cont)
            (cont type (bool-val (zero? (expval->num val))))
            (cont type val))))]
    [let-exp [vars exps body]
      (if (null? vars)
        (value-of/k body env cont)
        (let loop([exps exps] [vals '()])
          (if (null? exps)
            (value-of/k body (extend-env vars (reverse vals) env) cont)
            (value-of/k (car exps) env
              (lambda (type val)
                (if (eqv? type 'cont)
                  (loop (cdr exps) (cons val vals))
                  (cont type val)))))))]
    [if-exp [exp1 exp2 exp3]
      (value-of/k exp1 env
        (lambda (type val)
          (if (eqv? type 'cont)
            (if (expval->bool val)
              (value-of/k exp2 env cont)
              (value-of/k exp3 env cont))
            (cont type val))))]
    [diff-exp [exp1 exp2]
      (value-of/k exp1 env
        (lambda (type1 val1)
          (if (eqv? type1 'cont)
            (value-of/k exp2 env
              (lambda (type2 val2)
                (if (eqv? type2 'cont)
                  (cont type2 (num-val (- (expval->num val1) (expval->num val2))))
                  (cont type2 val2))))
            (cont type1 val1))))]
    [multi-exp [exp1 exp2]
      (value-of/k exp1 env
        (lambda (type1 val1)
          (if (eqv? type1 'cont)
            (value-of/k exp2 env
              (lambda (type2 val2)
                (if (eqv? type2 'cont)
                  (cont type2 (num-val (* (expval->num val1) (expval->num val2))))
                  (cont type2 val2))))
            (cont type1 val1))))]
    [call-exp [rator rands]
      (value-of/k rator env
        (lambda (type val)
          (if (eqv? type 'cont)
            (let loop([rands rands] [vals '()])
              (if (null? rands)
                (apply-proc/k (expval->proc val) (reverse vals) cont)
                (value-of/k (car rands) env
                  (lambda (rand-type rand-val)
                    (if (eqv? rand-type 'cont)
                      (loop (cdr rands) (cons rand-val vals))
                      (cont rand-type rand-val))))))
            (cont type val))))]
    [list-exp [exps]
      (let loop([exps exps] [vals '()])
        (if (null? exps)
          (cont 'cont (list-val (reverse vals)))
          (value-of/k (car exps) env
            (lambda (type val)
              (if (eqv? type 'cont)
                (loop (cdr exps) (cons val vals))
                (cont type val))))))]
    [car-exp [exp1]
      (value-of/k exp1 env
        (lambda (type val)
          (if (eqv? type 'cont)
            (cont type (car (expval->list val)))
            (cont type val))))]
    [cdr-exp [exp1]
      (value-of/k exp1 env
        (lambda (type val)
          (if (eqv? type 'cont)
            (cont type (list-val (cdr (expval->list val))))
            (cont type val))))]
    [null?-exp [exp1]
      (value-of/k exp1 env
        (lambda (type val)
          (if (eqv? type 'cont)
            (cont type (bool-val (null? (expval->list val))))
            (cont type val))))]
    [try-exp [exp1 var handler-exp]
      (value-of/k exp1 env
        (lambda (type val)
          (if (eqv? type 'cont) 
            (cont type val)
            (value-of/k handler-exp (extend-env (list var) (list val) env) cont))))]
    [raise-exp [exp1]
      (value-of/k exp1 env
        (lambda (type val)
          (cont 'exception val)))]
    ))

(define (apply-proc/k proc1 vals cont)
  (cases proc proc1
    [procedure [vars body saved-env]
      (value-of/k body (extend-env vars vals saved-env) cont)]))

;(trace apply-env)
;(trace apply-proc/k)
;(trace value-of/k)
;(trace apply-cont)

