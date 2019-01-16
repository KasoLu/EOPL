(load "types.rkt")

(define (report-no-binding-found var)
  (eopl:error 'report-no-binding-found "No binding for ~s" var))
(define (report-invalid-concrete-syntax datum)
  (eopl:error 'report-invalid-concrete-syntax "invalid concrete syntax: ~a" datum))
(define (report-expval-extractor-error type val)
  (eopl:error 'report-expval-extractor-error "invalid expval - ~a: ~a" type val))

(define (init-env)
  (empty-env))
(define (apply-env env1 var)
  (cases env env1
    [empty-env []
      (report-no-binding-found var)]
    [extend-env [saved-vars saved-vals saved-env]
      (define (found vars vals)
        (cond [(null? vars) (apply-env saved-env var)]
              [(eqv? (car vars) var) (car vals)]
              [else (found (cdr vars) (cdr vals))]))
      (found saved-vars saved-vals)]
    [extend-env-rec [names varss bodies saved-env]
      (define (found names varss bodies)
        (cond [(null? names) 
               (apply-env saved-env var)]
              [(eqv? (car names) var) 
               (proc-val (procedure (car varss) (car bodies) env1))]
              [else 
               (found (cdr names) (cdr varss) (cdr bodies))]))
      (found names varss bodies)]
    ))

(define (expval->num val)
  (cases expval val
    [num-val [num] num]
    [else (report-expval-extractor-error 'num val)]))
(define (expval->bool val)
  (cases expval val
    [bool-val [bool] bool]
    [else (report-expval-extractor-error 'bool val)]))
(define (expval->proc val)
  (cases expval val
    [proc-val [proc] proc]
    [else (report-expval-extractor-error 'proc val)]))

(define (end-cont)
  (lambda (val)
    (begin (eopl:printf "End of computation.~%")
           val)))
(define (diff1-cont exp2 env cont)
  (lambda (val1)
    (value-of/k exp2 env
      (diff2-cont val1 cont))))
(define (diff2-cont val1 cont)
  (lambda (val2)
    (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
      (apply-cont cont (num-val (- num1 num2))))))
(define (rator-cont rands env cont)
  (lambda (val1)
    (value-of/k (car rands) env
      (rands-cont val1 cont))))
(define (rands-cont val1 cont)
  (lambda (val2)
    (let ([proc1 (expval->proc val1)])
      (apply-proc/k proc1 (list val2) cont))))
(define (zero?-cont cont)
  (lambda (val1)
    (apply-cont cont (bool-val (zero? (expval->num val1))))))
(define (if-test-cont exp2 exp3 env cont)
  (lambda (val1)
    (if (expval->bool val1)
      (value-of/k exp2 env cont)
      (value-of/k exp3 env cont))))
(define (let-cont vars body env cont)
  (lambda (val)
    (value-of/k body (extend-env vars (list val) env) cont)))
