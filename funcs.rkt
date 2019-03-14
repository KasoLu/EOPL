(load "types.rkt")

(define (report-no-binding-found var)
  (eopl:error 'report-no-binding-found "No binding for ~s" var))
(define (report-expval-extractor-error type val)
  (eopl:error 'report-expval-extractor-error "invalid expval - ~a: ~a" type val))

(define (init-env)
  (empty-env))
(define (apply-env env1 var)
  (cases env env1
    [empty-env []
      (report-no-binding-found var)]
    [extend-env [saved-vars saved-vals saved-env]
      (let loop([vars saved-vars] [vals saved-vals])
        (cond [(null? vars) (apply-env saved-env var)]
              [(eqv? (car vars) var) (car vals)]
              [else (loop (cdr vars) (cdr vals))]))]
    [extend-env-rec [names varss bodies saved-env]
      (let loop([names names] [varss varss] [bodies bodies])
        (cond [(null? names) 
               (apply-env saved-env var)]
              [(eqv? (car names) var) 
               (proc-val (procedure (car varss) (car bodies) env1))]
              [else 
               (loop (cdr names) (cdr varss) (cdr bodies))]))]
    ))

(define init-tenv init-env)
(define apply-tenv apply-env)
(define extend-tenv extend-env)

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

(define (every? proc ls)
  (let loop ([ls ls] [res #t])
    (if (null? ls)
      res
      (loop (cdr ls) (and res (proc (car ls)))))))

(define (tvar-type? t)
  (cases type t
    [tvar-type [sn] #t]
    [else #f]))
(define (proc-type? t)
  (cases type t
    [proc-type [args-type ret-type] #t]
    [else #f]))
(define (proc-type->args-type pt)
  (cases type pt
    [proc-type [args-type ret-type] args-type]
    [else (report-invalid-type pt)]))
(define (proc-type->ret-type pt)
  (cases type pt
    [proc-type [args-type ret-type] ret-type]
    [else (report-invalid-type pt)]))

(define (ref-type? t)
  (cases type t
    [ref-type [to-type] #t]
    [else #f]))
(define (ref-type->to-type t)
  (cases type t
    [ref-type [to-type] to-type]
    [else (report-invalid-type t)]))
