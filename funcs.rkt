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
        (cond [(null? names) (apply-env saved-env var)]
              [(eqv? (car names) var) (proc-val (procedure (car varss) (car bodies) env1))]
              [else (found (cdr names) (cdr varss) (cdr bodies))]))
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
(define (expval->ref val)
  (cases expval val
    [ref-val [ref] ref]
    [else (report-expval-extractor-error 'ref val)]))

(define the-store 'uninit)
(define curr-ref 0)
; empty-store : () -> Store
(define (empty-store) (make-vector 1024))
; get-store : () -> Store
(define (get-store) the-store)
; init-store! : () -> Unspecified
(define (init-store!)
  (set! the-store (empty-store))
  (set! curr-ref 0))
; reference? : SchemeVal -> Bool
(define (reference? v)
  (integer? v))
; newref : ExpVal -> Ref
(define (newref val)
  (let ([next-ref (+ curr-ref 1)])
    (vector-set! the-store next-ref val)
    (set! curr-ref next-ref)
    curr-ref))
; deref : Ref -> ExpVal
(define (deref ref)
  (vector-ref the-store ref))
; setref! : Ref x ExpVal -> Unspecified
(define (setref! ref val)
  (vector-set! the-store ref val))
