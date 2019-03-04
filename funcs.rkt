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

(define the-store 'uninit)
;empty-store : () -> Store
(define (empty-store) '())
;get-store : () -> Store
(define (get-store) the-store)
;init-store! : () -> Unspecified
(define (init-store!)
  (set! the-store (empty-store)))
;newref : ExpVal -> Ref
(define (newref val)
  (let ([next-ref (length the-store)])
    (begin (set! the-store (append the-store (list val))) next-ref)))
;deref : Ref -> ExpVal
(define (deref ref)
  (list-ref the-store ref))
;setref! : Ref x ExpVal -> Void
(define (setref! ref val)
  (set! the-store
    (let loop ([store the-store] [ref ref])
      (cond [(null? store) (report-invalid-reference ref the-store)]
            [(zero? ref) (cons val (cdr store))]
            [else (cons (car store) (loop (cdr store) (- ref 1)))]))))
