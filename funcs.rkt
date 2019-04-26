(load "types.rkt")

(define (report-no-binding-found var)
  (eopl:error 'report-no-binding-found "No binding for ~s" var))
(define (report-invalid-concrete-syntax datum)
  (eopl:error 'report-invalid-concrete-syntax "invalid concrete syntax: ~a" datum))
(define (report-expval-extractor-error type val)
  (eopl:error 'report-expval-extractor-error "invalid expval - ~a: ~a" type val))

(define (init-env)
  (extend-env (list 'object) (list (newref (prop-val (a-property '() '() '()))))
    (empty-env)))
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
               (newref (proc-val (procedure (car varss) (car bodies) env1)))]
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

(define the-store 'uninit)
; empty-store : () -> Store
(define (empty-store) '())
; get-store : () -> Store
(define (get-store) the-store)
; init-store! : () -> Unspecified
(define (init-store!)
  (set! the-store (empty-store)))
; newref : ExpVal -> Ref
(define (newref val)
  (let ([next-ref (length the-store)])
    (set! the-store (append the-store (list val)))
    next-ref))
; deref : Ref -> ExpVal
(define (deref ref)
  (list-ref the-store ref))
; setref! : Ref x ExpVal -> Unspecified
(define (setref! ref val)
  (define (setref-inner store1 ref1)
    (cond [(null? store1) (report-invalid-reference ref the-store)]
          [(zero? ref1) (cons val (cdr store1))]
          [else (cons (car store1) (setref-inner (cdr store1) (- ref1 1)))]))
  (set! the-store (setref-inner the-store ref)))

(define (expval->property v)
  (cases expval v
    [prop-val [val] val]
    [else (report-expval-extractor-error 'property v)]))

(define find-method
  (lambda (prop method-name)
    (cases property prop
      [a-property [field-names field-refs methods]
        (let ([method-pair (assq method-name methods)])
          (if method-pair
            (cadr method-pair)
            (report-method-not-found)))])))

(define apply-method
  (lambda (m prop args)
    (cases property prop
      [a-property [field-names field-refs methods]
        (cases method m
          [a-method [vars body]
            (value-of body
              (extend-env vars (map newref args)
                (extend-env (list '%self) (list (prop-val prop))
                  (extend-env field-names field-refs
                    (empty-env)))))])])))
