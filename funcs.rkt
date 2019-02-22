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

;list-index : Pred x List(SchemeVal) -> Int
(define (list-index pred ls)
  (let loop ([ls ls] [pos 0])
    (cond [(null? ls) #f]
          [(pred (car ls)) pos]
          [else (loop (cdr ls) (+ 1 pos))])))

;list-set : List(SchemeVal) x Int x SchemeVal -> List(SchemeVal)
(define (list-set ls idx val)
  (let loop ([ls ls] [pos 0])
    (cond [(null? ls) '()]
          [(eqv? idx pos) (cons val (cdr ls))]
          [else (cons (car ls) (loop (cdr ls) (+ 1 pos)))])))

;every? : Pred x List(SchemeVal) -> Bool
(define (every? pred ls)
  (if (null? ls) #t (and (pred (car ls)) (every? pred (cdr ls)))))

;fresh-identifier : Symbol -> Symbol
(define g-count 0)
(define (fresh-identifier id)
  (let ([id-str (symbol->string id)])
    (set! g-count (+ 1 g-count))
    (string->symbol (string-append id-str (number->string g-count)))))

(define the-store 'uninit)
;empty-store : () -> Store
(define (empty-store) '())
;get-store : () -> Store
(define (get-store) the-store)
;init-store! : () -> Unspecified
(define (init-store!)
  (set! the-store (empty-store)))
;reference? : SchemeVal -> Bool
(define (reference? v)
  (integer? v))
;newref : ExpVal -> Ref
(define (newref val)
  (let ([next-ref (length the-store)])
    (begin (set! the-store (append the-store (list val))) next-ref)))
;deref : Ref -> ExpVal
(define (deref ref)
  (list-ref the-store ref))
;setref! : Ref x ExpVal -> Unspecified
(define (setref! ref val)
  (define (setref-inner store1 ref1)
    (cond [(null? store1) (report-invalid-reference ref the-store)]
          [(zero? ref1) (cons val (cdr store1))]
          [else (cons (car store1) (setref-inner (cdr store1) (- ref1 1)))]))
  (set! the-store (setref-inner the-store ref)))
