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
               (newref (proc-val (procedure (car varss) (car bodies) env1)))]
              [else 
               (found (cdr names) (cdr varss) (cdr bodies))]))
      (found names varss bodies)]
    ))

; make-pair : ExpVal x ExpVal -> MutPair
(define (make-pair val1 val2)
  (a-pair (newref val1) (newref val2)))
; left : MutPair -> ExpVal
(define (left p)
  (cases mutpair p
    [a-pair [left-loc right-loc]
      (deref left-loc)]))
; right : MutPair -> ExpVal
(define (right p)
  (cases mutpair p
    [a-pair [left-loc right-loc]
      (deref right-loc)]))
; setleft : MutPair x ExpVal -> Unspecified
(define (setleft p val)
  (cases mutpair p
    [a-pair [left-loc right-loc]
      (setref! left-loc val)]))
; setright : MutPair x ExpVal -> Unspecified
(define (setright p val)
  (cases mutpair p
    [a-pair [left-loc right-loc]
      (setref! right-loc val)]))

(define (make-array num val)
  (define (loop idx)
    (if (= idx num)
      (list)
      (cons (newref val) (loop (+ 1 idx)))))
  (let ([arr (loop 0)])
    (an-array num arr)))
(define (array-ref arr1 idx)
  (cases array arr1
    [an-array [len arr]
      (if (< idx len)
        (deref (list-ref arr idx))
        (report-invalid-range 'array-ref arr1 idx))]
    [else (report-invalid-array 'array-ref arr1)]))
(define (array-set! arr1 idx val)
  (cases array arr1
    [an-array [len arr]
      (if (< idx len)
        (setref! (list-ref arr idx) val)
        (report-invalid-range 'array-set! arr1 idx))]
    [else (report-invalid-array 'array-set! arr1)]))
(define (array-len arr1)
  (cases array arr1
    [an-array [len arr] len]
    [else (report-invalid-array 'array-len arr1)]))

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
(define (expval->mutpair val)
  (cases expval val
    [mutpair-val [pair] pair]
    [else (report-expval-extractor-error 'mutpair val)]))
(define (expval->arr val)
  (cases expval val
    [arr-val [arr] arr]
    [else (report-expval-extractor-error 'arr val)]))

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
