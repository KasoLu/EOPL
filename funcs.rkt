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

(define apply-method
  (lambda (m self args class-env)
    (cases method m
      [a-method [vars body super-name field-names]
        (value-of
          body
          (extend-env vars (map newref args)
            (extend-env-with-self-and-super self super-name
              (extend-env field-names (object->fields self)
                (empty-env))))
          class-env)])))

(define extend-env-with-self-and-super
  (lambda (self super-name env)
    (extend-env (list '%self '%super) (list self super-name) env)))

(define empty-class-env
  (lambda () '()))
(define extend-class-env
  (lambda (name class env)
    (cons (list name class) env)))
(define (apply-class-env env name)
  (let ([maybe-pair (assq name env)])
    (if maybe-pair
      (cadr maybe-pair)
      (report-unknown-class name))))

(define init-class-env
  (lambda (class-decls)
    (let ([class-env (extend-class-env 'object (a-class #f '() '()) (empty-class-env))])
      (let loop ([class-decls class-decls] [class-env class-env])
        (if (null? class-decls)
          class-env
          (let ([class-decl (car class-decls)])
            (let ([class (class-decl->class class-decl class-env)])
              (loop 
                (cdr class-decls)
                (extend-class-env (class-decl->class-name class-decl) class class-env)))))))))

(define class-decl->class
  (lambda (c-decl class-env)
    (cases class-decl c-decl 
      [a-class-decl [class-name super-name field-names method-decls]
        (let* ([super-class (apply-class-env class-env super-name)]
               [super-fields (class->field-names super-class)]
               [super-methods (class->method-env super-class)]
               [class-fields (append-field-names super-fields field-names)]
               [class-methods (method-decls->method-env method-decls super-name field-names)]
               [merge-methods (merge-method-envs class-methods super-methods)])
          (a-class super-name class-fields merge-methods))])))

(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      [a-class-decl [class-name super-name field-names method-decls] class-name]
      [else (report-class-decl-extract-error)])))

(define append-field-names
  (lambda (super-fields new-fields)
    (cond [(null? super-fields) new-fields]
          [else
           (cons (if (memq (car super-fields) new-fields)
                   (fresh-identifier (car super-fields))
                   (car super-fields))
                 (append-field-names (cdr super-fields) new-fields))])))

(define find-method
  (lambda (c-name name class-env)
    (let ([m-env (class->method-env (apply-class-env class-env c-name))])
      (let ([maybe-pair (assq name m-env)])
        (if (pair? maybe-pair)
          (cadr maybe-pair)
          (report-method-not-found name))))))

(define method-decls->method-env
  (lambda (m-decls super-name field-names)
    (map (lambda (m-decl)
           (cases method-decl m-decl
             [a-method-decl [method-name vars body]
               (list method-name (a-method vars body super-name field-names))])) 
         m-decls)))

(define merge-method-envs
  (lambda (super-m-env new-m-env)
    (append new-m-env super-m-env)))

(define (class->super-name c)
  (cases class c
    [a-class [super-name field-names method-env] super-name]
    [else (report-invalid-extract 'class c)]))

(define (class->field-names c)
  (cases class c
    [a-class [super-name field-names method-env] field-names]
    [else (report-invalid-extract 'class c)]))

(define (class->method-env c)
  (cases class c
    [a-class [super-name field-names method-env] method-env]
    [else (report-invalid-extract 'class c)]))

(define g-seed 0)
(define (fresh-identifier id)
  (set! g-seed (+ g-seed 1))
  (string->symbol (string-append (symbol->string id) (number->string g-seed))))

(define (object->fields o)
  (cases object o
    [an-object [class-name fields] fields]
    [else (report-invalid-extract 'object o)]))

(define (object->class-name o)
  (cases object o
    [an-object [class-name fields] class-name]
    [else (report-invalid-extract 'object o)]))

(define new-object
  (lambda (class-name)
    (an-object
      class-name
      (map (lambda (field-name) (newref (list 'uninit-field field-name))) 
           (class->field-names (lookup-class class-name))))))
