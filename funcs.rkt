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
  (lambda (m self args)
    (cases method m
      [a-method [vars body super-name field-names]
        (value-of body
          (extend-env vars (map newref args)
            (extend-env-with-self-and-super self super-name
              (extend-env field-names (object->fields self)
                (empty-env)))))])))

(define extend-env-with-self-and-super
  (lambda (self super-name env)
    (extend-env (list '%self '%super) (list self super-name) env)))

(define the-class-env '())
(define add-to-class-env!
  (lambda (class-name cls)
    (set! the-class-env
      (cons (list class-name cls) the-class-env))))
(define lookup-class
  (lambda (name)
    (let ([maybe-pair (assq name the-class-env)])
      (if maybe-pair
        (cadr maybe-pair)
        (report-unknown-class name)))))

(define init-class-env!
  (lambda (c-decls)
    (set! the-class-env
      (list
        (list 'object (a-class #f '() '()))))
    (for-each init-class-decl! c-decls)))

(define init-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      [a-class-decl [c-name s-name f-names m-decls]
        (let ([f-names (append-field-names (class->field-names (lookup-class s-name)) f-names)])
          (add-to-class-env!
            c-name
            (a-class s-name f-names
              (merge-method-envs
                (class->method-env (lookup-class s-name))
                (method-decls->method-env m-decls s-name f-names)))))])))

(define append-field-names
  (lambda (super-fields new-fields)
    (cond [(null? super-fields) new-fields]
          [else
           (cons (if (memq (car super-fields) new-fields)
                   (fresh-identifier (car super-fields))
                   (car super-fields))
                 (append-field-names (cdr super-fields) new-fields))])))

(define find-method
  (lambda (c-name name)
    (let ([m-env (class->method-env (lookup-class c-name))])
      (let ([maybe-pair (assq name m-env)])
        (if (pair? maybe-pair)
          (cadr maybe-pair)
          (report-method-not-found name))))))

(define method-decls->method-env
  (lambda (m-decls super-name field-names)
    (map (lambda (m-decl)
           (cases method-decl m-decl
             [a-method-decl [method-name vars body]
               (let ([optimize-body (optimize-super-call body super-name)])
                 (list method-name (a-method vars optimize-body super-name field-names)))]))
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

(define optimize-super-call
  (lambda (body super-name)
    (cases expression body
      [const-exp [num] 
        (const-exp num)]
      [var-exp [var]
        (var-exp var)]
      [diff-exp [exp1 exp2]
        (let ([optimized-exps (optimize-super-call-exps super-name (list exp1 exp2))])
          (apply diff-exp optimized-exps))]
      [zero?-exp [exp1]
        (zero?-exp (optimize-super-call exp1 super-name))]
      [if-exp [exp1 exp2 exp3]
        (let ([optimized-exps (optimize-super-call-exps super-name (list exp1 exp2))])
          (apply if-exp optimized-exps))]
      [let-exp [vars exps body]
        (let ([optimized-exps (optimize-super-call-exps super-name (cons body exps))])
          (let-exp vars (cdr optimized-exps) (car optimized-exps)))]
      [proc-exp [vars body]
        (proc-exp vars (optimize-super-call body super-name))]
      [call-exp [rator rands]
        (let ([optimized-exps (optimize-super-call-exps super-name (cons rator rands))])
          (call-exp (car optimized-exps) (cdr optimized-exps)))]
      [letrec-exp [names varss bodies letrec-body]
        (let ([optimized-exps (optimize-super-call-exps super-name (cons letrec-body bodies))])
          (letrec-exp names varss (cdr optimized-exps) (car optimized-exps)))]
      [assign-exp [var exp1]
        (assign-exp var (optimize-super-call exp1 super-name))]
      [begin-exp [exp1 exps]
        (let ([optimized-exps (optimize-super-call-exps super-name (cons exp1 exps))])
          (begin-exp (car optimized-exps) (cdr optimized-exps)))]
      [plus-exp [exp1 exp2]
        (let ([optimized-exps (optimize-super-call-exps super-name (list exp1 exp2))])
          (apply plus-exp optimized-exps))]
      [list-exp [exps]
        (let ([optimized-exps (optimize-super-call-exps super-name exps)])
          (list-exp optimized-exps))]
      [print-exp [exp1]
        (print-exp (optimize-super-call exp1 super-name))]
      [self-expr []
        (self-expr)]
      [method-call-expr [obj-exp method-name rands]
        (let ([optimized-exps (optimize-super-call-exps super-name (cons obj-exp rands))])
          (method-call-expr (car optimized-exps) method-name (cdr optimized-exps)))]
      [super-call-expr [method-name rands]
        (static-super-call (find-method super-name method-name) rands)]
      [new-object-expr [class-name rands]
        (let ([optimized-exps (optimize-super-call-exps super-name rands)])
          (new-object-expr class-name optimized-exps))]
      [else
        (report-invalid-expression expr)]
      )))

(define optimize-super-call-exps
  (lambda (super-name exps)
    (map (lambda (e) (optimize-super-call e super-name)) exps)))
