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
    [extend-env-class-scope [class-name saved-env]
      (apply-env saved-env var)]
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
  (lambda (m self args curr-class-name)
    (cases method m
      [a-method [vars body permission class-name]
        (let* ([cls (lookup-class class-name)]
               [fields-names (generate-field-names class-name (class->field-env cls))]
               [ext-env (extend-env fields-names (object->fields self) (empty-env))]
               [ext-env (extend-env-with-self-and-super self (class->super-name cls) ext-env)]
               [ext-env (extend-env vars (map newref args) ext-env)]
               [ext-env (extend-env-class-scope (object->class-name self) ext-env)])
          (cases method-permission permission
            [private-method-permission []
              (if (eqv? curr-class-name class-name)
                (value-of body ext-env)
                (report-method-permission-error m))]
            [protected-method-permission []
              (if (is-subclass-of-class curr-class-name class-name)
                (value-of body ext-env)
                (report-method-permission-error m))]
            [public-method-permission []
              (value-of body ext-env)]))])))

(define generate-field-names
  (lambda (curr-class-scope fields)
    (let loop ([fields fields] [field-names '()])
      (if (null? fields)
        (reverse field-names)
        (cases field (cadar fields)
          [a-field [name permission class-name]
            (cases field-permission permission
              [private-field-permission []
                (if (eqv? class-name curr-class-scope)
                  (loop (cdr fields) (cons name field-names))
                  (loop (cdr fields) (cons (fresh-identifier name) field-names)))]
              [protected-field-permission []
                (if (is-subclass-of-class curr-class-scope class-name)
                  (loop (cdr fields) (cons name field-names))
                  (loop (cdr fields) (cons (fresh-identifier name) field-names)))]
              [public-field-permission []
                (loop (cdr fields) (cons name field-names))])])))))

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
      [a-class-decl [c-name s-name f-decls m-decls]
        (let* ([super-class (lookup-class s-name)]
               [super-fields (class->field-env super-class)]
               [class-fields (field-decls->field-env f-decls c-name)]
               [class-fields (merge-field-envs super-fields class-fields)]
               [class-method (merge-method-envs (class->method-env super-class)
                                                (method-decls->method-env m-decls c-name))])
          (add-to-class-env! c-name (a-class s-name class-fields class-method)))])))

(define find-method
  (lambda (c-name name)
    (let ([m-env (class->method-env (lookup-class c-name))])
      (let ([maybe-pair (assq name m-env)])
        (if (pair? maybe-pair)
          (cadr maybe-pair)
          (report-method-not-found name))))))

(define field-decls->field-env
  (lambda (f-decls class-name)
    (map (lambda (f-decl)
           (cases field-decl f-decl
             [a-field-decl [field-permission field-name]
               (list field-name (a-field field-name field-permission class-name))]))
         f-decls)))

(define method-decls->method-env
  (lambda (m-decls class-name)
    (map (lambda (m-decl)
           (cases method-decl m-decl
             [a-method-decl [method-permission method-name vars body]
               (list method-name (a-method vars body method-permission class-name))])) 
         m-decls)))

(define merge-field-envs
  (lambda (super-fields new-fields)
    (cond [(null? super-fields) new-fields]
          [else
           (cons (if (memq (car super-fields) new-fields)
                   (fresh-field-name (car super-fields))
                   (car super-fields))
                 (merge-field-envs (cdr super-fields) new-fields))])))

(define merge-method-envs
  (lambda (super-m-env new-m-env)
    (append new-m-env super-m-env)))

(define (class->super-name c)
  (cases class c
    [a-class [super-name field-names method-env] super-name]
    [else (report-invalid-extract 'class c)]))

(define (class->field-env c)
  (cases class c
    [a-class [super-name field-env method-env] field-env]
    [else (report-invalid-extract 'class c)]))

(define (class->method-env c)
  (cases class c
    [a-class [super-name field-names method-env] method-env]
    [else (report-invalid-extract 'class c)]))

(define (class->super-name c)
  (cases class c
    [a-class [super-name field-names method-env] super-name]
    [else (report-invalid-extract 'class c)]))

(define g-seed 0)
(define (fresh-identifier id)
  (set! g-seed (+ g-seed 1))
  (string->symbol (string-append (symbol->string id) (number->string g-seed))))
(define (fresh-field-name f)
  (cases field f
    [a-field [name permission class-name]
      (a-field (fresh-identifier name) permission class-name)]))

(define (object->fields o)
  (cases object o
    [an-object [class-name fields] fields]
    [else (report-invalid-extract 'object o)]))

(define (object->class-name o)
  (cases object o
    [an-object [class-name fields] class-name]
    [else (report-invalid-extract 'object o)]))

(define (field->name f)
  (cases field f
    [a-field [name permission class-name] name]
    [else (report-invalid-extract 'field f)]))

(define is-subclass-of-class
  (lambda (class-name target-class-name)
    (let loop ([class-name class-name])
      (cond [(not class-name) #f]
            [(eqv? class-name target-class-name) #t]
            [else (loop (class->super-name (lookup-class class-name)))]))))

(define curr-class-scope
  (lambda (e)
    (cases env e
      [empty-env []
        (report-invalid-env)]
      [extend-env [vars vals saved-env]
        (curr-class-scope saved-env)]
      [extend-env-rec [names varss procs saved-env]
        (curr-class-scope saved-env)]
      [extend-env-class-scope [class-name saved-env]
        class-name])))

(define (class->field-names c)
  (let ([field-env (class->field-env c)])
    (let loop ([field-env field-env] [field-names '()])
      (if (null? field-env)
        (reverse field-names)
        (cases field (cadar field-env)
          [a-field [name permission class-name]
            (loop (cdr field-env) (cons name field-names))])))))

(define new-object
  (lambda (class-name)
    (an-object
      class-name
      (map (lambda (field-name) (newref (list 'uninit-field field-name))) 
           (class->field-names (lookup-class class-name))))))
