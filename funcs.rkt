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
    (if (not m)
      (report-method-not-found)
      (let ([object (find-object-with-class self (method->class-name m))])
        (if (not object)
          (report-invalid-object-to-method)
          (cases method m
            [a-method [vars body class-name field-names]
              (let ([class (lookup-class class-name)])
                (value-of body
                  (extend-env vars (map newref args)
                    (extend-env-with-self-and-super object (class->super-name class)
                      (extend-env field-names (object->fields object)
                        (empty-env))))))]))))))

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
        (list 'object (a-class #f '() '() '()))))
    (for-each init-class-decl! c-decls)))

(define init-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      [a-class-decl [c-name s-name mixins-names f-names m-decls]
        (let ([f-names (append-field-names (class->field-names (lookup-class s-name)) f-names)])
          (add-to-class-env!
            c-name
            (a-class s-name mixins-names f-names
              (method-decls->method-env m-decls c-name f-names))))])))

(define append-field-names
  (lambda (super-fields new-fields)
    (cond [(null? super-fields) new-fields]
          [else
           (cons (if (memq (car super-fields) new-fields)
                   (fresh-identifier (car super-fields))
                   (car super-fields))
                 (append-field-names (cdr super-fields) new-fields))])))

(define find-method
  (lambda (class-name method-name)
    (let loop ([class-name class-name])
      (if (not class-name)
        #f
        (let* ([class (lookup-class class-name)]
               [m-env (class->method-env class)]
               [mixins-names (class->mixins-names class)])
          (let ([maybe-pair (assq method-name m-env)])
            (if (pair? maybe-pair)
              (cadr maybe-pair)
              (let ([method (find-method-in-mixins mixins-names method-name)])
                (if method
                  method
                  (loop (class->super-name class)))))))))))

(define find-method-in-mixins
  (lambda (mixins-names method-name)
    (let loop ([mixins-names mixins-names])
      (if (null? mixins-names)
        #f
        (let ([found-method (find-method (car mixins-names) method-name)])
          (if found-method
            found-method
            (loop (cdr mixins-names))))))))

(define find-object-with-class
  (lambda (obj class-name)
    (if (is-subclass-of-class (object->class-name obj) class-name)
      obj
      (let loop ([mixins-objs (object->mixins-objs obj)])
        (if (null? mixins-objs)
          #f
          (let ([found-obj (find-object-with-class (car mixins-objs) class-name)])
            (if found-obj
              found-obj
              (loop (cdr mixins-objs)))))))))

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
    [a-class [super-name mixins-names field-names method-env] super-name]
    [else (report-invalid-extract 'class c)]))

(define (class->mixins-names c)
  (cases class c
    [a-class [super-name mixins-names field-names method-env] mixins-names]
    [else (report-invalid-extract 'class c)]))

(define (class->field-names c)
  (cases class c
    [a-class [super-name mixins-names field-names method-env] field-names]
    [else (report-invalid-extract 'class c)]))

(define (class->method-env c)
  (cases class c
    [a-class [super-name mixins-names field-names method-env] method-env]
    [else (report-invalid-extract 'class c)]))

(define g-seed 0)
(define (fresh-identifier id)
  (set! g-seed (+ g-seed 1))
  (string->symbol (string-append (symbol->string id) (number->string g-seed))))

(define (object->fields o)
  (cases object o
    [an-object [class-name fields mixins-objs] fields]
    [else (report-invalid-extract 'object o)]))

(define (object->class-name o)
  (cases object o
    [an-object [class-name fields mixins-objs] class-name]
    [else (report-invalid-extract 'object o)]))

(define (object->mixins-objs o)
  (cases object o
    [an-object [class-name fields mixins-objs] mixins-objs]
    [else (report-invalid-extract 'object o)]))

(define (method->class-name m)
  (cases method m
    [a-method [vars body class-name field-names] class-name]
    [else (report-invalid-extract 'method m)]))

(define is-subclass-of-class
  (lambda (class-name target-class-name)
    (let loop ([class-name class-name])
      (cond [(not class-name) #f]
            [(eqv? class-name target-class-name) #t]
            [else (loop (class->super-name (lookup-class class-name)))]))))

(define new-object
  (lambda (class-name)
    (let ([class (lookup-class class-name)])
      (an-object
        class-name
        (map (lambda (field-name) (newref (list 'uninit-field field-name))) 
             (class->field-names class))
        (mixins-new-object class-name)))))

(define mixins-new-object
  (lambda (class-name)
    (let loop1 ([class-name class-name] [obj-pairs '()])
      (if (not class-name)
        (reverse obj-pairs)
        (let ([class (lookup-class class-name)])
          (let loop2 ([mixins-names (class->mixins-names class)] [obj-pairs obj-pairs])
            (if (null? mixins-names)
              (loop1 (class->super-name class) obj-pairs)
              (loop2
                (cdr mixins-names) 
                (cons (new-object (car mixins-names)) obj-pairs)))))))))
