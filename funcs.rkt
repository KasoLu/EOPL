(load "types.rkt")

(require racket/trace)
(define-syntax lambda
  (syntax-rules ()
    [(_ kw body ...) (trace-lambda kw body ...)]))

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

(define the-static-class-env 'uninit)

(define empty-the-static-class-env!
  (lambda () (set! the-static-class-env '())))

(define add-static-class-binding!
  (lambda (class-name static-class)
    (set! the-static-class-env 
      (cons (list class-name static-class) the-static-class-env))))

(define lookup-static-class
  (lambda (class-name)
    (let ([maybe-pair (assq class-name the-static-class-env)])
      (if maybe-pair
        (cadr maybe-pair)
        (report-static-class-not-found)))))

(define abs-method-decls->method-tenv
  (lambda (abs-method-decls)
    (map (lambda (abs-m-decl)
           (cases abs-method-decl abs-m-decl
             [an-abs-method-decl [result-type method-name method-vars method-vars-types]
               (list method-name (proc-type method-vars-types result-type))])) 
         abs-method-decls)))

(define method-decls->method-tenv
  (lambda (method-decls)
    (map (lambda (m-decl)
           (cases method-decl m-decl
             [a-method-decl [result-type method-name vars vars-types body]
               (list method-name (proc-type vars-types result-type))]))
         method-decls)))

(define merge-method-tenvs
  (lambda (super-method-tenv new-method-tenv)
    (append super-method-tenv new-method-tenv)))

(define check-no-dups!
  (lambda (lst name)
    (let loop1 ([lst lst] [checked-lst '()])
      (if (null? lst)
        (void)
        (let loop2 ([lst-temp checked-lst])
          (cond [(null? lst-temp) (loop1 (cdr lst) (cons (car lst) checked-lst))]
                [(eqv? (car lst) (car lst-temp)) (report-dups)]
                [else (loop2 (cdr lst-temp))]))))))

(define check-for-init!
  (lambda (method-tenv class-name)
    (let ([maybe-pair (assq 'init method-tenv)])
      (if maybe-pair
        (void)
        (report-not-found-init-method)))))

(define append-field-names
  (lambda (super-fields new-fields)
    (cond [(null? super-fields) new-fields]
          [else
           (cons (if (memq (car super-fields) new-fields)
                   (fresh-identifier (car super-fields))
                   (car super-fields))
                 (append-field-names (cdr super-fields) new-fields))])))

(define init-tenv init-env)
(define apply-tenv apply-env)
(define extend-tenv extend-env)
(define extend-tenv-with-self-and-super extend-env-with-self-and-super)

;check-equal-type! : Type x Type x Expr -> Void
(define (check-equal-type! ty1 ty2 expr)
  (if (not (equal? ty1 ty2))
    (report-unequal-types ty1 ty2 expr)
    (void)))

;report-unequal-types : Type x Type x Expr -> Void
(define (report-unequal-types ty1 ty2 expr)
  (eopl:error 'check-equal-type! "Types didn't match: ~s != ~a in ~%~a"
              (type-to-external-form ty1)
              (type-to-external-form ty2)
              expr))

;type-to-external-form : Type -> List
(define (type-to-external-form ty)
  (cases type ty
    [void-type [] 'Void]
    [int-type  [] 'Int]
    [bool-type [] 'Bool]
    [proc-type [args-type ret-type]
      (let ([type-form 
              (foldr (lambda (at acc) (cons '* (cons (type-to-external-form at) acc))) 
                     (list '-> (type-to-external-form ret-type))
                     args-type)])
        (if (eqv? (car type-form) '->)
          (cons '() type-form)
          (cdr type-form)))]
    [list-type [elem-type]
      (list 'Listof (type-to-external-form elem-type))]
    [class-type [class-name]
      class-name]
    ))

(define check-is-subtype!
  (lambda (ty1 ty2 expr)
    (if (is-subtype? ty1 ty2)
      #t
      (report-subtype-failure
        (type-to-external-form ty1)
        (type-to-external-form ty2)
        expr))))

(define is-subtype?
  (lambda (ty1 ty2)
    (cases type ty1
      [class-type [class-name1]
        (cases type ty2
          [class-type [class-name2]
            (statically-is-subclass? class-name1 class-name2)]
          [else #f])]
      [proc-type [args-types1 return-type1]
        (cases type ty2
          [proc-type [args-types2 return-type2]
            (and
              (every2? is-subtype? args-types2 args-types1)
              (is-subtype? return-type1 return-type2))]
          [else #f])]
      [else (equal? ty1 ty2)])))

(define statically-is-subclass?
  (lambda (class-name1 class-name2)
    (or
      (eqv? class-name1 class-name2)
      (let ([super-name (static-class->super-name (lookup-static-class class-name1))])
        (if super-name
          (statically-is-subclass? super-name class-name2)
          #f))
      (let ([interface-names (static-class->interface-names (lookup-static-class class-name1))])
        (memv class-name2 interface-names)))))

(define every2?
  (lambda (pred ls1 ls2)
    (cond [(and (null? ls1) (null? ls2)) #t]
          [(or  (null? ls1) (null? ls2)) #f]
          [(and (pred (car ls1) (car ls2)) (every2? pred (cdr ls1) (cdr ls2)))])))

(define init-static-class-env!
  (lambda (class-decls)
    (empty-the-static-class-env!)
    (add-static-class-binding! 'object (a-static-class #f '() '() '() '()))
    (add-static-class-binding! 'ifaces (an-interface #f '()))
    (for-each add-class-decl-to-static-class-env! class-decls)))

(define add-class-decl-to-static-class-env!
  (lambda (c-decl)
    (cases class-decl c-decl
      [an-interface-decl [i-name super-name abs-method-decls]
        (let ([super-class (lookup-static-class super-name)]
              [m-tenv (abs-method-decls->method-tenv abs-method-decls)])
          (begin (check-no-dups! (map car m-tenv) i-name)
                 (let ([m-tenv (merge-method-tenvs (static-class->method-tenv super-class) m-tenv)])
                   (add-static-class-binding! i-name (an-interface super-name m-tenv)))))]
      [a-class-decl [c-name s-name i-names f-types f-names m-decls]
        (let* ([super-class (lookup-static-class s-name)]
               [i-names (append (static-class->interface-names super-class) i-names)]
               [f-names (append-field-names (static-class->field-names super-class) f-names)]
               [f-types (append (static-class->field-types super-class) f-types)]
               [method-tenv
                (let ([local-method-tenv (method-decls->method-tenv m-decls)])
                  (begin (check-no-dups! (map car local-method-tenv) c-name)
                         (merge-method-tenvs 
                           (static-class->method-tenv super-class)
                           local-method-tenv)))])
          (begin (check-no-dups! i-names c-name)
                 (check-no-dups! f-names c-name)
                 (check-for-init! method-tenv c-name)
                 (add-static-class-binding! 
                   c-name 
                   (a-static-class s-name i-names f-names f-types method-tenv))))])))

(define check-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      [an-interface-decl [i-name s-name abs-method-decls] #t]
      [a-class-decl [c-name s-name i-names f-types f-names m-decls]
        (let ([sc (lookup-static-class c-name)])
          (for-each
            (lambda (m-decl)
              (check-method-decl! 
                m-decl c-name s-name 
                (static-class->field-names sc)
                (static-class->field-types sc)))
            m-decls))
        (for-each (lambda (i-name) (check-if-implements! c-name i-name)) i-names)])))

(define check-method-decl!
  (lambda (m-decl self-name s-name f-names f-types)
    (cases method-decl m-decl
      [a-method-decl [res-type m-name vars var-types body]
        (let ([tenv 
               (extend-tenv vars var-types
                 (extend-tenv-with-self-and-super (class-type self-name) s-name
                   (extend-tenv f-names f-types
                     (init-tenv))))])
          (let ([body-type (type-of-expr body tenv)])
            (check-is-subtype! body-type res-type m-decl)
            (if (eqv? m-name 'init)
              #t
              (let ([maybe-super-type 
                     (maybe-find-method-type
                       (static-class->method-tenv
                         (lookup-static-class s-name))
                       m-name)])
                (if maybe-super-type
                  (check-is-subtype!
                    (proc-type var-types res-type)
                    maybe-super-type
                    body)
                  #t)))))])))

(define check-if-implements!
  (lambda (c-name i-name)
    (cases static-class (lookup-static-class i-name)
      [a-static-class [s-name i-names f-names f-types m-tenv]
        (report-cant-implement-non-interface c-name i-name)]
      [an-interface [s-name method-tenv]
        (let ([class-method-tenv (static-class->method-tenv (lookup-static-class c-name))])
          (for-each 
            (lambda (method-binding)
              (let ([m-name (car method-binding)]
                    [m-type (cadr method-binding)])
                (let ([c-method-type (maybe-find-method-type class-method-tenv m-name)])
                  (if c-method-type
                    (check-is-subtype! c-method-type m-type c-name)
                    (report-missing-method c-name i-name m-name)))))
            method-tenv))])))

(define static-class->interface-names
  (lambda (sc)
    (cases static-class sc
      [a-static-class [s-name i-names f-names f-types m-tenv] i-names]
      [else (report-extractor-error)])))

(define static-class->field-types
  (lambda (sc)
    (cases static-class sc
      [a-static-class [s-name i-names f-names f-types m-tenv] f-types]
      [else (report-extractor-error)])))

(define static-class->method-tenv
  (lambda (sc)
    (cases static-class sc
      [a-static-class [s-name i-names f-names f-types m-tenv] m-tenv]
      [an-interface [s-name m-tenv] m-tenv]
      [else (report-extractor-error)])))

(define static-class->field-names
  (lambda (sc)
    (cases static-class sc
      [a-static-class [s-name i-names f-names f-types m-tenv] f-names]
      [else (report-extractor-error)])))

(define maybe-find-method-type
  (lambda (method-tenv method-name)
    (let ([maybe-pair (assq method-name method-tenv)])
      (if maybe-pair
        (cadr maybe-pair)
        #f))))

(define find-method-type
  (lambda (class-name method-name)
    (let* ([class (lookup-static-class class-name)]
           [method-tenv (static-class->method-tenv class)]
           [maybe-pair (assq method-name method-tenv)])
      (if maybe-pair
        (cadr maybe-pair)
        (report-method-type-not-found)))))

(define type->class-name
  (lambda (t)
    (cases type t
      [class-type [class-name] class-name]
      [else (report-extractor-error)])))

(define class-type?
  (lambda (t)
    (cases type t
      [class-type [class-name] #t]
      [else #f])))

(define static-class->super-name
  (lambda (sc)
    (cases static-class sc
      [a-static-class [s-name i-names f-names f-types m-tenv] s-name]
      [else (report-extractor-error)])))
