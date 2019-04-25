(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")

(define (run str)
  (value-of-program
    (translator-of-program
      (scan&parse str))))

(define value-of-program
  (lambda (pgm)
    (init-store!)
    (cases program pgm
      [a-program [class-decls body]
        (init-class-env! class-decls)
        (value-of body (init-env))])))

(define (value-of expr env)
  (cases expression expr
    [const-exp [num] 
      (num-val num)]
    [var-exp [var]
      (deref (apply-env env var))]
    [diff-exp [exp1 exp2]
      (let ([val1 (value-of exp1 env)] [val2 (value-of exp2 env)])
        (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
          (num-val (- num1 num2))))]
    [zero?-exp [exp1]
      (let ([val1 (value-of exp1 env)])
        (let ([num1 (expval->num val1)])
          (if (zero? num1)
            (bool-val #t)
            (bool-val #f))))]
    [if-exp [exp1 exp2 exp3]
      (let ([val1 (value-of exp1 env)])
        (if (expval->bool val1)
          (value-of exp2 env)
          (value-of exp3 env)))]
    [let-exp [vars exps body]
      (let ([vals (map (lambda (e) (value-of e env)) exps)])
        (let ([refs (map (lambda (v) (newref v)) vals)])
          (value-of body (extend-env vars refs env))))]
    [proc-exp [vars body]
      (proc-val (procedure vars body env))]
    [call-exp [rator rands]
      (let ([proc1 (expval->proc (value-of rator env))]
            [args (map (lambda (x) (value-of x env)) rands)])
        (cases proc proc1
          [procedure [vars body saved-env]
            (let ([refs (map (lambda (a) (newref a)) args)])
              (value-of body (extend-env vars refs saved-env)))]))]
    [letrec-exp [names varss bodies letrec-body]
      (value-of letrec-body (extend-env-rec names varss bodies env))]
    [assign-exp [var exp1]
      (begin (setref! (apply-env env var) (value-of exp1 env))
             (num-val 27))]
    [begin-exp [exp1 exps]
      (let ([val1 (value-of exp1 env)])
        (foldl (lambda (e v) (value-of e env)) val1 exps))]
    [plus-exp [exp1 exp2]
      (let ([val1 (value-of exp1 env)] [val2 (value-of exp2 env)])
        (num-val (+ (expval->num val1) (expval->num val2))))]
    [list-exp [exps]
      (let ([vals (map (lambda (e) (value-of e env)) exps)])
        (list-val vals))]
    [print-exp [exp1]
      (let ([val1 (value-of exp1 env)])
        (begin (printf "~a~n" val1) val1))]
    [self-expr []
      (apply-env env '%self)]
    [method-call-expr [obj-exp method-name rands]
      (let ([args (map (lambda (e) (value-of e env)) rands)]
            [obj (value-of obj-exp env)])
        (apply-method
          (find-method (object->class-name obj) method-name)
          obj
          args))]
    [super-call-expr [method-name rands]
      (let ([args (map (lambda (e) (value-of e env)) rands)]
            [obj (apply-env env '%self)])
        (apply-method
          (find-method (apply-env env '%super) method-name)
          obj
          args))]
    [new-object-expr [class-name rands]
      (let ([args (map (lambda (e) (value-of e env)) rands)]
            [obj (new-object class-name)])
        (apply-method
          (find-method class-name (generate-method-signature 'init rands))
          obj
          args)
        obj)]
    [else
      (report-invalid-expression expr)]
    ))

(define translator-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program [class-decls body]
        (a-program
          (translator-of-class-decls class-decls)
          (translator-of-expression body))])))

(define translator-of-class-decls
  (lambda (class-decls)
    (let loop ([class-decls class-decls] [trans-class-decls '()])
      (if (null? class-decls)
        (reverse trans-class-decls)
        (cases class-decl (car class-decls)
          [a-class-decl [class-name super-name field-names method-decls]
            (let ([trans-method-decls (translator-of-method-decls method-decls)])
              (loop 
                (cdr class-decls)
                (cons
                  (a-class-decl class-name super-name field-names trans-method-decls)
                  trans-class-decls)))])))))

(define translator-of-method-decls
  (lambda (method-decls)
    (let loop ([method-decls method-decls] [trans-method-decls '()])
      (if (null? method-decls)
        (reverse trans-method-decls)
        (cases method-decl (car method-decls)
          [a-method-decl [method-name vars body]
            (let ([trans-method-name (generate-method-signature method-name vars)])
              (loop 
                (cdr method-decls)
                (cons (a-method-decl trans-method-name vars body) trans-method-decls)))])))))

(define translator-of-expression
  (lambda (expr)
    (cases expression expr
      [const-exp [num] 
        (const-exp num)]
      [var-exp [var]
        (var-exp var)]
      [diff-exp [exp1 exp2]
        (let ([trans-exps (map translator-of-expression (list exp1 exp2))])
          (apply diff-exp trans-exps))]
      [zero?-exp [exp1]
        (zero?-exp (translator-of-expression exp1))]
      [if-exp [exp1 exp2 exp3]
        (let ([trans-exps (map translator-of-expression (list exp1 exp2 exp3))])
          (apply if-exp trans-exps))]
      [let-exp [vars exps body]
        (let ([trans-exps (map translator-of-expression exps)]
              [trans-body (translator-of-expression body)])
          (let-exp vars trans-exps trans-body))]
      [proc-exp [vars body]
        (proc-exp vars (translator-of-expression body))]
      [call-exp [rator rands]
        (let ([trans-rator (translator-of-expression rator)]
              [trans-rands (map translator-of-expression rands)])
          (call-exp trans-rator trans-rands))]
      [letrec-exp [names varss procs rbody]
        (let ([trans-procs (map translator-of-expression procs)]
              [trans-rbody (translator-of-expression rbody)])
          (letrec-exp names varss trans-procs trans-rbody))]
      [assign-exp [var exp1]
        (assign-exp var (translator-of-expression exp1))]
      [begin-exp [exp1 exps]
        (let ([trans-exp1 (translator-of-expression exp1)]
              [trans-exps (map translator-of-expression exps)])
          (begin-exp trans-exp1 trans-exps))]
      [plus-exp [exp1 exp2]
        (let ([trans-exps (map translator-of-expression (list exp1 exp2))])
          (apply plus-exp trans-exps))]
      [list-exp [exps]
        (list-exp (map translator-of-expression exps))]
      [print-exp [exp1]
        (print-exp (translator-of-expression exp1))]
      [self-expr []
        (self-expr)]
      [method-call-expr [obj-exp method-name rands]
        (let ([trans-method-name (generate-method-signature method-name rands)]
              [trans-obj-exp (translator-of-expression obj-exp)]
              [trans-rands (map translator-of-expression rands)])
          (method-call-expr trans-obj-exp trans-method-name trans-rands))]
      [super-call-expr [method-name rands]
        (let ([trans-method-name (generate-method-signature method-name rands)]
              [trans-rands (map translator-of-expression rands)])
          (super-call-expr trans-method-name trans-rands))]
      [new-object-expr [class-name rands]
        (let ([trans-rands (map translator-of-expression rands)])
          (new-object-expr class-name trans-rands))]
      [else
        (report-invalid-expression expr)]
    )))

(define generate-method-signature
  (lambda (method-name method-vars)
    (let* ([vars-count (number->string (length method-vars))]
           [name-string (symbol->string method-name)])
      (string->symbol (string-append name-string ":@" vars-count)))))

;(trace init-class-env!)
;(trace init-class-decl!)
;(trace lookup-class)
;(trace append-field-names)
;(trace method-decls->method-env)
;(trace apply-method)
;(trace new-object)
;(trace find-method)
;(trace value-of)
