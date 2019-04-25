(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")

(define (run str)
  (value-of-program
    (scan&parse str)))

(define value-of-program
  (lambda (pgm)
    (init-store!)
    (cases program pgm
      [a-program [class-decls body]
        (value-of body (init-env) (init-class-env class-decls))])))

(define (value-of expr env class-env)
  (cases expression expr
    [const-exp [num] 
      (num-val num)]
    [var-exp [var]
      (deref (apply-env env var))]
    [diff-exp [exp1 exp2]
      (let ([val1 (value-of exp1 env class-env)] [val2 (value-of exp2 env class-env)])
        (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
          (num-val (- num1 num2))))]
    [zero?-exp [exp1]
      (let ([val1 (value-of exp1 env class-env)])
        (let ([num1 (expval->num val1)])
          (if (zero? num1)
            (bool-val #t)
            (bool-val #f))))]
    [if-exp [exp1 exp2 exp3]
      (let ([val1 (value-of exp1 env class-env)])
        (if (expval->bool val1)
          (value-of exp2 env class-env)
          (value-of exp3 env class-env)))]
    [let-exp [vars exps body]
      (let ([vals (map (lambda (e) (value-of e env class-env)) exps)])
        (let ([refs (map (lambda (v) (newref v)) vals)])
          (value-of body (extend-env vars refs env) class-env)))]
    [proc-exp [vars body]
      (proc-val (procedure vars body env))]
    [call-exp [rator rands]
      (let ([proc1 (expval->proc (value-of rator env class-env))]
            [args (map (lambda (x) (value-of x env class-env)) rands)])
        (cases proc proc1
          [procedure [vars body saved-env]
            (let ([refs (map (lambda (a) (newref a)) args)])
              (value-of body (extend-env vars refs saved-env) class-env))]))]
    [letrec-exp [names varss bodies letrec-body]
      (value-of letrec-body (extend-env-rec names varss bodies env class-env))]
    [assign-exp [var exp1]
      (begin (setref! (apply-env env var) (value-of exp1 env class-env))
             (num-val 27))]
    [begin-exp [exp1 exps]
      (let ([val1 (value-of exp1 env class-env)])
        (foldl (lambda (e v) (value-of e env class-env)) val1 exps))]
    [plus-exp [exp1 exp2]
      (let ([val1 (value-of exp1 env class-env)] [val2 (value-of exp2 env class-env)])
        (num-val (+ (expval->num val1) (expval->num val2))))]
    [list-exp [exps]
      (let ([vals (map (lambda (e) (value-of e env class-env)) exps)])
        (list-val vals))]
    [print-exp [exp1]
      (let ([val1 (value-of exp1 env class-env)])
        (begin (printf "~a~n" val1) val1))]
    [self-expr []
      (apply-env env '%self)]
    [method-call-expr [obj-exp method-name rands]
      (let ([args (map (lambda (e) (value-of e env class-env)) rands)]
            [obj (value-of obj-exp env class-env)])
        (apply-method
          (find-method (object->class-name obj) method-name class-env)
          obj
          args
          class-env))]
    [super-call-expr [method-name rands]
      (let ([args (map (lambda (e) (value-of e env class-env)) rands)]
            [obj (apply-env env '%self)])
        (apply-method
          (find-method (apply-env env '%super) method-name class-env)
          obj
          args
          class-env))]
    [new-object-expr [class-name rands]
      (let ([args (map (lambda (e) (value-of e env class-env)) rands)]
            [obj (new-object class-name class-env)])
        (apply-method
          (find-method class-name 'init class-env)
          obj
          args
          class-env)
        obj)]
    [letclass-expr [class-name class-decl body]
      (let ([ext-class-env 
              (extend-class-env class-name (class-decl->class class-decl class-env) class-env)])
        (value-of body env ext-class-env))]
    [else
      (report-invalid-expression expr)]
    ))

;(trace init-class-env!)
;(trace init-class-decl!)
;(trace lookup-class)
;(trace append-field-names)
;(trace method-decls->method-env)
;(trace apply-method)
;(trace new-object)
;(trace find-method)
;(trace value-of)
;(trace apply-class-env)
