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
      [a-program [body]
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
      (deref (apply-env env '%self))]
    [newobject-expr [super-exp method-names method-procs]
      (let ([super-val (value-of super-exp env)]
            [method-procs-vals (map (lambda (m) (value-of m env)) method-procs)])
        (obj-val (an-object (expval->object super-val) method-names method-procs-vals env)))]
    [getmethod-expr [obj-exp method-name]
      (let ([obj-val (value-of obj-exp env)])
        (find-method obj-val method-name))]
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
