(load "types.rkt")

(define (report-no-binding-found var)
  (eopl:error 'report-no-binding-found "No binding for ~s" var))
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
    [else
      (report-invalid-env env1)]
    ))

(define init-tenv init-env)
(define apply-tenv apply-env)
(define extend-tenv extend-env)

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

(define (decl->name d)
  (cases decl d
    [val-decl [name type] name]
    [opaque-type-decl [t-name] 'type-decl]
    [transparent-type-decl [t-name t-type] 'type-decl]
    [else (report-decl-extractor-error 'name d)]))
(define (decl->type d)
  (cases decl d
    [val-decl [name type] type]
    [opaque-type-decl [t-name] (qualified-type 'module-x t-name)]
    [transparent-type-decl [t-name t-type] t-type]
    [else (report-decl-extractor-error 'type d)]))

(define lookup-qualified-var-in-tenv
  (lambda (m-name var-name tenv)
    (let ([ifc (lookup-module-name-in-tenv tenv m-name)])
      (cases iface ifc
        [simple-iface [decls]
          (lookup-variable-name-in-decls decls var-name)]
        [proc-iface [param-name param-iface result-iface]
          (report-invalid-iface ifc m-name var-name)]))))

(define lookup-module-name-in-tenv
  (lambda (e n)
    (cases env e
      [empty-env []
        (report-invalid-module-name e n)]
      [extend-env [vars vals saved-env]
        (lookup-module-name-in-tenv saved-env n)]
      [extend-env-rec [names varss procs saved-env]
        (lookup-module-name-in-tenv saved-env n)]
      [extend-tenv-with-module [name m-iface saved-env]
        (if (eqv? name n)
          m-iface
          (lookup-module-name-in-tenv saved-env n))]
      [extend-tenv-with-type [t-name t-type saved-tenv]
        (lookup-module-name-in-tenv saved-tenv n)]
      [else
        (report-invalid-env e)])))

(define lookup-variable-name-in-decls
  (lambda (decls var-name)
    (cond [(null? decls) (report-invalid-decls)]
          [(eqv? (decl->name (car decls)) var-name) (decl->type (car decls))]
          [else (lookup-variable-name-in-decls (cdr decls) var-name)])))

(define expand-type
  (lambda (ty tenv)
    (cases type ty
      [any-type [] (any-type)]
      [int-type [] (int-type)]
      [bool-type [] (bool-type)]
      [proc-type [args-type ret-type]
        (proc-type
          (map (lambda (at) (expand-type at tenv)) args-type)
          (expand-type ret-type tenv))]
      [named-type [name]
        (lookup-type-name-in-tenv tenv name)]
      [qualified-type [m-name t-name]
        (lookup-qualified-type-in-tenv tenv m-name t-name)])))

(define lookup-type-name-in-tenv
  (lambda (tenv name)
    (cases env tenv
      [empty-env []
        (report-type-name-not-found name)]
      [extend-tenv-with-type [t-name t-type saved-tenv]
        (if (eqv? t-name name)
          t-type
          (lookup-type-name-in-tenv saved-tenv name))]
      [extend-env [vars vals saved-env]
        (lookup-type-name-in-tenv saved-env name)]
      [else
        (report-invalid-tenv tenv)])))

(define lookup-qualified-type-in-tenv
  (lambda (tenv m-name t-name)
    (let ([m-iface (lookup-module-name-in-tenv tenv m-name)])
      (cases iface m-iface
        [simple-iface [decls]
          (let loop ([decls decls])
            (if (null? decls)
              (report-not-found-type t-name)
              (cases decl (car decls)
                [val-decl [var-name var-type]
                  (if (eqv? var-name t-name)
                    var-type
                    (loop (cdr decls)))]
                [opaque-type-decl [name]
                  (if (eqv? name t-name)
                    (named-type name)
                    (loop (cdr decls)))]
                [transparent-type-decl [name type]
                  (if (eqv? name t-name)
                    type
                    (loop (cdr decls)))])))]
        [proc-iface [param-name param-iface result-iface]
          (report-invalid-iface)]))))

(define val-decl?
  (lambda (d)
    (cases decl d
      [val-decl [var-name var-type] #t]
      [else #f])))
(define transparent-type-decl?
  (lambda (d)
    (cases decl d
      [transparent-type-decl [t-name t-type] #t]
      [else #f])))
(define opaque-type-decl?
  (lambda (d)
    (cases decl d
      [opaque-type-decl [t-name] #t]
      [else #f])))

(define g-seed 0)
(define fresh-module-name
  (lambda (name)
    (let ([seed (begin (set! g-seed (+ g-seed 1)) g-seed)])
      (string->symbol (string-append (symbol->string name) (number->string seed))))))
