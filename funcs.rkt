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
    [else (report-decl-extractor-error 'name d)]))
(define (decl->type d)
  (cases decl d
    [val-decl [name type] type]
    [else (report-decl-extractor-error 'type d)]))

(define lookup-qualified-var-in-tenv
  (lambda (m-name var-name tenv)
    (let ([ifc (lookup-module-name-in-tenv tenv m-name)])
      (cases iface ifc
        [simple-iface [decls]
          (lookup-variable-name-in-decls decls var-name)]))))

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
      [else
        (report-invalid-env e)])))

(define lookup-variable-name-in-decls
  (lambda (decls var-name)
    (cond [(null? decls) (report-invalid-decls)]
          [(eqv? (decl->name (car decls)) var-name) (decl->type (car decls))]
          [else (lookup-variable-name-in-decls (cdr decls) var-name)])))
