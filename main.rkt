(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")

(define check-equal-type! ; Type x Type x Expr -> Void
  (lambda (ty1 ty2 expr)
    (if (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 expr)
      (void))))

(define report-unequal-types ; Type x Type x Expr -> Void
  (lambda (ty1 ty2 expr)
    (eopl:error 'check-equal-type! "Types didn't match: ~s != ~a in ~%~a"
                (type-to-external-form ty1)
                (type-to-external-form ty2)
                expr)))

(define type-to-external-form ; Type -> List
  (lambda (ty)
    (cases type ty
      [any-type  [] 'T]
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
      [named-type [name] name]
      [qualified-type [m-name t-name]
        (list 'from m-name 'take t-name)]
      )))

;run : String -> Type
(define (run str)
  (type-to-external-form
    (type-of-prgm
      (scan&parse str))))

;type-of-prgm : Prgm -> Type
(define type-of-prgm
  (lambda (p)
    (cases prgm p
      [a-prgm [mod-defs body]
        (type-of-expr body
          (add-mod-defs-to-tenv mod-defs (init-tenv)))])))

;type-of-expr : Expr x Tenv -> Type
(define (type-of-expr e tenv)
  (cases expr e 
    [num-expr [num]
      (int-type)]
    [var-expr [var]
      (apply-tenv tenv var)]
    [diff-expr [exp1 exp2]
      (let ([ty1 (type-of-expr exp1 tenv)] [ty2 (type-of-expr exp2 tenv)])
        (check-equal-type! ty1 (int-type) exp1)
        (check-equal-type! ty2 (int-type) exp2)
        (int-type))]
    [zero?-expr [exp1]
      (let ([ty1 (type-of-expr exp1 tenv)])
        (check-equal-type! ty1 (int-type) exp1)
        (bool-type))]
    [if-expr [exp1 exp2 exp3]
      (let ([ty1 (type-of-expr exp1 tenv)]
            [ty2 (type-of-expr exp2 tenv)]
            [ty3 (type-of-expr exp3 tenv)])
        (check-equal-type! ty1 (bool-type) exp1)
        (check-equal-type! ty2 ty3 e)
        ty2)]
    [let-expr [vars exps body]
      (let ([exps-type (map (lambda (x) (type-of-expr x tenv)) exps)])
        (type-of-expr body (extend-tenv vars (expand-types exps-type tenv) tenv)))]
    [proc-expr [vars vars-type body]
      (let ([ret-type (type-of-expr body (extend-tenv vars (expand-types vars-type tenv) tenv))])
        (proc-type vars-type ret-type))]
    [call-expr [rator rands]
      (let ([rator-type (type-of-expr rator tenv)]
            [rands-type (map (lambda (x) (type-of-expr x tenv)) rands)])
        (cases type rator-type
          [proc-type [args-type ret-type]
            (let loop ([args-type args-type] [rands-type rands-type] [rands rands])
              (if (null? args-type)
                (void)
                (begin (check-equal-type! (car args-type) (car rands-type) (car rands))
                       (loop (cdr args-type) (cdr rands-type) (cdr rands))))
              ret-type)]
          [else
            (report-rator-not-a-proc-type rator-type rator)]))]
    [letrec-expr [names varss varss-type pbody-type procs rbody]
      (let ([proc-types (map (lambda (vst pbt) (proc-type vst pbt)) varss-type pbody-type)])
        (let ([rbody-tenv (extend-tenv names (expand-types proc-types tenv) tenv)])
          (let ([procs-type 
                  (map (lambda (vs vst p) 
                         (type-of-expr p (extend-tenv vs (expand-types vst tenv) rbody-tenv)))
                       varss varss-type procs)])
            (map (lambda (pt opt p) (check-equal-type! pt opt p)) 
                 procs-type pbody-type procs)
            (type-of-expr rbody rbody-tenv))))]
    [qualified-var-expr [m-name var-name]
      (lookup-qualified-var-in-tenv m-name var-name tenv)]
    ))

(define add-mod-defs-to-tenv
  (lambda (mod-defs tenv)
    (if (null? mod-defs)
      tenv
      (cases mod-def (car mod-defs)
        [a-mod-def [m-name expected-iface m-body]
          (let ([actual-iface (interface-of m-body tenv)])
            (if (<:-iface actual-iface expected-iface tenv)
              (let ([new-tenv 
                      (extend-tenv-with-module 
                         m-name (expand-iface m-name expected-iface tenv) tenv)])
                (add-mod-defs-to-tenv (cdr mod-defs) new-tenv))
              (report-module-doesnt-satisfy-iface m-name expected-iface actual-iface)))]))))

(define interface-of
  (lambda (m-body tenv)
    (cases mod-body m-body
      [defs-mod-body [defs]
        (simple-iface (defs-to-decls defs tenv))])))

(define defs-to-decls
  (lambda (defs tenv)
    (if (null? defs)
      (list)
      (cases def (car defs)
        [val-def [var-name expr]
          (let ([type (type-of-expr expr tenv)])
            (cons (val-decl var-name type)
                  (defs-to-decls 
                    (cdr defs) 
                    (extend-tenv (list var-name) (list type) tenv))))]
        [type-def [t-name t-type]
          (let ([new-tenv (extend-tenv-with-type t-name (expand-type t-type tenv) tenv)])
            (cons (transparent-type-decl t-name t-type)
                  (defs-to-decls (cdr defs) new-tenv)))]))))

(define <:-iface
  (lambda (iface1 iface2 tenv)
    (cases iface iface1
      [simple-iface [decls1]
        (cases iface iface2
          [simple-iface [decls2]
            (<:-decls decls1 decls2 tenv)])])))

(define <:-decls
  (lambda (decls1 decls2 tenv)
    (if (null? decls2)
      #t
      (let ([tenv (check-decls decls1 tenv)])
        (check-decls decls2 tenv)
        (let loop ([decls2 decls2])
          (if (null? decls2)
            #t
            (begin (check-<:-decl decls1 (car decls2) tenv)
                   (loop (cdr decls2)))))))))

(define check-decls
  (lambda (decls tenv)
    (if (null? decls)
      tenv
      (cases decl (car decls)
        [val-decl [var-name var-type]
          (check-type var-type tenv)
          (check-decls (cdr decls) tenv)]
        [opaque-type-decl [t-name]
          (check-decls
            (cdr decls)
            (extend-tenv-with-type t-name (qualified-type 'module-x t-name) tenv))]
        [transparent-type-decl [t-name t-type]
          (check-decls
            (cdr decls)
            (extend-tenv-with-type t-name t-type tenv))]))))

(define check-<:-decl
  (lambda (decls d tenv)
    (let loop ([decls decls])
      (cond [(null? decls) (report-no-check-decl d)]
            [(<:-decl (car decls) d tenv) #t]
            [else (check-<:-decl (cdr decls) d tenv)]))))

(define check-type
  (lambda (t tenv)
    (cases type t
      [named-type [t-name] 
        (lookup-type-name-in-tenv tenv t-name)]
      [proc-type [vars-type ret-type]
        (proc-type (map (lambda (vt) (check-type vt tenv)) vars-type)
                   (check-type ret-type tenv))]
      [else t])))

(define expand-iface
  (lambda (m-name iface1 tenv)
    (cases iface iface1
      [simple-iface [decls]
        (simple-iface (expand-decls m-name decls tenv))])))

(define expand-decls
  (lambda (m-name decls internal-tenv)
    (if (null? decls)
      (list)
      (cases decl (car decls)
        [opaque-type-decl [t-name]
          (let ([expanded-type (qualified-type m-name t-name)])
            (let ([new-tenv (extend-tenv-with-type t-name expanded-type internal-tenv)])
              (cons (transparent-type-decl t-name expanded-type)
                    (expand-decls m-name (cdr decls) new-tenv))))]
        [transparent-type-decl [t-name t-type]
          (let ([expanded-type (expand-type t-type internal-tenv)])
            (let ([new-tenv (extend-tenv-with-type t-name expanded-type internal-tenv)])
              (cons (transparent-type-decl t-name expanded-type)
                    (expand-decls m-name (cdr decls) new-tenv))))]
        [val-decl [var-name var-type]
          (let ([expanded-type (expand-type var-type internal-tenv)])
            (cons (val-decl var-name expanded-type)
                  (expand-decls m-name (cdr decls) internal-tenv)))]))))

(define extend-tenv-with-decl
  (lambda (decl1 tenv)
    (cases decl decl1
      [val-decl [name type] tenv]
      [transparent-type-decl [name type]
        (extend-tenv-with-type name (expand-type type tenv) tenv)]
      [opaque-type-decl [name]
        (extend-tenv-with-type name (qualified-type (fresh-module-name '%unknown) name) tenv)])))

(define <:-decl
  (lambda (decl1 decl2 tenv)
    (or
      (and
        (val-decl? decl1)
        (val-decl? decl2)
        (equiv-type? (decl->type decl1) (decl->type decl2) tenv))
      (and
        (transparent-type-decl? decl1)
        (transparent-type-decl? decl2)
        (equiv-type? (decl->type decl1) (decl->type decl2) tenv))
      (and
        (transparent-type-decl? decl1)
        (opaque-type-decl? decl2))
      (and
        (opaque-type-decl? decl1)
        (opaque-type-decl? decl2)))))

(define equiv-type?
  (lambda (ty1 ty2 tenv)
    (equal? (expand-type ty1 tenv) (expand-type ty2 tenv))))

(define expand-types
  (lambda (types tenv)
    (map (lambda (t) (expand-type t tenv)) types)))

