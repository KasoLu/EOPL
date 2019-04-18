(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")

(define check-equal-type! ; Type x Type x Expr -> Void
  (lambda (ty1 ty2 expr tenv)
    (if (not (equiv-type? ty1 ty2 tenv))
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
        (check-equal-type! ty1 (int-type) exp1 tenv)
        (check-equal-type! ty2 (int-type) exp2 tenv)
        (int-type))]
    [zero?-expr [exp1]
      (let ([ty1 (type-of-expr exp1 tenv)])
        (check-equal-type! ty1 (int-type) exp1 tenv)
        (bool-type))]
    [if-expr [exp1 exp2 exp3]
      (let ([ty1 (type-of-expr exp1 tenv)]
            [ty2 (type-of-expr exp2 tenv)]
            [ty3 (type-of-expr exp3 tenv)])
        (check-equal-type! ty1 (bool-type) exp1 tenv)
        (check-equal-type! ty2 ty3 e tenv)
        ty2)]
    [let-expr [vars exps body]
      (let ([exps-type (map (lambda (x) (type-of-expr x tenv)) exps)])
        (type-of-expr body (extend-tenv vars exps-type tenv)))]
    [proc-expr [vars vars-type body]
      (let ([ret-type (type-of-expr body (extend-tenv vars vars-type tenv))])
        (proc-type vars-type ret-type))]
    [call-expr [rator rands]
      (let ([rator-type (type-of-expr rator tenv)]
            [rands-type (map (lambda (x) (type-of-expr x tenv)) rands)])
        (cases type rator-type
          [proc-type [args-type ret-type]
            (let loop ([args-type args-type] [rands-type rands-type] [rands rands])
              (if (null? args-type)
                (void)
                (begin (check-equal-type! (car args-type) (car rands-type) (car rands) tenv)
                       (loop (cdr args-type) (cdr rands-type) (cdr rands))))
              ret-type)]
          [else
            (report-rator-not-a-proc-type rator-type rator)]))]
    [letrec-expr [names varss varss-type pbody-type procs rbody]
      (let ([proc-types (map (lambda (vst pbt) (proc-type vst pbt)) varss-type pbody-type)])
        (let ([rbody-tenv (extend-tenv names proc-types tenv)])
          (let ([procs-type 
                  (map (lambda (vs vst p) 
                         (type-of-expr p (extend-tenv vs vst rbody-tenv)))
                       varss varss-type procs)])
            (map (lambda (pt opt p) (check-equal-type! pt opt p tenv)) 
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
      [var-mod-body [m-name]
        (lookup-module-name-in-tenv tenv m-name)]
      [defs-mod-body [defs]
        (simple-iface (defs-to-decls defs tenv))]
      [app-mod-body [rator-id rand-id]
        (let ([rator-iface (lookup-module-name-in-tenv tenv rator-id)]
              [rand-iface (lookup-module-name-in-tenv tenv rand-id)])
          (cases iface rator-iface
            [simple-iface [decls]
              (report-attemp-to-apply-simple-module rator-id)]
            [proc-iface [param-name param-iface result-iface]
              (if (<:-iface rand-iface param-iface tenv)
                (rename-in-iface result-iface param-name rand-id)
                (report-bad-module-application-error param-iface rand-iface m-body))]))]
      [proc-mod-body [rand-name rand-iface m-body]
        (let ([body-iface
                (interface-of m-body
                  (extend-tenv-with-module
                    rand-name
                    (expand-iface rand-name rand-iface tenv)
                    tenv))])
          (proc-iface rand-name rand-iface body-iface))])))

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
            (<:-decls decls1 decls2 tenv)]
          [proc-iface [param-name2 param-iface2 result-iface2]
            #f])]
      [proc-iface [param-name1 param-iface1 result-iface1]
        (cases iface iface2
          [simple-iface [decls2] #f]
          [proc-iface [param-name2 param-iface2 result-iface2]
            (let ([new-name (fresh-module-name param-name1)])
              (let ([result-iface1 (rename-in-iface result-iface1 param-name1 new-name)]
                    [result-iface2 (rename-in-iface result-iface2 param-name2 new-name)])
                (and
                  (<:-iface param-iface2 param-iface1 tenv)
                  (<:-iface result-iface1 result-iface2
                    (extend-tenv-with-module
                      new-name
                      (expand-iface new-name param-iface1 tenv)
                      tenv)))))])])))

(define <:-decls
  (lambda (decls1 decls2 tenv)
    (cond [(null? decls2) #t]
          [(null? decls1) #f]
          [else
           (let ([name1 (decl->name (car decls1))] [name2 (decl->name (car decls2))])
             (if (eqv? name1 name2)
               (and
                 (<:-decl (car decls1) (car decls2) tenv)
                 (<:-decls (cdr decls1) (cdr decls2) (extend-tenv-with-decl (car decls1) tenv)))
               (<:-decls (cdr decls1) decls2 (extend-tenv-with-decl (car decls1) tenv))))])))

(define expand-iface
  (lambda (m-name iface1 tenv)
    (cases iface iface1
      [simple-iface [decls]
        (simple-iface (expand-decls m-name decls tenv))]
      [proc-iface [param-name param-iface result-iface]
        iface1])))

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

