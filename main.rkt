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
            (cdr type-form)))])))

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
                (begin (check-equal-type! (car args-type) (car rands-type) (car rands))
                       (loop (cdr args-type) (cdr rands-type) (cdr rands))))
              ret-type)]
          [else
            (report-rator-not-a-proc-type rator-type rator)]))]
    [letrec-expr [names varss varss-type pbody-type procs rbody]
      (let ([proc-types (map (lambda (vst pbt) (proc-type vst pbt)) varss-type pbody-type)])
        (let ([rbody-tenv (extend-tenv names proc-types tenv)])
          (let ([procs-type 
                  (map (lambda (vs vst p) (type-of-expr p (extend-tenv vs vst rbody-tenv)))
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
              (let ([new-tenv (extend-tenv-with-module m-name expected-iface tenv)])
                (add-mod-defs-to-tenv (cdr mod-defs) new-tenv))
              (report-module-doesnt-satisfy-iface m-name expected-iface actual-iface)))]))))

(define interface-of
  (lambda (m-body tenv)
    (cases mod-body m-body
      [defs-mod-body [defs]
        (simple-iface (defs-to-decls defs tenv))]
      [let-mod-body [vars exps m-body]
        (let ([types (map (lambda (e) (type-of-expr e tenv)) exps)])
          (interface-of m-body (extend-tenv vars types tenv)))]
      [letrec-mod-body [names varss varss-type prets-type procs m-body]
        (let ([procs-type (map (lambda (vst rt) (proc-type vst rt)) varss-type prets-type)])
          (let ([m-body-tenv (extend-tenv names procs-type tenv)])
            (let ([actual-rets-type 
                    (map (lambda (vs vt p) (type-of-expr p (extend-tenv vs vt m-body-tenv))) 
                         varss varss-type procs)])
              (map (lambda (pt at p) (check-equal-type! pt at p)) 
                   actual-rets-type prets-type procs)
              (interface-of m-body m-body-tenv))))])))

(define defs-to-decls
  (lambda (defs tenv)
    (if (null? defs)
      (list)
      (cases def (car defs)
        [val-def [var-name expr]
          (let ([ty (type-of-expr expr tenv)])
            (cons (val-decl var-name ty)
                  (defs-to-decls (cdr defs) (extend-tenv (list var-name) (list ty) tenv))))]))))

(define <:-iface
  (lambda (iface1 iface2 tenv)
    (cases iface iface1
      [simple-iface [decls1]
        (cases iface iface2
          [simple-iface [decls2]
            (<:-decls decls1 decls2 tenv)])])))

(define <:-decls
  (lambda (decls1 decls2 tenv)
    (cond [(null? decls2) #t]
          [(null? decls1) #f]
          [else
           (let ([name1 (decl->name (car decls1))] [name2 (decl->name (car decls2))])
             (if (eqv? name1 name2)
               (and (equal? (decl->type (car decls1)) (decl->type (car decls2)))
                    (<:-decls (cdr decls1) (cdr decls2) tenv))
               (<:-decls (cdr decls1) decls2 tenv)))])))
