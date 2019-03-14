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
      [tvar-type [sn]
        (string->symbol (string-append "ty" (number->string sn)))]
      )))

(define apply-one-subst ; Type x Tvar x Type -> Type
  (lambda (ty0 tvar ty1)
    (cases type ty0
      [any-type  [] (any-type)]
      [int-type  [] (int-type)]
      [bool-type [] (bool-type)]
      [proc-type [args-type ret-type]
        (proc-type
          (map (lambda (t) (apply-one-subst t tvar ty1)) args-type)
          (apply-one-subst ret-type tvar ty1))]
      [tvar-type [sn]
        (if (equal? ty0 tvar) ty1 ty0)]
      )))

(define apply-subst-to-type ; Type x Subst -> Type
  (lambda (ty subst)
    (cases type ty
      [any-type  [] (any-type)]
      [int-type  [] (int-type)]
      [bool-type [] (bool-type)]
      [proc-type [args-type ret-type]
        (proc-type
          (map (lambda (t) (apply-subst-to-type t subst)) args-type)
          (apply-subst-to-type ret-type subst))]
      [tvar-type [sn]
        (let ([tmp (assoc ty subst)])
          (if tmp (cdr tmp) ty))]
      )))

(define empty-subst ; () -> Subst
  (lambda () '()))

(define extend-subst ; Subst x Tvar x Type -> Subst
  (lambda (subst tvar ty)
    (cons (cons tvar ty)
          (map (lambda (p) (cons (car p) (apply-one-subst (cdr p) tvar ty))) subst))))

(define empty-eqs ; () -> Eqs
  (lambda () '()))

(define extend-eqs ; Eqs x Tvar x Type -> Eqs
  (lambda (eqs tvar ty exp1)
    (cons (an-equation tvar ty exp1) eqs)))

(define unify ; Eqs -> Subst
  (lambda (eqs)
    (let loop ([eqs eqs] [subst (empty-subst)])
      (if (null? eqs)
        subst
        (cases equation (car eqs)
          [an-equation [tvar type expr]
            (loop (cdr eqs) (unifier tvar type subst expr))])))))

(define unifier ; Type x Type x Subst x Expr -> Subst
  (lambda (ty1 ty2 subst expr)
    (let ([ty1 (apply-subst-to-type ty1 subst)] [ty2 (apply-subst-to-type ty2 subst)])
      (cond [(equal? ty1 ty2) subst]
            [(tvar-type? ty1)
             (if (no-occurrence? ty1 ty2)
               (extend-subst subst ty1 ty2)
               (report-no-occurrence-violation ty1 ty2 expr))]
            [(tvar-type? ty2)
             (if (no-occurrence? ty2 ty1)
               (extend-subst subst ty2 ty1)
               (report-no-occurrence-violation ty2 ty1 expr))]
            [(and (proc-type? ty1) (proc-type? ty2))
             (let ([ty1-args-type (proc-type->args-type ty1)]
                   [ty1-ret-type (proc-type->ret-type ty1)]
                   [ty2-args-type (proc-type->args-type ty2)]
                   [ty2-ret-type (proc-type->ret-type ty2)])
               (let ([subst (foldl (lambda (t1 t2 res) (unifier t1 t2 res expr)) subst 
                                   ty1-args-type ty2-args-type)])
                 (unifier ty1-ret-type ty2-ret-type subst expr)))]
            [else
              (report-unification-failure ty1 ty2 expr)]))))

(define no-occurrence? ; Tvar x Type -> Bool
  (lambda (tvar ty)
    (cases type ty
      [any-type  [] #t]
      [int-type  [] #t]
      [bool-type [] #t]
      [proc-type [args-type ret-type]
        (and (every? (lambda (t) (no-occurrence? tvar t)) args-type)
             (no-occurrence? tvar res-type))]
      [tvar-type [sn]
        (not (equal? tvar ty))]
      )))

(define otype->type ; OptionType -> Type
  (lambda (otype)
    (cases opty otype
      [no-type [] (fresh-tvar-type)]
      [an-type [ty] ty])))

(define fresh-tvar-type ; () -> Type
  (let ([sn 0])
    (lambda ()
      (set! sn (+ sn 1))
      (tvar-type sn))))

(define run ; String -> Type
  (lambda (str)
    (type-to-external-form
      (type-of-prgm
        (scan&parse str)))))

(define type-of-prgm ; Prgm -> Type
  (lambda (p)
    (cases prgm p
      [a-prgm [expr]
        (cases answer (type-of-expr expr (init-tenv) (empty-eqs))
          [an-answer [ty eqs]
            (apply-subst-to-type ty (unify eqs))])])))

(define type-of-expr ; Expr x Tenv x Eqs -> Answer
  (lambda (e tenv eqs)
    (cases expr e
      [num-expr [num] 
        (an-answer (int-type) eqs)]
      [zero?-expr [exp1]
        (cases answer (type-of-expr exp1 tenv eqs)
          [an-answer [ty1 eqs]
            (an-answer (bool-type) (extend-eqs eqs ty1 (int-type) exp1))])]
      [diff-expr [exp1 exp2]
        (cases answer (type-of-expr exp1 tenv eqs)
          [an-answer [ty1 eqs]
            (let ([eqs (extend-eqs eqs ty1 (int-type) exp1)])
              (cases answer (type-of-expr exp2 tenv eqs)
                [an-answer [ty2 eqs]
                  (let ([eqs (extend-eqs eqs ty2 (int-type) exp2)])
                    (an-answer (int-type) eqs))]))])]
      [if-expr [exp1 exp2 exp3]
        (cases answer (type-of-expr exp1 tenv eqs)
          [an-answer [ty1 eqs]
            (let ([eqs (extend-eqs eqs ty1 (bool-type) exp1)])
              (cases answer (type-of-expr exp2 tenv eqs)
                [an-answer [ty2 eqs]
                  (cases answer (type-of-expr exp3 tenv eqs)
                    [an-answer [ty3 eqs]
                      (let ([eqs (extend-eqs eqs ty2 ty3 e)])
                        (an-answer ty2 eqs))])]))])]
      [var-expr [var]
        (an-answer (apply-tenv tenv var) eqs)]
      [let-expr [vars exps body]
        (let loop ([exps exps] [types '()] [eqs eqs])
          (if (null? exps)
            (type-of-expr body (extend-tenv vars (reverse types) tenv) eqs)
            (cases answer (type-of-expr (car exps) tenv eqs)
              [an-answer [ty1 eqs]
                (loop (cdr exps) (cons ty1 types) eqs)])))]
      [proc-expr [vars vars-opty body]
        (let ([vars-type (map otype->type vars-opty)])
          (cases answer (type-of-expr body (extend-tenv vars vars-type tenv) eqs)
            [an-answer [body-type eqs]
              (an-answer (proc-type vars-type body-type) eqs)]))]
      [call-expr [rator rands]
        (let ([ret-type (fresh-tvar-type)])
          (cases answer (type-of-expr rator tenv eqs)
            [an-answer [rator-type eqs]
              (let loop ([rands rands] [types '()] [eqs eqs])
                (if (null? rands)
                  (let* ([pt (proc-type (reverse types) ret-type)]
                         [eqs (extend-eqs eqs rator-type pt rator)])
                    (an-answer ret-type eqs))
                  (cases answer (type-of-expr (car rands) tenv eqs)
                    [an-answer [rand-type eqs]
                      (loop (cdr rands) (cons rand-type types) eqs)])))]))]
      [letrec-expr [names varss varss-opty prets-opty pbs rbody]
        (let* ([prets-type (map otype->type prets-opty)]
               [varss-type (map (lambda (vs) (map otype->type vs)) varss-opty)]
               [procs-type (map (lambda (vst pt) (proc-type vst pt)) varss-type prets-type)]
               [rbody-tenv (extend-tenv names procs-type tenv)])
          (let loop ([pbs pbs] [varss varss] [varss-type varss-type] 
                     [prets-type prets-type] [eqs eqs])
            (if (null? pbs)
              (type-of-expr rbody rbody-tenv eqs)
              (let ([ext-tenv (extend-tenv (car varss) (car varss-type) rbody-tenv)])
                (cases answer (type-of-expr (car pbs) ext-tenv eqs)
                  [an-answer [pb-type eqs]
                    (let ([eqs (extend-eqs eqs pb-type (car prets-type) (car pbs))])
                      (loop (cdr pbs) (cdr varss) (cdr varss-type) 
                            (cdr prets-type) eqs))])))))]
      )))
