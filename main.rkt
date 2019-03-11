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
        (cases answer (type-of-expr expr (init-tenv) (empty-subst))
          [an-answer [ty subst]
            (apply-subst-to-type ty subst)])])))

(define type-of-expr ; Expr x Tenv x Subst -> Answer
  (lambda (e tenv subst)
    (cases expr e
      [num-expr [num] 
        (an-answer (int-type) subst)]
      [zero?-expr [exp1]
        (cases answer (type-of-expr exp1 tenv subst)
          [an-answer [ty1 subst1]
            (let ([subst1 (unifier ty1 (int-type) subst1 exp1)])
              (an-answer (bool-type) subst1))])]
      [diff-expr [exp1 exp2]
        (cases answer (type-of-expr exp1 tenv subst)
          [an-answer [ty1 subst1]
            (let ([subst1 (unifier ty1 (int-type) subst1 exp1)])
              (cases answer (type-of-expr exp2 tenv subst1)
                [an-answer [ty2 subst2]
                  (let ([subst2 (unifier ty2 (int-type) subst2 exp2)])
                    (an-answer (int-type) subst2))]))])]
      [if-expr [exp1 exp2 exp3]
        (cases answer (type-of-expr exp1 tenv subst)
          [an-answer [ty1 subst1]
            (let ([subst1 (unifier ty1 (bool-type) subst1 exp1)])
              (cases answer (type-of-expr exp2 tenv subst1)
                [an-answer [ty2 subst2]
                  (cases answer (type-of-expr exp3 tenv subst2)
                    [an-answer [ty3 subst3]
                      (let ([subst3 (unifier ty2 ty3 subst3 e)])
                        (an-answer ty2 subst3))])]))])]
      [var-expr [var]
        (an-answer (apply-tenv tenv var) subst)]
      [let-expr [vars exps body]
        (let loop ([exps exps] [types '()] [subst subst])
          (if (null? exps)
            (type-of-expr body (extend-tenv vars (reverse types) tenv) subst)
            (cases answer (type-of-expr (car exps) tenv subst)
              [an-answer [ty1 subst1]
                (loop (cdr exps) (cons ty1 types) subst1)])))]
      [proc-expr [vars vars-opty body]
        (let ([vars-type (map otype->type vars-opty)])
          (cases answer (type-of-expr body (extend-tenv vars vars-type tenv) subst)
            [an-answer [body-type body-subst]
              (an-answer (proc-type vars-type body-type) body-subst)]))]
      [call-expr [rator rands]
        (let ([res-type (fresh-tvar-type)])
          (cases answer (type-of-expr rator tenv subst)
            [an-answer [rator-type rator-subst]
              (let loop ([rands rands] [types '()] [subst subst])
                (if (null? rands)
                  (let ([subst (unifier rator-type (proc-type (reverse types) res-type) subst e)])
                    (an-answer res-type subst))
                  (cases answer (type-of-expr (car rands) tenv subst)
                    [an-answer [rand-type rand-subst]
                      (loop (cdr rands) (cons rand-type types) rand-subst)])))]))]
      [letrec-expr [names varss varss-opty prets-opty pbodies rbody]
        (let ([prets-type (map otype->type prets-opty)]
              [varss-type (map (lambda (vs) (map otype->type vs)) varss-opty)])
          (let ([procs-type (map (lambda (vst pt) (proc-type vst pt)) varss-type prets-type)])
            (let ([rbody-tenv (extend-tenv names procs-type tenv)])
              (let loop ([pbodies pbodies] [varss varss] [varss-type varss-type] 
                         [prets-type prets-type] [subst subst])
                (if (null? pbodies)
                  (type-of-expr rbody rbody-tenv subst)
                  (cases answer (type-of-expr (car pbodies) (extend-tenv (car varss) (car varss-type) rbody-tenv) subst)
                    [an-answer [pbody-type pbody-subst]
                      (let ([pbody-subst (unifier pbody-type (car prets-type) pbody-subst e)])
                        (loop (cdr pbodies) (cdr varss) (cdr varss-type) 
                              (cdr prets-type) pbody-subst))]))))))]
      )))
