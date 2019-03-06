(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")

;check-equal-type! : Type x Type x Expr -> Void
(define (check-equal-type! ty1 ty2 expr)
  (if (not (equal? ty1 ty2))
    (report-unequal-types ty1 ty2 expr)
    (void)))

;report-unequal-types : Type x Type x Expr -> Void
(define (report-unequal-types ty1 ty2 expr)
  (eopl:error 'check-equal-type! "Types didn't match: ~s != ~a in ~%~a"
              (type-to-external-form ty1)
              (type-to-external-form ty2)
              expr))

;type-to-external-form : Type -> List
(define (type-to-external-form ty)
  (cases type ty
    [any-type  [] 'T]
    [int-type  [] 'Int]
    [bool-type [] 'Bool]
    [void-type [] 'Void]
    [proc-type [args-type ret-type]
      (let ([type-form 
              (foldr (lambda (at acc) (cons '* (cons (type-to-external-form at) acc))) 
                     (list '-> (type-to-external-form ret-type))
                     args-type)])
        (if (eqv? (car type-form) '->)
          (cons '() type-form)
          (cdr type-form)))]
    [pair-type [left-type right-type]
      (list (type-to-external-form left-type) '* (type-to-external-form right-type))]
    ))

;run : String -> Type
(define (run str)
  (type-to-external-form
    (type-of-prgm
      (scan&parse str))))

;type-of-prgm : Prgm -> Type
(define (type-of-prgm p)
  (cases prgm p
    [a-prgm [expr]
      (type-of-expr expr (init-tenv))]))

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
    [pair-expr [exp1 exp2]
      (let ([exp1-type (type-of-expr exp1 tenv)] [exp2-type (type-of-expr exp2 tenv)])
        (pair-type exp1-type exp2-type))]
    [left-expr [exp1]
      (let ([exp1-type (type-of-expr exp1 tenv)])
        (cases type exp1-type
          [pair-type [left-type right-type] left-type]
          [else (report-unequal-types exp1-type (pair-type (any-type) (any-type)) exp1)]))]
    [right-expr [exp1]
      (let ([exp1-type (type-of-expr exp1 tenv)])
        (cases type exp1-type
          [pair-type [left-type right-type] right-type]
          [else (report-unequal-types exp1-type (pair-type (any-type) (any-type)) exp1)]))]
;    [expr ("setright" "(" expr "," expr ")") setright-expr]
    [setleft-expr [exp1 exp2]
      (let ([exp1-type (type-of-expr exp1 tenv)] [exp2-type (type-of-expr exp2 tenv)])
        (cases type exp1-type
          [pair-type [left-type right-type]
            (check-equal-type! exp2-type left-type exp2)
            (void-type)]
          [else
            (report-unequal-types exp1-type (pair-type (any-type) (any-type)) exp1)]))]
    [setright-expr [exp1 exp2]
      (let ([exp1-type (type-of-expr exp1 tenv)] [exp2-type (type-of-expr exp2 tenv)])
        (cases type exp1-type
          [pair-type [left-type right-type]
            (check-equal-type! exp2-type right-type exp2)
            (void-type)]
          [else
            (report-unequal-types exp1-type (pair-type (any-type) (any-type)) exp1)]))]
    [begin-expr [exp1 exps]
      (foldl (lambda (e _) (type-of-expr e tenv)) (type-of-expr exp1 tenv) exps)]
    ))
