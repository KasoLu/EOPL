(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")

(define (run str)
  (type-of-program
    (scan&parse str)))

(define type-of-program
  (lambda (pgm)
    (cases program pgm
      [a-program [class-decls expr]
        (init-static-class-env! class-decls)
        (for-each check-class-decl! class-decls)
        (type-of-expr expr (init-tenv))])))

(define type-of-expr
  (lambda (expr tenv)
    (cases expression expr
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
          (check-equal-type! ty2 ty3 expr)
          ty2)]
      [let-expr [vars exps body]
        (let ([exps-type (map (lambda (x) (type-of-expr x tenv)) exps)])
          (type-of-expr body (extend-tenv vars exps-type tenv)))]
      [proc-expr [vars vars-type result-type body]
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
      [assign-expr [var exp1]
        (let ([var-type (apply-tenv tenv var)] 
              [exp1-type (type-of-expr exp1 tenv)])
          (begin (check-equal-type! exp1-type var-type exp1)
                 (void-type)))]
      [begin-expr [exp1 exps]
        (let ([exp1-type (type-of-expr exp1 tenv)])
          (foldl (lambda (e t) (type-of-expr e tenv)) exp1-type exps))]
      [plus-expr [exp1 exp2]
        (let ([exp1-type (type-of-expr exp1 tenv)]
              [exp2-type (type-of-expr exp2 tenv)])
          (begin (check-equal-type! exp1-type (int-type) exp1)
                 (check-equal-type! exp2-type (int-type) exp2)
                 (int-type)))]
      [list-expr [exp1 exps]
        (let ([exp1-type (type-of-expr exp1 tenv)]
              [exps-type (map (lambda (e) (type-of-expr e tenv)) exps)])
          (begin (for-each (lambda (e t) (check-equal-type! t exp1-type e)) exps exps-type)
                 (list-type exp1-type)))]
      [print-expr [exp1]
        (let ([exp1-type (type-of-expr exp1 tenv)])
          (begin (check-equal-type! exp1-type (int-type) exp1)
                 (int-type)))]
      [new-object-expr [class-name rands]
        (let ([args-types (map (lambda (e) (type-of-expr e tenv)) rands)]
              [class (lookup-static-class class-name)])
          (cases static-class class
            [an-interface [method-tenv]
              (report-cant-instantiate-interface class-name)]
            [a-static-class [s-name i-names f-names f-types m-tenv]
              (let ([method-type (find-method-type class-name 'init)])
                (begin (type-of-call 'init method-type args-types rands expr #t)
                       (class-type class-name)))]))]
      [method-call-expr [obj-expr method-name rands]
        (let* ([args-types (map (lambda (e) (type-of-expr e tenv)) rands)]
               [obj-type (type-of-expr obj-expr tenv)]
               [method-type (find-method-type (type->class-name obj-type) method-name)])
          (type-of-call method-name method-type args-types rands expr #f))]
      [super-call-expr [method-name rands]
        (let* ([args-types (map (lambda (e) (type-of-expr e tenv)) rands)]
               [obj-type (apply-tenv '%self)]
               [super-type (apply-tenv tenv '%super)]
               [method-type (find-method-type super-type method-name)])
          (type-of-call method-name method-type args-types rands expr #f))]
      [self-expr []
        (apply-tenv tenv '%self)]
      [instanceof-expr [obj-expr class-name]
        (let ([obj-type (type-of-expr obj-expr tenv)])
          (if (class-type? obj-type)
            (bool-type)
            (report-bad-type-to-instanceof obj-type obj-expr)))]
      [cast-expr [obj-expr class-name]
        (let ([obj-type (type-of-expr obj-expr tenv)])
          (if (class-type? obj-type)
            (class-type class-name)
            (report-bad-type-to-cast obj-type obj-expr)))]
      )))

(define type-of-call
  (lambda (rator-name rator-type rands-types rands expr is-obj-create)
    (if (and (eqv? rator-name 'init) (eqv? is-obj-create #f))
      (report-init-method-only-be-called-in-object-create)
      (cases type rator-type
        [proc-type [args-types result-type]
          (if (not (= (length args-types) (length rands-types)))
            (report-wrong-number-of-arguments
              (map type-to-external-form args-types)
              (map type-to-external-form rands-types)
              expr)
            (void))
          (begin (for-each check-is-subtype! rands-types args-types rands) result-type)]
        [else
          (report-rator-not-a-proc-type
            (type-to-external-form rator-type) expr)]))))

