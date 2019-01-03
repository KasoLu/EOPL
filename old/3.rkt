; 3.6
(define-datatype expression expression?
  ; ...
  [minus-exp
    [exp expression?]])
(define (value-of exp env)
  (cases expression exp
    ; ...
    [minus-exp [exp]
      (let ([val (value-of exp env)])
        (let ([num (expval->num val)])
          (num->val (- num))))]))

; 3.7
(define-datatype expression expression?
  ; ...
  [arith-exp
    [op symbol?]
    [exp1 expression?]
    [exp2 expression?]])
(define (value-of exp env)
  (cases expression exp
    ; ...
    [arith-exp [op exp1 exp2]
      (let ([val1 (value-of exp1 env)]
            [val2 (value-of exp2 env)])
        (let ([num1 (expval->num val1)]
              [num2 (expval->num val2)])
          (cond [(eqv? op 'add) (num-val (+ num1 num2))]
                [(eqv? op 'mul) (num-val (* num1 num2))]
                [(eqv? op 'div) (num-val (quotient num1 num2))]
                [else (report-invalid-opertion 'arith-exp op)])))]))

; 3.8
(define-datatype expression expression?
  ; ...
  [logic-exp
    [op symbol?]
    [exp1 expression?]
    [exp2 expression?]])
(define (value-of exp env)
  (cases expression exp
    ; ...
    [logic-exp [op exp1 exp2]
      (let ([val1 (value-of exp1 env)]
            [val2 (value-of exp2 env)])
        (let ([num1 (expval->num val1)]
              [num2 (expval->num val2)])
          (cond [(eqv? op 'equal?) (bool-val (= num1 num2))]
                [(eqv? op 'greater?) (bool-val (> num1 num2))]
                [(eqv? op 'less?) (bool-val (< num1 num2))]
                [else (report-invalid-opertion 'logic-exp op)])))]))

; 3.9
(define-datatype expval expval?
  ; ...
  [list-val
    [lst (lambda (x) (or (null? x) (pair? x)))]])
(define (expval->list val)
  (cases expval val
    [list-val [lst] lst]
    [else (report-expvalextractor-error 'list val)]))
(define-datatype expression expression?
  ; ...
  [emptylist-exp]
  [cons-exp
    [exp1 expression?]
    [exp2 expression?]]
  [car-exp
    [exp1 expression?]]
  [cdr-exp
    [exp1 expression?]]
  [null?-exp
    [exp1 expression?]])
(define (value-of exp env)
  (cases expression exp
    ; ...
    [emptylist-exp []
      (list-val '())]
    [cons-exp [exp1 exp2]
      (let ([val1 (value-of exp1 env)]
            [val2 (value-of exp2 env)])
        (list-val (cons val1 val2)))]
    [car-exp [exp1]
      (let ([val1 (value-of exp1 env)])
        (cases expval val1
          [list-val [lst]
            (if (null? lst)
              (report-invalid-list 'car-exp exp1)
              (car lst))]
          [else (report-invalid-list 'car-exp exp1)]))]
    [cdr-exp [exp1]
      (let ([val1 (value-of exp1 env)])
        (cases expval val1
          [list-val [lst]
            (if (null? lst)
              (report-invalid-list 'cdr-exp exp1)
              (cdr lst))]
          [else (report-invalid-list 'cdr-exp exp1)]))]
    [null?-exp [exp1]
      (let ([val1 (value-of exp1 env)])
        (cases expval val1
          [list-val [lst]
            (bool-val (null? lst))]
          [else (report-invalid-list 'null?-exp exp1)]))]))

; 3.10
(define-datatype expression expression?
  ; ...
  [list-exp
    [exps (list-of expression?)]])
(define (value-of exp env)
  (cases expression exp
    ; ...
    [list-exp [exps]
      (let ([vals (map (lambda (x) (value-of x env)) exps)])
        (foldr (lambda (x s) (list-val (cons x s))) (list-val '()) vals))]))

; 3.12
(define-datatype expression expression?
  ; ...
  [cond-exp
    [preds (list-of expression?)]
    [exps (list-of expression?)]])
(define (value-of exp env)
  (cases expression exp
    ; ...
    [cond-exp [preds exps]
      (let ([vals (map (lambda (x) (value-of x env)) preds)])
        (define (loop preds exps)
          (cond [(null? preds) (report-invalid-cond 'cond-exp preds exps)]
                [(expval->bool (car preds)) (value-of (car exps) env)]
                [else (loop (cdr preds) (cdr exps))]))
        (loop vals exps))]))

; 3.13
(define (value-of exp env)
  (cases expression exp
    ; ...
    [if-exp [exp1 exp2 exp3]
      (let ([val1 (value-of exp1 env)])
        (if (zero? (expval->num val1))
          (value-of exp3 env)
          (value-of exp2 env)))]))

; 3.14
(define-datatype boolexp boolexp?
  ; ...
  [op-exp
    [op symbol?]
    [exp1 expression?]
    [exp2 expression?]])
(define (value-of-bool-exp exp env)
  (cases boolexp exp
    ; ...
    [op-exp [op exp1 exp2]
      (let ([val1 (value-of exp1 env)]
            [val2 (value-of exp2 env)])
        (let ([num1 (expval->num val1)]
              [num2 (expval->num val2)])
          (cond [(eqv? op 'greater?) (bool-val (> num1 num2))]
                [(eqv? op 'equal?) (bool-val (= num1 num2))]
                [(eqv? op 'less?) (bool-val (< num1 num2))])))]))

; 3.15
(define (value-of exp env)
  (cases expression exp
    ; ...
    [print-exp [exp1]
      (let ([val1 (value-of exp1 env)])
        (cases expval val1
          [num-val [num]
            (printf "~a\n" num)]
          [bool-val [bool]
            (printf "~a\n" bool)]
          [list-val [lst]
            (printf "~a\n" lst)])
        (num-val 1))]))

; 3.16
(define-datatype boolexp boolexp?
  ; ...
  [let-exp
    [vars (list-of identifier?)]
    [exps (list-of expression?)]
    [body expression?]])
(define (value-of exp env)
  (cases expression exp
    ; ...
    [let-exp [vars exps body]
      (let ([ext-env (foldl (lambda (var exp res) (extend-env var (value-of exp env) res)) env vars exps)])
        (value-of body ext-env))]))

; 3.17
(define-datatype boolexp boolexp?
  ; ...
  [let*-exp
    [vars (list-of identifier?)]
    [exps (list-of expression?)]
    [body expression?]])
(define (value-of exp env)
  (cases expression exp
    ; ...
    [let*-exp [vars exps body]
      (let ([ext-env (foldl (lambda (var exp res) (extend-env var (value-of exp res) res)) env vars exps)])
        (value-of body ext-env))]))

; 3.18
(define-datatype boolexp boolexp?
  ; ...
  [unpack-exp
    [vars (list-of identifier?)]
    [lst expression?]
    [body expression?]])
(define (value-of exp env)
  (cases expression exp
    ; ...
    [unpack-exp [vars lst body]
      (define (unpack-loop vars lst ext-env)
        (let ([ls (expval->list lst)])
          (cond [(and (null? vars) (null? ls)) 
                 ext-env]
                [(and (pair? vars) (pair? ls))
                 (unpack-loop (cdr vars) (cdr ls) (extend-env (car vars) (car ls) ext-env))]
                [else (report-invalid-unpack-exp 'unpack-exp vars lst)])))
      (value-of body (unpack-loop vars (value-of lst env) env))]))

; 3.19
(define-datatype letproc letproc?
  [let-proc
    [name identifier?]
    [var identifier?]
    [body expression?]
    [let-body expression?]])
(define-datatype boolexp boolexp?
  ; ...
  [letproc-exp
    [name identifier?]
    [var identifier?]
    [body expression?]
    [let-body expression?]])
(define (value-of exp env)
  (cases expression exp
    ; ...
    [letproc-exp [name var body let-body]
      (let ([proc (proc-val (procedure var body env))])
        (value-of let-body (extend-env name proc env)))]))
  
; 3.20
(define e 
  (let-exp '(f) (list (proc-exp 'x 
                        (proc-exp 'y 
                          (diff-exp (var-exp 'x) 
                                    (diff-exp (const-exp 0) 
                                              (var-exp 'y))))))
           (call-exp (call-exp (var-exp 'f) 
                               (const-exp 3))
                     (const-exp 4))))

; 3.21
(define-datatype proc proc?
  [procedure
    [vars (list-of identifier?)]
    [body expression?]
    [env env?]])
(define (apply-procedure proc1 vals)
  (cases proc proc1
    [procedure [vars body saved-env]
      (let ([ext-env (foldr (lambda (var val res) (extend-env var val res)) saved-env vars vals)])
        (value-of body ext-env))]))

; 3.22
(define e 
  (let-exp 'makemult-n
    (proc-exp 'n
      (proc-exp 'maker
        (proc-exp 'x
          (if-exp
            (zero?-exp (var-exp 'x))
            (const-exp 0)
            (diff-exp
              (call-exp 
                (call-exp (var-exp 'maker) (var-exp 'maker))
                (diff-exp (var-exp 'x) (const-exp 1)))
              (diff-exp (const-exp 0) (var-exp 'n)))))))
    (let-exp 'factorial
      (proc-exp 'fac-maker
        (proc-exp 'n
          (if-exp
            (zero?-exp (var-exp 'n))
            (const-exp 1)
            (let-exp 'maker-n (call-exp (var-exp 'makemult-n) (var-exp 'n))
              (let-exp 'times-n 
                (proc-exp 'x
                  (call-exp
                    (call-exp (var-exp 'maker-n) (var-exp 'maker-n))
                    (var-exp 'x)))
                (call-exp
                  (var-exp 'times-n)
                  (call-exp
                    (call-exp (var-exp 'fac-maker) (var-exp 'fac-maker))
                    (diff-exp (var-exp 'n) (const-exp 1)))))))))
      (call-exp
        (call-exp (var-exp 'factorial) (var-exp 'factorial))
        (const-exp 5)))))

; 3.24
(define e
  (let-exp 'even
    (proc-exp 'even
      (proc-exp 'odd
        (proc-exp 'x
          (if-exp (zero?-exp (var-exp 'x))
                  (const-exp 1)
                  (call-exp
                    (call-exp (call-exp (var-exp 'odd) (var-exp 'odd))
                              (var-exp 'even))
                    (diff-exp (var-exp 'x) (const-exp 1)))))))
    (let-exp 'odd
      (proc-exp 'odd
        (proc-exp 'even
          (proc-exp 'x
            (if-exp (zero?-exp (var-exp 'x))
                    (const-exp 0)
                    (call-exp
                      (call-exp (call-exp (var-exp 'even) (var-exp 'even))
                                (var-exp 'odd))
                      (diff-exp (var-exp 'x) (const-exp 1)))))))
      (call-exp (call-exp (call-exp (var-exp 'even) (var-exp 'even)) (var-exp 'odd)) 
                (const-exp 1)))))
; 3.25
(define e
  (let-exp 'makerec
    (proc-exp 'f
      (proc-exp 'n
        (let-exp 'd
          (proc-exp 'd
            (proc-exp 'n
              (call-exp (call-exp (var-exp 'f) 
                                  (call-exp (var-exp 'd) (var-exp 'd))) 
                        (var-exp 'n))))
          (call-exp (call-exp (var-exp 'f) 
                              (call-exp (var-exp 'd) (var-exp 'd))) 
                    (var-exp 'n)))))
    (let-exp 'maketimes4
      (proc-exp 'rec-maketimes4
        (proc-exp 'x
          (if-exp (zero?-exp (var-exp 'x))
                  (const-exp 0)
                  (diff-exp (call-exp (var-exp 'rec-maketimes4) 
                                      (diff-exp (var-exp 'x) (const-exp 1))) 
                            (const-exp -4)))))
      (let-exp 'times4
        (call-exp (var-exp 'makerec) (var-exp 'maketimes4))
        (call-exp (var-exp 'times4) (const-exp 3))))))

; 3.26
(define (search-in-env env1 var)
  (cases env env1
    [empty-env [] '()]
    [extend-env [saved-var saved-val saved-env]
      (if (eqv? var saved-var)
        saved-val
        (search-in-env saved-env var))]))
(define (search-free-var exp1 var-env free-vars)
  (printf "exp: ~a\nvar-env: ~a\nfree-vars: ~a\n\n" exp1 var-env free-vars)
  (cases expression exp1
    [const-exp [num] free-vars]
    [var-exp [var]
      (cond [(pair? (memv var free-vars)) free-vars]
            [(null? (search-in-env var-env var)) (cons var free-vars)]
            [else free-vars])]
    [diff-exp [exp1 exp2]
      (let ([exp1-free-vars (search-free-var exp1 var-env free-vars)])
        (search-free-var exp2 var-env exp1-free-vars))]
    [zero?-exp [exp1]
      (search-free-var exp1 var-env free-vars)]
    [if-exp [exp1 exp2 exp3]
      (let ([exp1-free-vars (search-free-var exp1 var-env free-vars)])
        (let ([exp2-free-vars (search-free-var exp2 var-env exp1-free-vars)])
          (search-free-var exp3 var-env exp2-free-vars)))]
    [let-exp [var exp1 body]
      (let ([exp1-free-vars (search-free-var exp1 var-env free-vars)])
        (search-free-var body (extend-env var #f var-env) exp1-free-vars))]
    [proc-exp [var exp1]
      (search-free-var exp1 (extend-env var #f var-env) free-vars)]
    [call-exp [exp1 exp2]
      (let ([exp1-free-vars (search-free-var exp1 var-env free-vars)])
        (search-free-var exp2 var-env exp1-free-vars))]))
(define (value-of exp env)
  (cases expression exp
    ; ...
    [proc-exp [var body]
      (printf "proc-exp - var: ~a, body: ~a\n\n" var body)
      (let ([free-vars (search-free-var body (extend-env var #f (init-env)) '())])
        (let ([bind-env (foldr (lambda (v e) (extend-env v (apply-env env v) e)) 
                               (init-env) 
                               free-vars)])
          (printf "free-vars: ~a\nbind-env: ~a\n\n" free-vars bind-env)
          (proc-val (procedure var body bind-env))))]))

; 3.27
(define-datatype proc proc?
  ; ...
  [traceproc
    [var identifier?]
    [body expression?]
    [saved-env env?]])
(define (apply-procedure proc1 val)
  (cases proc proc1
    ; ...
    [traceproc [var body saved-env]
      (printf "traceproc begin - ~a: ~a\n" var val)
      (value-of body (extend-env var val saved-env))
      (printf "traceproc end\n")]))
(define (value-of exp env)
  (cases expression exp
    ; ...
    [proc-exp [var body]
      (printf "proc-exp - var: ~a, body: ~a\n\n" var body)
      (proc-val (traceproc var body env))]))

; 3.28
(define (value-of exp env)
  (cases expression exp
    ; ...
    [call-exp [rator rand]
      (printf "call-exp - rator: ~a, rand: ~a\n\n" rator rand)
      (let ([proc1 (expval->proc (value-of rator env))])
        (cases proc proc1
          [procedure [var body saved-env]
            (value-of body (extend-env var (value-of rand env) env))]))]))

; 3.31
(define grammar-spec
  '(; ...
    [expression ("(" expression (arbno expression) ")") call-exp]
    [expression ("letrec-m" identifier "(" (arbno identifier) ")" "=" expression 
                 "in" expression) letrec-m-exp]))
(define-datatype env env?
  ; ...
  [extend-env-rec
    [p-name identifier?]
    [b-vars (list-of identifier?)]
    [body expression?]
    [env env?]])
(define (apply-env env1 var)
  (cases env env1
    ; ...
    [extend-env-rec [p-name b-vars p-body saved-env]
      (if (eqv? var p-name)
        (proc-val (mult-proc b-vars p-body env1))
        (apply-env saved-env var))]))
(define-datatype proc proc?
  ; ...
  [mult-proc
    [vars (list-of identifier?)]
    [body expression?]
    [saved-env env?]])
(define (apply-procedure proc1 val)
  (cases proc proc1
    ; ...
    [mult-proc [vars body saved-env]
      (let ([vs (foldr (lambda (var val env) 
                         (extend-env var val env)) saved-env vars val)])
        (value-of body vs))]))
(define-datatype expression expression?
  ; ...
  [call-exp
    [rator expression?]
    [rand (list-of expression?)]]
  [letrec-m-exp
    [p-name identifier?]
    [b-vars (list-of identifier?)]
    [p-body expression?]
    [letrec-body expression?]])
(define (value-of exp env)
  (cases expression exp
    ; ...
    [call-exp [rator rand]
      (let ([proc (expval->proc (value-of rator env))]
            [args (map (lambda (x) (value-of x env)) rand)])
        (apply-procedure proc args))]
    [letrec-m-exp [p-name b-vars p-body letrec-body]
      (value-of letrec-body (extend-env-rec p-name b-vars p-body env))]))

; 3.32
(define grammar-spec
  '(; ...
    [expression ("letrec" (arbno identifier "(" identifier ")" "=" expression)
                 "in" expression) letrec-exp]))
(define-datatype expression expression?
  ; ...
  [letrec-exp
    [p-names (list-of identifier?)]
    [b-vars (list-of identifier?)]
    [p-bodies (list-of expression?)]
    [letrec-body expression?]])
(define (apply-env env1 var)
  (cases env env1
    ; ...
    [extend-env-rec [p-name b-var p-body saved-env]
      (if (eqv? var p-name)
        (proc-val (procedure b-var p-body env1))
        (let ([p-v (expval->proc (apply-env saved-env var))])
          (cases proc p-v
            [procedure [var body saved-env]
              (proc-val (procedure var body env1))])))]))
(define (value-of exp env)
  (cases expression exp
    ; ...
    [letrec-exp [p-names b-vars p-bodies letrec-body]
      (let ([ext-env (foldr (lambda (n v b r) (extend-env-rec n v b r)) env p-names b-vars p-bodies)])
        (value-of letrec-body ext-env))]))

; 3.33
(define grammar-spec
  '(; ...
    [expression ("(" expression (arbno expression) ")") call-exp]
    [expression ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression)
                 "in" expression) letrec-exp]))
(define-datatype env env?
  ; ...
  [extend-env-rec
    [p-names (list-of identifier?)]
    [b-var (list-of (list-of identifier?))]
    [bodies (list-of expression?)]
    [env env?]])
(define-datatype proc proc?
  ; ...
  [procedure
    [vars (list-of identifier?)]
    [body expression?]
    [saved-env env?]])
(define-datatype expression expression?
  ; ...
  [call-exp
    [rator expression?]
    [rands (list-of expression?)]]
  [letrec-exp
    [p-names (list-of identifier?)]
    [b-vars (list-of (list-of identifier?))]
    [p-bodies (list-of expression?)]
    [letrec-body expression?]])
(define (apply-env env1 var)
  (cases env env1
    ; ...
    [extend-env-rec [p-names b-vars p-bodies saved-env]
      (define (found names vars-s bodies)
        (cond [(null? names) (apply-env saved-env var)]
              [(eqv? (car names) var) (proc-val (procedure (car vars-s) (car bodies) env1))]
              [else (found (cdr names) (cdr vars-s) (cdr bodies))]))
      (found p-names b-vars p-bodies)]))
(define (apply-procedure proc1 val)
  (cases proc proc1
    ; ...
    [procedure [vars body saved-env]
      (let ([ext-env (foldr (lambda (va vl e) (extend-env va vl e)) saved-env vars val)])
        (value-of body ext-env))]
    ))
(define (value-of exp env)
  (cases expression exp
    ; ...
    [call-exp [rator rands]
      (let ([proc (expval->proc (value-of rator env))]
            [args (map (lambda (x) (value-of x env)) rands)])
        (apply-procedure proc args))]
    [letrec-exp [p-names b-vars-s p-bodies letrec-body]
      (value-of letrec-body (extend-env-rec p-names b-vars-s p-bodies env))]))

; 3.35
(define (apply-env env1 var)
  (cases env env1
    ; ...
    [extend-env [saved-var saved-val saved-env]
      (if (eqv? saved-var var)
        (if (vector? saved-val)
          (vector-ref saved-val 0)
          saved-val)
        (apply-env saved-env var))]))

; 3.36
(define grammar-spec
  '(; ...
    [expression ("letrec" (arbno identifier "(" identifier ")" "=" expression)
                 "in" expression) letrec-exp]))
(define-datatype env env?
  [empty-env]
  [extend-env
    [vars (list-of identifier?)]
    [vals (list-of (lambda (x) #t))]
    [env env?]])
(define (apply-env env1 var)
  (cases env env1
    ; ...
    [extend-env [saved-vars saved-vals saved-env]
      (define (found vars vals)
        (cond [(null? vars) '()]
              [(eqv? (car vars) var) (car vals)]
              [else (found (cdr vars) (cdr vals))]))
      (let ([val (found saved-vars saved-vals)])
        (cond [(null? val) (apply-env saved-env var)]
              [(vector? val) (vector-ref val 0)]
              [else val]))]))
(define (extend-env-rec p-names b-vars bodies saved-env)
  (let ([vals (map (lambda (v) (make-vector 1)) b-vars)])
    (let ([new-env (extend-env p-names vals saved-env)])
      (map (lambda (vec var body) (vector-set! vec 0 (proc-val (procedure var body new-env)))) vals b-vars bodies)
      new-env)))
(define (apply-procedure proc1 val)
  (cases proc proc1
    [procedure [var body saved-env]
      (value-of body (extend-env (list var) (list val) saved-env))]))
(define (value-of exp env)
  (cases expression exp
    ; ...
    [letrec-exp [p-names b-vars p-bodies letrec-body]
      (value-of letrec-body (extend-env-rec p-names b-vars p-bodies env))]))
