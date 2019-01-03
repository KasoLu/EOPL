; 2.1
(define N 15)
(define (zero) '())
(define is-zero? null?)
(define (successor n) 
  (cond [(is-zero? n) '(1)]
        [(= (- N 1) (car n)) (cons 0 (successor (cdr n)))]
        [else (cons (+ 1 (car n)) (cdr n))]))
(define (predecessor n)
  (cond [(or (is-zero? n) (equal? n '(1))) (zero)]
        [(zero? (car n)) (cons (- N 1) (predecessor (cdr n)))]
        [else (cons (- (car n) 1) (cdr n))]))
(define (plus x y)
  (if (is-zero? x)
    y
    (successor (plus (predecessor x) y))))
(define (mult x y)
  (if (is-zero? x)
    (zero)
    (plus y (mult (predecessor x) y))))
(define (factorial n)
  (if (is-zero? n)
    (successor (zero))
    (mult n (factorial (predecessor n)))))

; 2.3
; Diff-Tree ::= (one) | (diff Diff-Tree Diff-Tree)
(define pone '(one))
(define none '(diff (diff (one) (one)) (one)))
(define (inductive n)
  (define (neg-stack stack)
    (map (lambda (s) (if (equal? s pone) none pone)) stack))
  (define (traversal n stack)
    (cond [(equal? n pone) (list pone)]
          [else (letrec ([lstack (traversal (cadr n) stack)] 
                         [rstack (traversal (caddr n) lstack)])
                  (append (neg-stack rstack) lstack))]))
  (define (balance-stack stack)
    (define (new-stack-handle x new-stack)
      (cond [(null? new-stack) (cons x new-stack)]
            [(and (equal? x pone) (equal? (car new-stack) none)) (cdr new-stack)]
            [(and (equal? x none) (equal? (car new-stack) pone)) (cdr new-stack)]
            [else (cons x new-stack)]))
    (foldl new-stack-handle '() stack))
  (define (create-tree new-stack)
    (define (create-tree-handle x tree)
      (if (equal? x pone)
        (successor tree)
        (predecessor tree)))
    (foldl create-tree-handle (zero) new-stack))
  (create-tree (balance-stack (traversal n '()))))

(define (zero) 
  '(diff (one) (one)))
(define (is-zero? n)
  (equal? (inductive n) (zero)))
(define (successor n)
  (list 'diff n none))
(define (predecessor n)
  (list 'diff n pone))
(define (diff-tree-plus x y)
  (list 'diff x (list 'diff (zero) y)))

; 2.4
(define (empty-stack) '())
(define push cons)
(define pop cdr)
(define top car)
(define empty-stack? null?)

; 2.5
; empty-env : () -> Env
(define (empty-env) '())
; extend-env : Var x SchemeVal x Env -> Env
(define (extend-env var val env)
  (cons (cons var val) env))
; apply-env : Env x Var -> SchemeVal
(define (apply-env env search-var)
  (cond [(null? env) (report-no-binding-found search-var)]
        [(eqv? (caar env) search-var) (cdar env)]
        [else (apply-env (cdr env) search-var)]))
(define (report-no-binding-found search-val)
  (eopl:error 'apply-env "No binding for ~s" search-val))

; 2.8
; empty-env? : Env -> Bool
(define empty-env? null?)

; 2.9
; has-binding? : Env x Var -> Bool
(define (has-binding? env s)
  (cond [(empty-env? env) #f]
        [(eqv? (caar env) s) #t]
        [else (has-binding? (cdr env) s)]))

; 2.10
; extend-env* : Listof(Var) x Listof(SchemeVal) x Env -> Env
(define (extend-env* vars vals env)
  (foldl extend-env env vars vals))

; 2.11
; empty-env : () -> Env
(define (empty-env) '())
; extend-env : Var x SchemeVal x Env -> Env
(define (extend-env var val env)
  (cons (cons (list var) (list val)) env))
; apply-env : Env x Var -> SchemeVal
(define (apply-env env search-var)
  (define (search-in-ribcage vars vals x)
    (cond [(null? vars) '()]
          [(eqv? (car vars) x) (cons (car vars) (car vals))]
          [else (search-in-ribcage (cdr vars) (cdr vals) x)]))
  (define (access-in-ribcage binding)
    (if (null? binding) 
      (report-invalid-env env)
      (cdr binding)))
  (define (iter-for-env e)
    (cond [(null? e)
           (report-no-binding-found search-var)]
          [(pair? (search-in-ribcage (caar env) (cdar env) search-var))
           (access-in-ribcage (search-in-ribcage (caar env) (cdar env) search-var))]
          [else (iter-for-env (cdr e))]))
  (iter-for-env env))
(define (extend-env* vars vals env)
  (cons (cons vars vals) env))
(define (report-invalid-env env)
  (eopl:error 'apply-env "Bad enviroment: ~s" env))

; 2.12
(define (empty-stack)
  (lambda (op) 
    (cond [(eqv? op 'empty-stack?) #t]
          [else (eopl:error 'empty-stack "stack is empty: ~s" op)])))
(define (push saved-x saved-stack)
  (lambda (op)
    (cond [(eqv? op 'pop) saved-stack]
          [(eqv? op 'top) saved-x]
          [(eqv? op 'empty-stack?) #f]
          [else (eopl:error 'stack "invalid operation: ~s" op)])))
(define (pop stack)
  (stack 'pop))
(define (top stack)
  (stack 'top))
(define (empty-stack? stack)
  (stack 'empty-stack?))

; 2.13 - 2.14
(define (empty-env)
  (lambda ()
    (list (lambda (search-var) (report-no-binding-found search-var))
          (lambda () #t)
          (lambda (search-var) #f))))
(define (extend-env saved-var saved-val saved-env)
  (lambda ()
    (list (lambda (search-var) 
            (if (eqv? search-var saved-var)
              (cons saved-var saved-val)
              (apply-env saved-env search-var)))
          (lambda () #f)
          (lambda (search-var)
            (if (eqv? search-var saved-var)
              #t
              (has-binding? saved-env search-var))))))
(define (apply-env env search-var)
  ((car (env)) search-var))
(define (empty-env? env)
  ((cadr (env))))
(define (has-binding? env search-var)
  ((caddr (env)) search-var))

; 2.15
; Lc-exp ::= Id | (lambda (Id) Lc-exp) | (Lc-exp Lc-exp)
; lc-exp? : Lc-exp -> Bool
(define (lc-exp? lc-exp)
  (or (var-exp? lc-exp)
      (lambda-exp? lc-exp)
      (app-exp? lc-exp)))
; var-exp : Var -> Lc-exp
(define (var-exp var) var)
; var-exp? : Lc-exp -> Bool
(define (var-exp? lc-exp) (symbol? lc-exp))
; var-exp->var : Lc-exp -> Var
(define (var-exp->var lc-exp) lc-exp) 
; lambda-exp : Var x Lc-exp -> Lc-exp
(define (lambda-exp var lc-exp)
  (list 'lambda (list var) lc-exp))
; lambda-exp? : Lc-exp -> Bool
(define (lambda-exp? lc-exp)
  (and (eqv? (car lc-exp) 'lambda)
       (var-exp? (lambda-exp->bound-var lc-exp))
       (let ([body (lambda-exp->body lc-exp)])
         (or (var-exp? body)
             (lambda-exp? body)
             (app-exp? body)))))
; lambda-exp->bound-var : Lc-exp -> Var
(define (lambda-exp->bound-var lc-exp)
  (caadr lc-exp))
; lambda-exp->body : Lc-exp -> Lc-exp
(define (lambda-exp->body lc-exp)
  (caddr lc-exp))
; app-exp : Lc-exp x Lc-exp -> Lc-exp
(define (app-exp rator rand)
  (list rator rand))
; app-exp->rator : Lc-exp -> Lc-exp
(define (app-exp->rator lc-exp)
  (car lc-exp))
; app-exp->rand : Lc-exp -> Lc-exp
(define (app-exp->rand lc-exp)
  (cadr lc-exp))
; app-exp? : Lc-exp -> Bool
(define (app-exp? lc-exp)
  (and (lc-exp? (app-exp->rator lc-exp))
       (lc-exp? (app-exp->rand lc-exp))))

; 2.16
; lambda-exp : Var x Lc-exp -> Lc-exp
(define (lambda-exp var lc-exp)
  (list 'lambda var lc-exp))
; lambda-exp? : Lc-exp -> Bool
(define (lambda-exp? lc-exp)
  (and (eqv? (car lc-exp) 'lambda)
       (var-exp? (lambda-exp->bound-var lc-exp))
       (let ([body (lambda-exp->body lc-exp)])
         (or (var-exp? body)
             (lambda-exp? body)
             (app-exp? body)))))
; lambda-exp->bound-var : Lc-exp -> Var
(define (lambda-exp->bound-var lc-exp)
  (cadr lc-exp))
; lambda-exp->body : Lc-exp -> Lc-exp
(define (lambda-exp->body lc-exp)
  (caddr lc-exp))

; 2.18
; NodeInSequence ::= (Int Listof(Int) Listof(Int))
(define (number->sequence i) (list i '() '()))
(define (current-element nis) (car nis))
(define (move-to-left nis)
  (if (null? (cadr nis))
    (eopl:error 'move-to-left "sequence left is end: ~s" nis)
    (list (caadr nis) (cdadr nis) (cons (current-element nis) (caddr nis)))))
(define (move-to-right nis)
  (if (null? (caddr nis))
    (eopl:error 'move-to-right "sequence right is end: ~s" nis)
    (list (caaddr nis) (cons (current-element nis) (cadr nis)) (cdaddr nis))))
(define (at-left-end? nis)
  (null? (cadr nis)))
(define (at-right-end? nis)
  (null? (caddr nis)))
(define (insert-to-left i nis)
  (list (current-element nis) (cons i (cadr nis)) (caddr nis)))
(define (insert-to-right i nis)
  (list (current-element nis) (cadr nis) (cons i (caddr nis))))

; 2.19
; Bintree ::= () | (Int Bintree Bintree)
(define (number->bintree i) (list i '() '()))
(define (current-element bt) (car bt))
(define (move-to-left-son bt) (cadr bt))
(define (move-to-right-son bt) (caddr bt))
(define (at-leaf? bt) 
  (null? bt))
(define (insert-to-left i bt)
  (letrec ([ori-left (move-to-left-son bt)]
           [new-son (list i ori-left '())])
    (list (current-element bt) new-son (move-to-right-son bt))))
(define (insert-to-right i bt)
  (letrec ([ori-right (move-to-right-son bt)]
           [new-son (list i '() ori-right)])
    (list (current-element bt) (move-to-left-son bt) new-son)))

; 2.20
; BTNodeInSequence ::= (Bintree Listof(Bintree) Listof(Bintree))
(define (number->btseq i)
  (let ([bt (number->bintree i)])
    (list bt '() '())))
(define (btseq-current-element btseq)
  (caar btseq))
(define (btseq-move-to-left-son btseq)
  (let ([bt (car btseq)])
    (list (move-to-left-son bt) (cons bt (cadr btseq)) '())))
(define (btseq-move-to-right-son btseq)
  (let ([bt (car btseq)])
    (list (move-to-right-son bt) (cons bt (cadr btseq)) '())))
(define (btseq-at-leaf? btseq)
  (let ([bt (car btseq)])
    (null? bt)))
(define (btseq-insert-to-left i btseq)
  (let ([bt (car btseq)])
    (list (insert-to-left i bt) (cadr btseq) (caddr btseq))))
(define (btseq-insert-to-right i btseq)
  (let ([bt (car btseq)])
    (list (insert-to-right i bt) (cadr btseq) (caddr btseq))))
(define (btseq-move-up btseq)
  (let ([bt (car btseq)])
    (list (caadr btseq) (cdadr btseq) '())))
(define (btseq-at-root? btseq)
  (null? (cadr btseq)))

; 2.21
; Env-exp ::= (empty-env) | (extend-env Identifier Scheme-value Env-exp)
(define-datatype Env env?
  [empty-env]
  [extend-env 
    [var symbol?]
    [val (lambda (x) #t)]
    [env env?]])
; has-binding? : Env x Var -> Bool
(define (has-binding? env s)
  (cases Env env
    [empty-env []
      (report-no-binding-found s)]
    [extend-env [saved-var saved-val saved-env]
      (if (eqv? saved-var s)
        #t
        (has-binding? saved-env s))]))

; 2.22
; Stack-exp ::= (empty-stack) | (push-stack SchemeVal Stack-exp)
(define-datatype Stack stack?
  [empty-stack]
  [push-stack
    [val (lambda (x) #t)]
    [stack stack?]])
(define (push stack val)
  (push-stack val stack))
(define (pop stack)
  (cases Stack stack
    [empty-stack [] (eopl:error 'pop "the stack is empty")]
    [push-stack [saved-val saved-stack] saved-stack]))
(define (top stack)
  (cases Stack stack
    [empty-stack [] (eopl:error 'top "the stack is empty")]
    [push-stack [saved-val saved-stack] saved-val]))
(define (empty-stack? stack)
  (cases Stack stack
    [empty-stack [] #t]
    [else #f]))

; 2.23
(define-datatype lc-exp lc-exp?
  [var-exp
    [var (lambda (x) (and (not (eqv? x 'lambda)) (identifier? x)))]]
  [lambda-exp
    [bound-var identifier?]
    [body lc-exp?]]
  [app-exp
    [rator lc-exp?]
    [rand lc-exp?]])

; 2.24
(define (bintree-to-list bt)
  (cases bintree bt
    [leaf-node [num]
      (list 'leaf-node num)]
    [interior-node [key left right]
      (list 'interior-node key (bintree-to-list left) (bintree-to-list right))]))

; 2.25
(define (max-interior bt)
  (define (sum-interior bt)
    (cases bintree bt
      [leaf-node [num]
        (list (list 'leaf-node num))]
      [interior-node [key left right]
        (let ([ls (sum-interior left)] [rs (sum-interior right)])
          (cons (list key (+ (cadar ls) (cadar rs))) 
                (append ls rs)))]))
  (define (filter-leaf blst)
    (filter (lambda (x) (not (eqv? (car x) 'leaf-node))) blst))
  (define (max-node? n1 n2)
    (cond [(null? n2) n1]
          [(> (cadr n1) (cadr n2)) n1]
          [else n2]))
  (let ([lst (filter-leaf (sum-interior bt))])
    (car (foldl max-node? '() lst))))

; 2.26
(define (e226 rbst)
  (define (e226-aux rbst c)
    (cases RBSTree rbst
      [red-node [n1 n2]
        (red-node (e226-aux n1 (+ c 1)) (e226-aux n2 (+ c 1)))]
      [blue-node [ns]
        (blue-node (map (lambda (x) (e226-aux x c)) ns))]
      [leaf-node [i]
        (leaf-node c)]))
  (e226-aux rbst 0))

; 2.29
; Lc-exp ::= Identifier
;        ::= (lambda ({Identifier}*) Lc-exp)
;        ::= (Lc-exp {Lc-exp}*)
(define-datatype Lc-exp2 lc-exp2?
  [var-exp 
    [var identifier?]]
  [lambda-exp
    [bound-vars (list-of identifier?)]
    [body lc-exp2?]]
  [app-exp
    [rator lc-exp2?]
    [rands (list-of lc-exp2?)]])
(define (parse-expression2 exp)
  (cond [(symbol? exp) (var-exp exp)]
        [(pair? exp)
         (if (eqv? (car exp) 'lambda)
           (lambda-exp (cadr exp) (parse-expression2 (caddr exp)))
           (app-exp (parse-expression2 (car exp))
                    (map parse-expression2 (cdr exp))))]))

; 2.30
; parse-expression : SchemeVal -> LcExp
(define (parse-expression datum)
  (cond [(identifier? datum) (var-exp datum)]
        [(pair? datum) 
         (if (eqv? (car datum) 'lambda)
           (if (and (pair? (cdr datum)) (pair? (cddr datum))) 
             (lambda-exp (caadr datum) (parse-expression (caddr datum)))
             (eopl:error 'parse-expression "lambda exp invalid: ~a" datum))
           (if (and (pair? (cdr datum)) (null? (cddr datum)))
             (app-exp (parse-expression (car datum)) (parse-expression (cadr datum)))
             (eopl:error 'parse-expression "application exp invalid: ~a" datum)))]
        [else (report-invalid-concrete-syntax datum)]))

; 2.31
; Prefix-list ::= (Prefix-exp)
; Prefix-exp  ::= Int
;             ::= - Prefix-exp Prefix-exp
(define-datatype prefix-exp prefix-exp?
  [const-exp
    [num integer?]]
  [diff-exp
    [operand1 prefix-exp?]
    [operand2 prefix-exp?]])
(define (parse-prefix-exp exp)
  (define (traverse-exp exp)
    (cond [(null? exp) (cons '() '())]
          [(number? (car exp)) 
           (cons (const-exp (car exp)) (cdr exp))]
          [(eqv? (car exp) '-)
           (letrec ([lpe (traverse-exp (cdr exp))]
                    [rpe (traverse-exp (cdr lpe))])
             (cons (diff-exp (car lpe) (car rpe)) (cdr rpe)))]))
  (car (traverse-exp exp)))
