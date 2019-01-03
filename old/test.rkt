; functions
(require racket/trace)
(define (report-no-binding-found var)
  (eopl:error 'report-no-binding-found "No binding for ~s" var))
(define (report-invalid-concrete-syntax datum)
  (eopl:error 'report-invalid-concrete-syntax "invalid concrete syntax: ~a" datum))
(define (report-expval-extractor-error type val)
  (eopl:error 'report-expval-extractor-error "invalid expval - ~a: ~a" type val))
(define (identifier? x) 
  (symbol? x))

; Program     ::= Expression
; Expression  ::= Number
; Expression  ::= -( Expression , Expression )
; Expression  ::= zero?( Expression )
; Expression  ::= if Expression then Expression else Expression
; Expression  ::= Identifier
; Expression  ::= let Identifier = Expression in Expression
; Expression  ::= letrec Identifier ( Identifier ) = Expression in Expression
; Expression  ::= letrec {Identifier ( Identifier ) = Expression}* in Expression
; Expression  ::= %lexref Number
; Expression  ::= %let Expression in Expression
; Expression  ::= %lexproc Expression

(define scanner-spec
  '([whitespace (whitespace) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number ((or "-" "") digit (arbno digit)) number]))
(define grammar-spec
  '([program (expression) a-program]
    [expression (number) const-exp]
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression (identifier) var-exp]
    [expression ("let" identifier "=" expression "in" expression) let-exp]
    [expression ("proc" "(" identifier ")" expression) proc-exp]
    [expression ("(" expression expression ")") call-exp]))
(define scanner
  (sllgen:make-string-scanner scanner-spec grammar-spec))
(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar-spec))

(define-datatype env env?
  [empty-env]
  [extend-env
    [var identifier?]
    [val (lambda (x) #t)]
    [env env?]]
  ;[extend-env
  ;  [vars (list-of identifier?)]
  ;  [vals (list-of (lambda (x) #t))]
  ;  [env env?]]
  ;[extend-env-rec
  ;  [p-names (list-of identifier?)]
  ;  [b-var (list-of (list-of identifier?))]
  ;  [bodies (list-of expression?)]
  ;  [env env?]]
  )
(define-datatype proc proc?
  [procedure 
    [body expression?]
    [saved-nameless-env nameless-env?]]
  ;[procedure
  ;  [var identifier?]
  ;  [body expression?]
  ;  [saved-env env?]]
  ;[procedure
  ;  [vars (list-of identifier?)]
  ;  [body expression?]
  ;  [saved-env env?]]
  )
(define-datatype expval expval?
  [num-val
    [num number?]]
  [bool-val
    [bool boolean?]]
  [proc-val
    [proc proc?]])
(define-datatype program program?
  [a-program 
    [exp1 expression?]])
(define-datatype expression expression?
  [const-exp
    [num number?]]
  [diff-exp
    [exp1 expression?]
    [exp2 expression?]]
  [zero?-exp
    [exp1 expression?]]
  [if-exp
    [exp1 expression?]
    [exp2 expression?]
    [exp3 expression?]]
  [var-exp
    [var identifier?]]
  [let-exp
    [var identifier?]
    [exp expression?]
    [body expression?]]
  [proc-exp
    [var identifier?]
    [body expression?]]
  [call-exp
    [rator expression?]
    [rand expression?]]
  ;[call-exp
  ;  [rator expression?]
  ;  [rands (list-of expression?)]]
  ;[letrec-exp
  ;  [p-names (list-of identifier?)]
  ;  [b-vars (list-of identifier?)]
  ;  [p-body (list-of expression?)]
  ;  [letrec-body expression?]]
  [nameless-var-exp
    [num number?]]
  [nameless-let-exp
    [exp1 expression?]
    [body expression?]]
  [nameless-proc-exp
    [body expression?]]
  )

(define (init-env)
  (empty-env))
(define (apply-env env1 var)
  (cases env env1
    [empty-env []
      (report-no-binding-found var)]
    [extend-env [saved-var saved-val saved-env]
      (if (eqv? saved-var var)
        saved-val
        (apply-env saved-env var))]
    ;[extend-env [saved-vars saved-vals saved-env]
    ;  (define (found vars vals)
    ;    (cond [(null? vars) '()]
    ;          [(eqv? (car vars) var) (car vals)]
    ;          [else (found (cdr vars) (cdr vals))]))
    ;  (let ([val (found saved-vars saved-vals)])
    ;    (cond [(null? val) (apply-env saved-env var)]
    ;          [(vector? val) (vector-ref val 0)]
    ;          [else val]))]
    ;[extend-env-rec [p-name b-var p-body saved-env]
    ;  (if (eqv? var p-name)
    ;    (proc-val (procedure b-var p-body env1))
    ;    (apply-env saved-env var))]
    ;[extend-env-rec [p-names b-vars p-bodies saved-env]
    ;  (define (found names vars-s bodies)
    ;    (cond [(null? names) (apply-env saved-env var)]
    ;          [(eqv? (car names) var) (proc-val (procedure (car vars-s) (car bodies) env1))]
    ;          [else (found (cdr names) (cdr vars-s) (cdr bodies))]))
    ;  (found p-names b-vars p-bodies)]
    ))
;(define (extend-env-rec p-names b-vars bodies saved-env)
;  (let ([vals (map (lambda (v) (make-vector 1)) b-vars)])
;    (let ([new-env (extend-env p-names vals saved-env)])
;      (map (lambda (vec var body) (vector-set! vec 0 (proc-val (procedure var body new-env)))) vals b-vars bodies)
;      new-env)))

(define (empty-senv) '())
(define (extend-senv var senv)
  (cons var senv))
(define (apply-senv senv var)
  (cond [(null? senv) (report-unbound-var var)]
        [(eqv? var (car senv)) 0]
        [else (+ 1 (apply-senv (cdr senv) var))]))
(define (init-senv)
  (extend-senv 'i 
    (extend-senv 'v
      (extend-senv 'x
        (empty-senv)))))

; nameless-env? : SchemeVal -> Bool
(define (nameless-env? x)
  ((list-of expval?) x))
; empty-nameless-env : () -> Nameless-env
(define (empty-nameless-env) '())
; extend-nameless-env : ExpVal x Nameless-env -> Nameless-env
(define (extend-nameless-env val nameless-env)
  (cons val nameless-env))
; apply-nameless-env : Nameless-env x Lexaddr -> ExpVal
(define (apply-nameless-env n)
  (list-ref nameless-env n))
(define (init-nameless-env)
  (empty-nameless-env))

(define (apply-procedure proc1 val)
  (cases proc proc1
    [procedure [body saved-nameless-env]
      (value-of body
        (extend-nameless-env val saved-nameless-env))]))
;(define (apply-procedure proc1 val)
;  (cases proc proc1
;    [procedure [var body saved-env]
;      (value-of body (extend-env var val saved-env))]
;    ;[procedure [var body saved-env]
;    ;  (value-of body (extend-env (list var) (list val) saved-env))]
;    ;[procedure [vars body saved-env]
;    ;  (let ([ext-env (foldr (lambda (va vl e) (extend-env va vl e)) saved-env vars val)])
;    ;    (value-of body ext-env))]
;    ))

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

;(define (run ast)
;  (cases program ast
;    [a-program [exp1] (value-of exp1 (init-env))]))
;(define (value-of-program pgm)
;  (cases program pgm
;    [a-program [expr]
;      (value-of expr (init-env))]))
(define (value-of-program pgm)
  (cases program pgm
    [a-program [expr]
      (value-of expr (init-nameless-env))]))
(define (value-of exp env)
  (cases expression exp
    [const-exp [num] 
      (num-val num)]
    [var-exp [var] 
      (apply-env env var)]
    [nameless-var-exp [n]
      (apply-nameless-env nameless-env n)]
    [diff-exp [exp1 exp2]
      (let ([val1 (value-of exp1 env)] [val2 (value-of exp2 env)])
        (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
          (num-val (- num1 num2))))]
    [zero?-exp [exp1]
      (let ([val1 (value-of exp1 env)])
        (let ([num1 (expval->num val1)])
          (if (zero? num1)
            (bool-val #t)
            (bool-val #f))))]
    [if-exp [exp1 exp2 exp3]
      (let ([val1 (value-of exp1 env)])
        (if (expval->bool val1)
          (value-of exp2 env)
          (value-of exp3 env)))]
    ;[let-exp [var exp body]
    ;  (value-of body (extend-env (list var) (list (value-of exp env)) env))]
    [let-exp [var exp body]
      (value-of body (extend-env var (value-of exp env) env))]
    [nameless-let-exp [expr body]
      (let ([val (value-of expr env)])
        (value-of body (extend-nameless-env val env)))]
    [proc-exp [var body]
      (proc-val (procedure var body env))]
    [nameless-proc-exp [body]
      (proc-val (procedure body env))]
    [call-exp [rator rand]
      (let ([proc (expval->proc (value-of rator env))]
            [arg (value-of rand env)])
        (apply-procedure proc arg))]
    ;[letrec-exp [p-names b-vars p-bodies letrec-body]
    ;  (value-of letrec-body (extend-env-rec p-names b-vars p-bodies env))]
    ;[call-exp [rator rands]
    ;  (let ([proc (expval->proc (value-of rator env))]
    ;        [args (map (lambda (x) (value-of x env)) rands)])
    ;    (apply-procedure proc args))]
    ;[letrec-exp [p-names b-vars-s p-bodies letrec-body]
    ;  (value-of letrec-body (extend-env-rec p-names b-vars-s p-bodies env))]
    [else
      (report-invalid-expression exp)]
    ))

(define (translation-of-program pgm)
  (cases program pgm
    [a-program [exp1]
      (a-program (translation-of exp1 (init-senv)))]))
(define (translation-of expr senv)
  (cases expression expr
    [const-exp [num]
      (const-exp num)]
    [diff-exp [expr1 expr2]
      (diff-exp
        (translation-of expr1 senv)
        (translation-of expr2 senv))]
    [zero?-exp [expr]
      (zero?-exp (translation-of expr senv))]
    [if-exp [expr1 expr2 expr3]
      (if-exp
        (translation-of expr1 senv)
        (translation-of expr2 senv)
        (translation-of expr3 senv))]
    [var-exp [var]
      (nameless-var-exp (apply-senv senv var))]
    [let-exp [var expr body]
      (nameless-let-exp
        (translation-of expr senv)
        (translation-of body (extend-senv var senv)))]
    [proc-exp [var body]
      (nameless-proc-exp
        (translation-of body
          (extend-senv var senv)))]
    [call-exp [rator rand]
      (call-exp
        (translation-of rator senv)
        (translation-of rand senv))]
    [else
      (report-invalid-source-expression expr)]))

(define (run str)
  (value-of-program
    (translation-of-program
      (scan&parse str))))

;(trace apply-env)
;(trace value-of)
;(define p
;  (string-append "letrec"
;                 "  even(x) = if zero?(x) then 1 else (odd -(x,1)) "
;                 "  odd(x)  = if zero?(x) then 0 else (even -(x,1)) "
;                 "in (odd 13)"))
;(define p
;  "letrec double(x) = if zero?(x) then 0 else -((double -(x,1)), -2) in (double 6)")
;(define ast (scan&parse p))
(define p
  "let x = 37 in proc (y) let z = -(y,x) in -(x,y)")
;(run p)

