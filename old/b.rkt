; b.1
(define-datatype arith-expr arith-expr?
  [a-expr
    [term arith-term?]
    [add-ops (list-of arith-add-op?)]
    [terms (list-of arith-term?)]])
(define-datatype arith-term arith-term?
  [a-term
    [factor arith-factor?]
    [mul-ops (list-of arith-mul-op?)]
    [factors (list-of arith-factor?)]])
(define-datatype arith-factor arith-factor?
  [a-number 
    [num number?]]
  [a-compound-expr 
    [expr arith-expr?]])
(define-datatype arith-add-op arith-add-op?
  [a-add]
  [a-sub])
(define-datatype arith-mul-op arith-mul-op?
  [a-mul]
  [a-div])
(define scanner-b1
  '([white-sp (whitespace) skip]
    [operator (letter) symbol]
    [number (digit (arbno digit)) number]))
(define grammar-b1
  '([arith-expr (arith-term (arbno arith-add-op arith-term)) a-expr]
    [arith-term (arith-factor (arbno arith-mul-op arith-factor)) a-term]
    [arith-factor (number) a-number]
    [arith-factor ("(" arith-expr ")") a-compound-expr]
    [arith-add-op ("+") a-add]
    [arith-add-op ("-") a-sub]
    [arith-mul-op ("*") a-mul]
    [arith-mul-op ("/") a-div]))
(define scan&parse-b1
  (sllgen:make-string-parser scanner-b1 grammar-b1))

; b.3
; interp-expr : expr -> number
(define (interp-expr expr)
  (cases arith-expr expr
    [a-expr [term add-ops terms]
      (foldl (lambda (op term res) 
               ((interp-add-op op) res (interp-term term)))
             (interp-term term) add-ops terms)]))
; interp-term : term -> number
(define (interp-term term)
  (cases arith-term term
    [a-term [factor mul-ops factors]
      (foldl (lambda (op factor res) 
               ((interp-mul-op op) res (interp-factor factor)))
             (interp-factor factor) mul-ops factors)]))
; interp-factor : factor -> number
(define (interp-factor factor)
  (cases arith-factor factor
    [a-number [num] num]
    [a-compound-expr [expr] (interp-expr expr)]))
; interp-arith-add-op : arith-add-op -> proc
(define (interp-add-op op)
  (cases arith-add-op op
    [a-add [] +]
    [a-sub [] -]))
; interp-arith-mul-op : arith-mul-op -> proc
(define (interp-mul-op op)
  (cases arith-mul-op op
    [a-mul [] *]
    [a-div [] /]))

; b.4
(define scanner-b4
  '([white-sp (whitespace) skip]
    [operator ((or "+" "-" "*" "/")) symbol]
    [num (digit (arbno digit)) number]
    [var (letter (arbno (or letter digit))) symbol]))
(define grammar-b4
  '([arith-expr   ("let" arith-var "=" arith-expr "in" arith-expr)  a-let]
    [arith-expr   (arith-term (arbno arith-add-op arith-term))      a-expr]
    [arith-term   (arith-factor (arbno arith-mul-op arith-factor))  a-term]
    [arith-factor (num)                                             a-number]
    [arith-factor (arith-var)                                       a-factor-var]
    [arith-factor ("(" arith-expr ")")                              a-compound-expr]
    [arith-add-op ("+")                                             a-add]
    [arith-add-op ("-")                                             a-sub]
    [arith-mul-op ("*")                                             a-mul]
    [arith-mul-op ("/")                                             a-div]
    [arith-var    (var)                                             a-var]))
(define scan&parse-b4
  (sllgen:make-string-parser scanner-b4 grammar-b4))
(define-datatype env env?
  [empty-env]
  [extend-env
    [var symbol?]
    [val (lambda (x) #t)]
    [env env?]])
(define (init-env)
  (empty-env))
(define (apply-env env1 var)
  (cases env env1
    [empty-env []
      (report-no-binding-found var)]
    [extend-env [saved-var saved-val saved-env]
      (if (eqv? saved-var var)
        saved-val
        (apply-env saved-env var))]))
(define (interp-expr expr env)
  (cases arith-expr expr
    [a-expr [term add-ops terms]
      (foldl (lambda (op term res) 
               ((interp-add-op op) res (interp-term term env)))
             (interp-term term env) add-ops terms)]
    [a-let [var val body]
      (let ([val-val (interp-expr val env)])
        (interp-expr body (extend-env (apply-var var) val-val env)))]))
(define (interp-term term env)
  (cases arith-term term
    [a-term [factor mul-ops factors]
      (foldl (lambda (op factor res) 
               ((interp-mul-op op) res (interp-factor factor env)))
             (interp-factor factor env) mul-ops factors)]))
(define (interp-factor factor env)
  (cases arith-factor factor
    [a-number [num] num]
    [a-compound-expr [expr] (interp-expr expr env)]
    [a-factor-var [var] (apply-env env (apply-var var))]))
(define (interp-add-op op)
  (cases arith-add-op op
    [a-add [] +]
    [a-sub [] -]))
(define (interp-mul-op op)
  (cases arith-mul-op op
    [a-mul [] *]
    [a-div [] /]))

; b.5
(define scanner-b4
  '(; ...
    [num ((or "-" "") digit (arbno digit)) number]))
