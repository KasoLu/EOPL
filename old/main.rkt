; Program     ::= Expression  
; [a-program (exp1)]
; Expression  ::= Number      
; [const-exp (num)]
; Expression  ::= -(Expression, Expression)
; [diff-exp (exp1 exp2)]
; Expression  ::= zero? (Expression)
; [zero?-exp (exp1)]
; Expression  ::= if Expression then Expression else Expression
; [if-exp (exp1 exp2 exp3)]
; Expression  ::= Identifier
; [var-exp (var)]
; Expression  ::= let Identifier = Expression in Expression
; [let-exp (var exp1 body)]
; Expression  ::= proc (Identifier) Expression
; [proc-exp (var body)]
; Expression  ::= (Expression Expression)
; [call-exp (rator rand)]

; ExpVal ::= Int | Bool | Proc

; num-val       : Int -> ExpVal
; bool-val      : Bool -> ExpVal
; expval->num   : ExpVal -> Int
; expval->bool  : ExpVal -> Bool

; const-exp     : Int -> Exp
; zero?-exp     : Exp -> Exp
; if-exp        : Exp x Exp x Exp -> Exp
; diff-exp      : Exp x Exp -> Exp
; var-exp       : Var -> Exp
; let-exp       : Var x Exp x Exp -> Exp
; value-of      : Exp x Env -> ExpVal


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
    [exp1 expression?]
    [body expression?]])

(define-datatype expval expval?
  [num-val
    [num number?]]
  [bool-val
    [bool boolean?]])

; expval->num : ExpVal -> Int
(define (expval->num val)
  (cases expval val
    [num-val [num] num]
    [else (report-expval-extractor-error 'num val)]))

(define (expval->bool val)
  (cases expval val
    [bool-val [bool] bool]
    [else (report-expval-extractor-error 'bool val)]))

; run : String -> ExpVal
(define (run str)
  (value-of-program (scan&parse str)))
; value-of-program : Program -> ExpVal
(define (value-of-program pgm)
  (cases program pgm
    [a-program [exp1]
      (value-of exp1 (init-env))]))
; value-of : Exp x Env -> ExpVal
(define (value-of exp env)
  (cases expression exp
    [const-exp [num] (num-val num)]
    [var-exp [var] (apply-env env var)]
    [diff-exp [exp1 exp2]
      (let ([val1 (value-of exp1 env)]
            [val2 (value-of exp2 env)])
        (let ([num1 (expval->num val1)]
              [num2 (expval->num val2)])
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
    [let-exp [var exp1 body]
      (let ([val1 (value-of exp1 env)])
        (value-of body (extend-env var val1 env)))]))

; proc? : SchemeVal -> Bool
(define (proc? val)
  (procedure? val))

; procedure : Var x Exp x Env -> Proc
(define (procedure var body env)
  (lambda (val) (value-of body (extend-env var val env))))

; apply-procedure : Proc x ExpVal -> ExpVal
(define (apply-proedure proc1 val)
  (proc1 val))

(define-datatype proc proc?
  [procedure
    [var identifier?]
    [body expression?]
    [saved-env environment?]])
; apply-procedure : Proc x ExpVal -> ExpVal
(define (apply-procedure proc1 val)
  (cases proc proc1
    [procedure [var body saved-env]
      (value-of body (extend-env var val saved-env))]))


