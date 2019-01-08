(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")
(require racket/trace)

(define (run str)
  (value-of-program
    (scan&parse str)))

(define (value-of-program pgm)
  (init-store!)
  (cases program pgm
    [a-program [expr]
      (value-of expr (init-env))]))

(define (value-of expr env)
  (cases expression expr
    [const-exp [num] 
      (num-val num)]
    [var-exp [var]
      (deref (apply-env env var))]
    [diff-exp [exp1 exp2]
      (let ([val1 (value-of exp1 env)] [val2 (value-of exp2 env)])
        (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
          (num-val (- num1 num2))))]
    [not-exp [exp1]
      (let ([bool (expval->bool (value-of exp1 env))])
        (bool-val (not bool)))]
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
    [let-exp [vars exps body]
      (let ([vals (map (lambda (e) (value-of e env)) exps)])
        (let ([refs (map (lambda (v) (newref v)) vals)])
          (value-of body (extend-env vars refs env))))]
    [proc-exp [vars body]
      (proc-val (procedure vars body env))]
    [call-exp [rator rands]
      (let ([proc1 (expval->proc (value-of rator env))]
            [args (map (lambda (x) (value-of x env)) rands)])
        (cases proc proc1
          [procedure [vars body saved-env]
            (let ([refs (map (lambda (a) (newref a)) args)])
              (value-of body (extend-env vars refs saved-env)))]))]
    [letrec-exp [names varss bodies letrec-body]
      (value-of letrec-body (extend-env-rec names varss bodies env))]
    [assign-exp [var exp1]
      (begin (setref! (apply-env env var) (value-of exp1 env))
             (num-val 27))]
    [begin-exp [exp1 exps]
      (let ([val1 (value-of exp1 env)])
        (foldl (lambda (e v) (value-of e env)) val1 exps))]
    [else
      (report-invalid-expression expr)]
    ))

(define (result str)
  (result-of-program
    (scan&parse str)))

(define (result-of-program pgm)
  (init-store!)
  (cases program pgm
    [a-program [stmt]
      (result-of stmt (init-env))]))

(define (result-of stmt env)
  (cases statement stmt
    [assign-stmt [var1 exp1]
      (setref! (apply-env env var1) (value-of exp1 env))]
    [print-stmt [exp1]
      (let ([val (value-of exp1 env)])
        (cases expval val
          [num-val [num]   (printf "~a\n" num)]
          [bool-val [bool] (printf "~a\n" bool)]
          [proc-val [proc] (printf "~a\n" proc)]))]
    [multi-stmt [stmts]
      (map (lambda (s) (result-of s env)) stmts)]
    [if-stmt [exp1 stmt1 stmt2]
      (let ([val (value-of exp1 env)])
        (if (expval->bool val)
          (result-of stmt1 env)
          (result-of stmt2 env)))]
    [while-stmt [exp1 stmt1]
      (define (loop)
        (let ([val (value-of exp1 env)])
          (if (expval->bool val)
            (begin (result-of stmt1 env)
                   (loop))
            #f)))
      (loop)]
    [var-stmt [vars exps stmt1]
      (let ([refs (map (lambda (e) (newref (value-of e env))) exps)])
        (result-of stmt1 (extend-env vars refs env)))]
    [read-stmt [var1]
      (let ([ref (apply-env env var1)] [num (read)])
        (if (number? num)
          (setref! ref (num-val num))
          (report-invalid-number)))]
    [do-while-stmt [stmt1 exp1]
      (begin (result-of stmt1 env)
             (result-of (while-stmt exp1 stmt1) env))]
    ))

;(trace value-of-program)
;(trace value-of)
;(trace apply-env)
;(trace apply-proc)
;(trace result-of)

; res = (num-val 1)
(define p
  "let x = 0
   in letrec
      even()
        = if zero?(x) then 1 else begin set x = -(x,1); (odd) end
      odd()
        = if zero?(x) then 0 else begin set x = -(x,1); (even) end
      in begin set x = 13; (odd) end")

; res = (num-val 12)
(define p5
  "let f = proc(x)
             proc(y)
               begin
                 set x = -(x, -1);
                 -(x, y)
               end
   in ((f 44) 33)")

; res = (num-val 12)
(define p6
  "let times4 = 0
   in begin
       set times4 = proc(x)
                     if zero?(x)
                     then 0
                     else -((times4 -(x,1)), -4);
       (times4 3)
      end")

; res = 7
(define p7
  "var x, y;
   { x = 3; y = 4; print -(x,y)}")

; res = 5
(define p8
  "var x, y, z;
   { x = 10; y = 1; z = 5;
     while not(zero?(z))
     { x = -(x,y); z = -(z,1) };
     print x
   }")

; res = 3 4 3
(define p9
  "var x;
   { x = 3; print x;
     var x; { x = 4; print x}; 
     print x}")
 
; res = 1
(define p10
  "var f, x;
   { f = proc(x y) -(x, y);
     x = 3;
     print (f 4 x)
   }")

; res = input
(define p11
  "var x;
   { x = 0;
     print x;
     read x;
     print x
   }")

; res = 4
(define p12
  "var x, y, z;
   { x = 10; y = 1; z = 5;
     do {
       x = -(x,y); z = -(z,1)
     } while zero?(z);
     print x
   }")

(define p13
  "var x = 10, y = 1, z = 5;
   { do {
       x = -(x,y); z = -(z,1)
     } while zero?(z);
     print x
   }")
