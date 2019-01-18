(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")
(require racket/trace)

(define (result str)
  (result-of-program
    (scan&parse str)))

(define (result-of-program pgm)
  (init-store!)
  (cases program pgm
    [a-program [stmt]
      (result-of/k stmt (init-env)
        (lambda () (eopl:printf "End of statement\n")))]))

(define (value-of/k expr env cont)
  (cases expression expr
    [const-exp [num] 
      (apply-cont cont (num-val num))]
    [var-exp [var]
      (apply-cont cont (deref (apply-env env var)))]
    [diff-exp [exp1 exp2]
      (value-of/k exp1 env
        (lambda (val1)
          (value-of/k exp2 env
            (lambda (val2)
              (let ([num1 (expval->num val1)] [num2 (expval->num val2)])
                (apply-cont cont (num-val (- num1 num2))))))))]
    [not-exp [exp1]
      (value-of/k exp1 env
        (lambda (val1)
          (let ([bool (expval->bool val1)])
            (apply-cont cont (bool-val (not bool))))))]
    [zero?-exp [exp1]
      (value-of/k exp1 env
        (lambda (val1)
          (apply-cont cont (bool-val (zero? (expval->num val1))))))]
    [if-exp [exp1 exp2 exp3]
      (value-of/k exp1 env
        (lambda (val1)
          (if (expval->bool val1)
            (value-of/k exp2 env cont)
            (value-of/k exp3 env cont))))]
    [let-exp [vars exps body]
      (if (null? vars)
        (value-of/k body env cont)
        (let loop([exps exps] [vals '()])
          (if (null? exps)
            (value-of/k body (extend vars (reverse vals) env) cont)
            (value-of/k (car exps) env
              (lambda (val)
                (loop (cdr exps) (cons (newref val) vals)))))))]
    [proc-exp [vars body]
      (apply-cont cont (proc-val (procedure vars body env)))]
    [call-exp [rator rands]
      (value-of/k rator env
        (lambda (rator)
          (let loop([rands rands] [vals '()])
            (if (null? rands)
              (apply-proc/k (expval->proc rator) (reverse vals) cont)
              (value-of/k (car rands) env
                (lambda (val)
                  (loop (cdr rands) (cons (newref val) vals))))))))]
    [letrec-exp [names varss bodies letrec-body]
      (value-of/k letrec-body (extend-env-rec names varss bodies env) cont)]
    [assign-exp [var exp1]
      (value-of/k exp1 env
        (lambda (val1)
          (begin (setref! (apply-env env var) val1)
                 (apply-cont cont (num-val 27)))))]
    [begin-exp [exp1 exps]
      (value-of/k exp1 env
        (lambda (val1)
          (let loop([exps exps] [val val1])
            (if (null? exps)
              (apply-cont cont val)
              (value-of/k (car exps) env
                (lambda (val)
                  (loop (cdr exps) val)))))))]
    [else
      (report-invalid-expression expr)]
    ))

(define (apply-proc/k proc1 refs cont)
  (cases proc proc1
    [procedure [vars body saved-env]
      (value-of/k body (extend-env vars refs) cont)]))

(define (apply-cont cont val)
  (cont val))

(define (result-of/k stmt env cmd-cont)
  (cases statement stmt
    [assign-stmt [var1 exp1]
      (value-of/k exp1 env
        (lambda (val1)
          (setref! (apply-env env var1) val1)
          (apply-cmd-cont cmd-cont)))]
    [print-stmt [exp1]
      (value-of/k exp1 env
        (lambda (val1)
          (cases expval val1
            [num-val [num]   (printf "~a\n" num)]
            [bool-val [bool] (printf "~a\n" bool)]
            [proc-val [proc] (printf "~a\n" proc)])
          (apply-cmd-cont cmd-cont)))]
    [multi-stmt [stmts]
      (let loop([stmts stmts])
        (if (null? stmts)
          (apply-cmd-cont cmd-cont)
          (result-of/k (car stmts) env
            (lambda ()
              (loop (cdr stmts))))))]
    [if-stmt [exp1 stmt1 stmt2]
      (value-of/k exp1 env
        (lambda (val1)
          (if (expval->bool val)
            (result-of/k stmt1 env cmd-cont)
            (result-of/k stmt2 env cmd-cont))))]
    [while-stmt [exp1 stmt1]
      (let loop()
        (value-of/k exp1 env
          (lambda (val1)
            (if (expval->bool val1)
              (result-of/k stmt1 env
                (lambda () (loop)))
              (apply-cmd-cont cmd-cont)))))]
    [var-stmt [vars stmt1]
      (let ([refs (map (lambda (v) (newref 'uninit)) vars)])
        (result-of/k stmt1 (extend-env vars refs env) cmd-cont))]))

(define (apply-cmd-cont cmd-cont)
  (cmd-cont))

;(trace value-of-program)
;(trace value-of)
;(trace apply-env)
;(trace apply-proc)
;(trace result-of)

; res = -1
(define p7
  "var x, y;
   { x = 3; y = 4; print -(x,y) }")

; res = 5
(define p8
  "var x, y, z;
   { x = 10; y = 1; z = 5;
     while not(zero?(z))
     { x = -(x,y); z = -(z,1) };
     print x }")

; res = 3 4 3
(define p9
  "var x;
   { x = 3; print x;
     var x; { x = 4; print x}; 
     print x }")
 
; res = 1
(define p10
  "var f, x;
   { f = proc(x y) -(x, y);
     x = 3;
     print (f 4 x) }")
