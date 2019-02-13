(load "types.rkt")
(load "parse.rkt")
(load "funcs.rkt")
(require racket/trace)

; run : String -> FinalAnswer
(define (run str)
  (value-of-program 2 (scan&parse str)))

; value-of-program : Int x Program -> FinalAnswer
(define (value-of-program timeslice pgm)
  (init-store!)
  (init-scheduler! timeslice)
  (cases program pgm
    [a-program [expr]
      (value-of/k expr (init-env) (end-main-thread-cont))]))

; value-of/k : Expr x Env x Cont -> FinalAnswer
(define (value-of/k expr env cont)
  (cases expression expr
    [const-exp [num] 
      (apply-cont cont (num-val num))]
    [var-exp [var]
      (apply-cont cont (deref (apply-env env var)))]
    [proc-exp [vars body]
      (apply-cont cont (proc-val (procedure vars body env)))]
    [letrec-exp [names varss bodies letrec-body]
      (value-of/k letrec-body (extend-env-rec names varss bodies env) cont)]
    [zero?-exp [exp1]
      (value-of/k exp1 env
        (zero?-cont cont))]
    [if-exp [exp1 exp2 exp3]
      (value-of/k exp1 env
        (if-test-cont exp2 exp3 env cont))]
    [let-exp [vars exps body]
      (if (null? vars)
        (value-of/k body env cont)
        (value-of/k (car exps) env
          (let-cont vars (cdr exps) '() body env cont)))]
    [diff-exp [exp1 exp2]
      (value-of/k exp1 env
        (diff1-cont exp2 env cont))]
    [call-exp [rator rands]
      (value-of/k rator env
        (rator-cont rands env cont))]
    [assign-exp [var exp1]
      (value-of/k exp1 env
        (assign-cont (apply-env env var) cont))]
    [begin-exp [exp1 exps]
      (value-of/k exp1 env
        (begin-cont exps env cont))]
    [spawn-exp [exp1]
      (value-of/k exp1 env
        (spawn-cont cont))]
    [mutex-exp []
      (apply-cont cont (mutex-val (new-mutex)))]
    [wait-exp [exp1]
      (value-of/k exp1 env
        (wait-cont cont))]
    [signal-exp [exp1]
      (value-of/k exp1 env
        (signal-cont cont))]
    [print-exp [exp1]
      (value-of/k exp1 env
        (print-cont cont))]
    ))

; apply-cont : Cont x ExpVal -> FinalAnswer
(define (apply-cont cont1 val)
  (if (time-expired?)
    (begin 
      (place-on-ready-queue! (lambda () (apply-cont cont1 val)) (get-thread-id))
      (run-next-thread))
    (begin 
      (decrement-timer!)
      (cases cont cont1
        [end-main-thread-cont []
          (eopl:printf "End of mainthread: ~a\n" val)
          (set-final-answer! val)
          (run-next-thread)]
        [end-subthread-cont []
          (eopl:printf "End of subthread: ~a\n" val)
          (run-next-thread)]
        [diff1-cont [exp2 env cont]
          (value-of/k exp2 env
            (diff2-cont val env cont))]
        [diff2-cont [val1 env cont]
          (let ([num1 (expval->num val1)] [num2 (expval->num val)])
            (apply-cont cont (num-val (- num1 num2))))]
        [rator-cont [rands env cont]
          (if (null? rands)
            (apply-proc/k (expval->proc val) '() cont)
            (value-of/k (car rands) env
              (rands-cont val (cdr rands) '() env cont)))]
        [rands-cont [rator rands vals env cont]
          (let ([vals (cons (newref val) vals)])
            (if (null? rands)
              (apply-proc/k (expval->proc rator) (reverse vals) cont)
                (value-of/k (car rands) env
                  (rands-cont rator (cdr rands) vals env cont))))]
        [zero?-cont [cont]
          (apply-cont cont (bool-val (zero? (expval->num val))))]
             [if-test-cont [exp2 exp3 env cont]
                (if (expval->bool val)
                  (value-of/k exp2 env cont)
                  (value-of/k exp3 env cont))]
        [let-cont [vars exps vals body env cont]
          (let ([vals (cons (newref val) vals)])
            (if (null? exps)
              (value-of/k body (extend-env vars (reverse vals) env) cont)
              (value-of/k (car exps) env
                (let-cont vars (cdr exps) vals body env cont))))]
        [assign-cont [ref cont]
          (begin (setref! ref val)
                 (apply-cont cont (num-val 27)))]
        [begin-cont [exps env cont]
          (if (null? exps)
            (apply-cont cont val)
            (value-of/k (car exps) env
              (begin-cont (cdr exps) env cont)))]
        [spawn-cont [saved-cont]
          (let ([proc1 (expval->proc val)]
                [new-id (create-thread-id)]
                [cur-id (get-thread-id)])
            (place-on-ready-queue!
              (lambda ()
                (apply-proc/k proc1 (list (newref (num-val new-id)) 
                                          (newref (num-val cur-id))) 
                  (end-subthread-cont)))
              new-id)
            (apply-cont saved-cont (num-val new-id)))]
        [wait-cont [saved-cont]
          (wait-for-mutex
            (expval->mutex val)
            (lambda () (apply-cont saved-cont (num-val 52))))]
        [signal-cont [saved-cont]
          (signal-mutex
            (expval->mutex val)
            (lambda () (apply-cont saved-cont (num-val 53))))]
        [print-cont [saved-cont]
          (eopl:printf "~a\n" val)
          (apply-cont saved-cont (num-val 100))]
        ))))

; apply-proc/k : Proc x List(Ref) -> Cont
(define (apply-proc/k proc1 refs cont)
  (cases proc proc1
    [procedure [vars body saved-env]
      (value-of/k body (extend-env vars refs saved-env) cont)]))

;(trace value-of/k)
;(trace apply-proc/k)
;(trace apply-cont)
