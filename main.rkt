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
    ))

(define (end-main-thread-cont)
  (cons (lambda (val cont)
          (eopl:printf "End of mainthread: ~a\n" val)
          (set-final-answer! val)
          (run-next-thread))
        '()))
(define (end-subthread-cont)
  (cons (lambda (val cont)
          (eopl:printf "End of subthread: ~a\n" val)
          (run-next-thread))
        '()))
(define (diff1-cont exp2 env saved-cont)
  (cons (lambda (val cont)
          (value-of/k exp2 env
            (diff2-cont val env cont)))
        saved-cont))
(define (diff2-cont val1 env saved-cont)
  (cons (lambda (val cont)
          (let ([num1 (expval->num val1)] [num2 (expval->num val)])
            (apply-cont cont (num-val (- num1 num2)))))
        saved-cont))
(define (rator-cont rands env saved-cont)
  (cons (lambda (val cont)
          (if (null? rands)
            (apply-proc/k (expval->proc val) '() cont)
            (value-of/k (car rands) env
              (rands-cont val (cdr rands) '() env cont))))
        saved-cont))
(define (rands-cont rator rands vals env saved-cont)
  (cons (lambda (val cont)
          (let ([vals (cons (newref val) vals)])
            (if (null? rands)
              (apply-proc/k (expval->proc rator) (reverse vals) cont)
              (value-of/k (car rands) env
                (rands-cont rator (cdr rands) vals env cont)))))
        saved-cont))
(define (zero?-cont saved-cont)
  (cons (lambda (val cont)
          (apply-cont cont (bool-val (zero? (expval->num val)))))
        saved-cont))
(define (if-test-cont exp2 exp3 env saved-cont)
  (cons (lambda (val cont)
          (if (expval->bool val)
            (value-of/k exp2 env cont)
            (value-of/k exp3 env cont)))
        saved-cont))
(define (let-cont vars exps vals body env saved-cont)
  (cons (lambda (val cont)
          (let ([vals (cons (newref val) vals)])
            (if (null? exps)
              (value-of/k body (extend-env vars (reverse vals) env) cont)
              (value-of/k (car exps) env
                (let-cont vars (cdr exps) vals body env cont)))))
        saved-cont))
(define (assign-cont ref saved-cont)
  (cons (lambda (val cont)
          (begin (setref! ref val)
                 (apply-cont cont (num-val 27))))
        saved-cont))
(define (begin-cont exps env saved-cont)
  (cons (lambda (val cont)
          (if (null? exps)
            (apply-cont cont val)
            (value-of/k (car exps) env
              (begin-cont (cdr exps) env cont))))
        saved-cont))
(define (spawn-cont saved-cont)
  (cons (lambda (val cont)
          (place-on-ready-queue!
            (lambda () 
              (apply-proc/k (expval->proc val) 
                            (list (newref (num-val 28))) 
                            (end-subthread-cont))))
          (apply-cont cont (num-val 73)))
        saved-cont))
(define (wait-cont saved-cont)
  (cons (lambda (val cont)
          (wait-for-mutex 
            (expval->mutex val)
            (lambda () (apply-cont cont (num-val 52)))))
        saved-cont))
(define (signal-cont saved-cont)
  (cons (lambda (val cont)
          (signal-mutex
            (expval->mutex val)
            (lambda () (apply-cont cont (num-val 53)))))
        saved-cont))

; apply-cont : Cont x ExpVal -> FinalAnswer
(define (apply-cont cont val)
  (if (time-expired?)
    (begin (place-on-ready-queue! (lambda () (apply-cont cont val)))
           (run-next-thread))
    (begin (decrement-timer!)
           (if (pair? cont)
             ((car cont) val (cdr cont))
             (void)))))

; apply-proc/k : Proc x List(Ref) -> Cont
(define (apply-proc/k proc1 refs cont)
  (cases proc proc1
    [procedure [vars body saved-env]
      (value-of/k body (extend-env vars refs saved-env) cont)]))

;(trace value-of/k)
;(trace apply-proc/k)
;(trace apply-cont)

; res = (num-val 1)
(define p1
  "let x = 0
   in letrec
      even()
        = if zero?(x) then 1 else begin set x = -(x,1); (odd) end
      odd()
        = if zero?(x) then 0 else begin set x = -(x,1); (even) end
      in begin set x = 13; (odd) end")

; res = (num-val 12)
(define p2
  "let f = proc(x)
             proc(y)
               begin
                 set x = -(x, -1);
                 -(x, y)
               end
   in ((f 44) 33)")

; res = (num-val 12)
(define p3
  "let times4 = 0
   in begin
       set times4 = proc(x) if zero?(x) then 0 else -((times4 -(x,1)), -4);
       (times4 3)
      end")

; res = (num-val 73)
(define p4
  "let x = 0
   in let incr_x = proc(id) proc(dummy) set x = -(x, -1)
      in begin
           spawn((incr_x 100));
           spawn((incr_x 200));
           spawn((incr_x 300))
         end")

; res = (num-val 73)
(define p5
  "let x = 0
   in let mut = mutex()
      in let incr_x = 
             proc(id) proc(dummy) begin wait(mut); set x = -(x, -1); signal(mut) end
         in begin
              spawn((incr_x 100));
              spawn((incr_x 200));
              spawn((incr_x 300))
            end")
