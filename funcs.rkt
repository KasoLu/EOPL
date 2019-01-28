(load "types.rkt")

(define (report-no-binding-found var)
  (eopl:error 'report-no-binding-found "No binding for ~s" var))
(define (report-invalid-concrete-syntax datum)
  (eopl:error 'report-invalid-concrete-syntax "invalid concrete syntax: ~a" datum))
(define (report-expval-extractor-error type val)
  (eopl:error 'report-expval-extractor-error "invalid expval - ~a: ~a" type val))

(define (init-env)
  (empty-env))
(define (apply-env env1 var)
  (cases env env1
    [empty-env []
      (report-no-binding-found var)]
    [extend-env [saved-vars saved-vals saved-env]
      (let loop([vars saved-vars] [vals saved-vals])
        (cond [(null? vars) (apply-env saved-env var)]
              [(eqv? (car vars) var) (car vals)]
              [else (loop (cdr vars) (cdr vals))]))]
    [extend-env-rec [names varss bodies saved-env]
      (let loop([names names] [varss varss] [bodies bodies])
        (cond [(null? names) 
               (apply-env saved-env var)]
              [(eqv? (car names) var) 
               (newref (proc-val (procedure (car varss) (car bodies) env1)))]
              [else 
               (loop (cdr names) (cdr varss) (cdr bodies))]))]
    ))

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
(define (expval->mutex val)
  (cases expval val
    [mutex-val [mutex] mutex]
    [else (report-expval-extractor-error 'mutex val)]))

(define the-store 'uninit)
; empty-store : () -> Store
(define (empty-store) '())
; get-store : () -> Store
(define (get-store) the-store)
; init-store! : () -> Unspecified
(define (init-store!) (set! the-store (empty-store)))

; newref : ExpVal -> Ref
(define (newref val)
  (let ([next-ref (length the-store)])
    (set! the-store (append the-store (list val)))
    next-ref))
; deref : Ref -> ExpVal
(define (deref ref)
  (list-ref the-store ref))
; setref! : Ref x ExpVal -> Unspecified
(define (setref! ref val)
  (define (setref-inner store1 ref1)
    (cond [(null? store1) (report-invalid-reference ref the-store)]
          [(zero? ref1) (cons val (cdr store1))]
          [else (cons (car store1) (setref-inner (cdr store1) (- ref1 1)))]))
  (set! the-store (setref-inner the-store ref)))

(define the-ready-queue    'uninit)
(define the-ready-times    'uninit)
(define the-final-answer   'uninit)
(define the-max-time-slice 'uninit)
(define the-time-remaining 'uninit)

; init-scheduler! : Int -> Unspecified
(define (init-scheduler! ticks)
  (set! the-ready-queue (empty-queue))
  (set! the-ready-times (empty-queue))
  (set! the-final-answer 'uninit)
  (set! the-max-time-slice ticks)
  (set! the-time-remaining the-max-time-slice))
; place-on-ready-queue! : Thread x Int -> Unspecified
(define (place-on-ready-queue! th times)
  (set! the-ready-queue (enqueue the-ready-queue th))
  (set! the-ready-times (enqueue the-ready-times times)))
; run-next-thread : () -> FinalAnswer
(define (run-next-thread)
  (if (empty? the-ready-queue)
    the-final-answer
    (dequeue the-ready-queue
      (lambda (first-ready-thread other-ready-threads)
        (dequeue the-ready-times
          (lambda (first-ready-times other-ready-timess)
            (set! the-ready-queue other-ready-threads)
            (set! the-ready-times other-ready-timess)
            (set! the-time-remaining first-ready-times)
            (first-ready-thread)))))))
; set-final-answer! : ExpVal -> Unspecified
(define (set-final-answer! val)
  (set! the-final-answer val))
; time-expired? : () -> Bool
(define (time-expired?)
  (zero? the-time-remaining))
; decrement-timer! : () -> Unspecified
(define (decrement-timer!)
  (set! the-time-remaining (- the-time-remaining 1)))
; time-max-slice : () -> Int
(define (time-max-slice) 
  the-max-time-slice)
; time-remaining : () -> Int
(define (time-remaining)
  the-time-remaining)

; new-mutex : () -> Mutex
(define (new-mutex)
  (a-mutex (newref #f) (newref '())))
; wait-for-mutex : Mutex x Thread -> FinalAnswer
(define (wait-for-mutex m th)
  (cases mutex m
    [a-mutex [ref-to-closed? ref-to-wait-queue]
      (cond [(deref ref-to-closed?)
             (setref! ref-to-wait-queue (enqueue (deref ref-to-wait-queue) th))
             (run-next-thread)]
            [else
             (setref! ref-to-closed? #t)
             (th)])]))
; signal-mutex : Mutex x Thread -> FinalAnswer
(define (signal-mutex m th)
  (cases mutex m
    [a-mutex [ref-to-closed? ref-to-wait-queue]
      (let ([closed? (deref ref-to-closed?)] [wait-queue (deref ref-to-wait-queue)])
        (if closed?
          (if (empty? wait-queue)
            (setref! ref-to-closed? #f)
            (dequeue wait-queue
              (lambda (first-waiting-th other-waiting-ths)
                (place-on-ready-queue! first-waiting-th (time-max-slice))
                (setref! ref-to-wait-queue other-waiting-ths))))
          (void))
        (th))]))

; empty-queue : () -> Queue
(define (empty-queue) '())
; enqueue : Queue x SchemeVal -> Queue
(define (enqueue q x) (append q (list x)))
; dequeue : Queue x Proc(head rest) -> Unspecified
(define (dequeue q c) (if (empty? q) (void) (c (car q) (cdr q))))
