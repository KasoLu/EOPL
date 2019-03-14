(load "main.rkt")

(define p1 ; (() -> Int)
  "let a = proc(x: Int y: ?) x
       b = proc() 10 in
     letrec f(x: ? y: ?) -> ? =
              if zero?(x)
              then 10
              else (g -(x, 1) zero?(0)) 
            g(x: Int y: ?) -> Int =
              if zero?(x)
              then 20
              else (f -(x, 1) 0) in
       b")

(define p2 ; (refto Int)
  "let a = newref(10) in
     let b = newref(deref(a)) in b")
