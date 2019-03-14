(load "main.rkt")

(define p1 ; () -> Int
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

(define p2 ; Int * Bool -> Int
  "let f = proc(x: ?) unpair a b = x in -(a, if b then 10 else 20) in f")

(define p3 ; Int * Bool
  "newpair(10, zero?(0))")
