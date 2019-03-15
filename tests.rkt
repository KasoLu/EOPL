(load "main.rkt")

(define p1 ; (Int * T -> Int)
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
       a")

(define p2 ; (T -> T)
  "let f = proc(x: ?) x in
     if (f zero?(0))
       then f
       else f")
