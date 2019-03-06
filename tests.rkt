(load "main.rkt")

;res = (() -> Int)
(define p1
  "let a = proc(x : Int y : Bool) x 
       b = proc() 10 in
     letrec f(x : Int y : Bool) -> Int = 
              if zero?(x) 
              then 10 
              else (g -(x, 1) zero?(0)) 
            g(x : Int y : Bool) -> Int = 
              if zero?(x) 
              then 20 
              else (f -(x, 1) zero?(1)) in
       b")

