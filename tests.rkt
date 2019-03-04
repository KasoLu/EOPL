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
       begin
         set a = 10;
         set b = 20;
         b
       end")

;(Bool)
(define p2
  "let a = 10 b = 20 in
     let c = begin set b = proc() zero?(0); zero?(1) end in
       c")
