(load "main.rkt")

(define p2
  "module m1
     interface
       [a : Int
        b : Int
        c : Int]
     body
       [b = 33
        c = 44
        a = 55]
   module m2
     interface
       [a : Int
        b : Int]
     body
       [b = 66
        a = 77]
   let z = 99 in
     -(z, -(from m1 take a, from m2 take a))")
