(load "main.rkt")

(define p2
  "module m1
     interface
       [a : Int
        b : Int
        c : Int]
     body
       [a = 33
        b = 44
        c = 55]
   module m2
     interface
       [a : Int
        b : Int]
     body
       [a = 66
        b = 77]
   let z = 99 in
     -(z, -(from m1 take a, from m2 take a))")

(define p3
  "module m1
     interface
       [u : Int
        v : Int]
     body
       module m2
         interface [v : Int]
         body [v = 33]
       [u = 44
        v = -(from m2 take v, 1)]
   from m1 take v")
