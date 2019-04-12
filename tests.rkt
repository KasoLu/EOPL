(load "main.rkt")

(define p2
  "module m1
     interface
       [a : Int
        b : Int
        c : Int]
     body
       depends-on
       [a = 33
        b = 44
        c = 55]
   module m2
     interface
       [a : Int
        b : Int]
     body
       depends-on m1
       [a = 66
        b = from m1 take b]
   depends-on m1, m2
   let z = 99 in
     -(z, -(from m1 take a, from m2 take a))")
