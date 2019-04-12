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
     interface [] body [x = print(1)]
   module m2
     interface [] body [x = print(2)]
   module m3
     interface []
     body
       import m2
       [x = print(3)]
   import m3, m1
   33")
