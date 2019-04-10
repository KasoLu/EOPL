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
   let z = 99 y = 20 in
     -(y, -(from m1 take a, from m2 take a))")

(define p3
  "module even-odd
     interface
       [even : (Int -> Bool)
        odd  : (Int -> Bool)]
     body
       letrec
         local-odd(x : Int) -> Bool = (local-even -(x, 1))
         local-even(x : Int) -> Bool = (local-odd -(x, 1)) in
           [even = local-even
            odd  = local-odd]
   from even-odd take even")
