(load "main.rkt")

;res = (num-val 0)
(define p1
  "let a = 1 b = 2 c = 0
   in let d = -(b, a) e = c
      in letrec sub1(y1) = if zero?(y1) then y1 else (sub1 -(y1, 1))
         in (sub1 e)")

;res = (num-val 5)
(define p2
  "let a = 1 b = 2 c = 3
   in +(a, -(c, 2), c)")

;res = (num-val 55)
(define p3
  "letrec fib-anf(n) = 
     if zero?(-(n, 1))
     then 1
     else 
       if zero?(-(n, 2))
       then 1
       else 
         let val1 = (fib-anf -(n, 1))
         in let val2 = (fib-anf -(n, 2))
            in +(val1, val2)
   in (fib-anf 10)")

;res = (num-val 55)
(define p4
  "letrec fib(n) =
     if zero?(-(n, 1))
     then 1
     else 
       if zero?(-(n, 2))
       then 1
       else
         +((fib -(n, 1)), (fib -(n, 2)))
   in (fib 10)")

