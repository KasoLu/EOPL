(load "main.rkt")

;res = (num-val -1)
(define p1
  "let a = 1 b = 2
   in let c = -(b, a) d = 0
      in letrec add1(x1) = if zero?(-(x1, 10)) then x1 else (add1 -(x1, -1))
                sub1(y1) = if zero?(y1) then y1 else (sub1 -(y1, 1))
         in if zero?(c)
            then (sub1 d)
            else (add1 d)")

