(load "main.rkt")
;(trace value-of/k-cps)
;(trace value-of-smpexp)
;(trace value-of-cstexp)

;res = (num-val -1)
(define p1
  "let a = 1 b = 2 c = 0
   in let d = -(b, a) e = c
      in letrec add1(x1) = let t = -(x1, 10) in if zero?(t) then x1 else (add1 -(x1, -1))
                sub1(y1) = if zero?(y1) then y1 else (sub1 -(y1, 1))
         in if zero?(d)
            then (sub1 e)
            else (add1 e)")

