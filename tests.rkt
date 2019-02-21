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

;res = (num-val 2)
(define p3
  "let a = 0 b = 1
   in let f = proc(x) +(x, 1)
          g = proc(x) -(x, 1)
      in +(a, (f a), b, (g b))")

;res = (num-val 12)
(define p4
  "let a = newref(1) b = newref(2)
   in let c = setref(a, 10)
      in +(deref(a), deref(b))")

;res = (num-val 12)
(define p5
  "let a = newref(1) b = newref(2)
   in begin setref(a, 10); +(deref(a), deref(b)) end")
