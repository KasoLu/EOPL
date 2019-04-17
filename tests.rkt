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
  "module mybool
     interface
       [opaque t
        true : t
        false : t
        and : (t -> (t -> t))
        not : (t -> t)
        to-bool : (t -> Bool)]
     body
       [type t = Int
        true = 1
        false = 0
        and = proc(x : t) proc(y : t)
          if zero?(x) then false else y
        not = proc(x : t)
          if zero?(x) then true else false
        to-bool = proc(x : t)
          if zero?(x) then zero?(1) else zero?(0)]
   let true = from mybool take true
       false = from mybool take false
       and = from mybool take and
   in and")

(define p4
  "module mybool
     interface
       [opaque t
        true : t
        false : t
        and : (t -> (t -> t))
        not : (t -> t)
        to-bool : (t -> Bool)]
     body
       [type t = Int
        false = 0
        true  = 1
        not = proc(x : t)
          if zero?(x) then true else false
        and = proc(x : t) proc(y : t)
          if zero?(x) then false else y
        to-bool = proc(x : t)
          if zero?(x) then zero?(1) else zero?(0)]
   let true = from mybool take true
       false = from mybool take false
       and = from mybool take and
   in and")
