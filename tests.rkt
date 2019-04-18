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
       [transparent t = Int
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

(define p5
  "module to-int-maker
     interface
       ((ints : [opaque t
                 zero : t
                 succ : (t -> t)
                 pred : (t -> t)
                 is-zero : (t -> Bool)]) =>
        [to-int : (from ints take t -> Int)])
       body
         module-proc (ints : [opaque t
                              zero : t
                              succ : (t -> t)
                              pred : (t -> t)
                              is-zero : (t -> Bool)])
           [to-int = let z? = from ints take is-zero
                     in let p = from ints take pred
                        in letrec to-int(x : from ints take t) -> Int =
                             if (z? x) then 0 else -((to-int (p x)), -1)
                           in to-int]
   module ints1
     interface
       [opaque t
        zero : t
        succ : (t -> t)
        pred : (t -> t)
        is-zero : (t -> Bool)]
     body
       [type t = Int
        zero = 0
        succ = let succ = proc(x : t) -(x, -1) in succ
        pred = let pred = proc(x : t) -(x,  1) in pred
        is-zero = let is-zero = proc(x : t) if zero?(x) then zero?(0) else zero?(1) in is-zero]
   module to-int1
     interface
       [opaque t
        zero : t
        succ : (t -> t)
        pred : (t -> t)
        is-zero : (t -> Bool)]
     body
       (to-int-maker ints1)
   let z = from to-int1 take zero
       a = (from to-int1 take succ z)
   in a")

