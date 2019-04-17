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
       module-proc (ints : [opaque t
                            zero : t
                            succ : (t -> t)
                            pred : (t -> t)
                            is-zero : (t -> Bool)])
         [from-int = letrec loop(x : Int y : from ints take t) -> from ints take t =
                              if zero?(x) then y else (loop -(x, 1) (succ y))
                     in let func = proc(x : Int y : from ints take t) -> from ints take t
                                     (loop x from ints take zero)
                        in func]
   module from-int-to-ints1
     interface [from-int : (Int -> from ints1 take t)]
     body (from-int-maker ints1)
   module from-int-to-ints2
     interface [from-int : (Int -> from ints2 take t)]
     body (from-int-maker ints2)
   let to-ints1 = from from-int-to-ints1 take from-int
   in let to-ints2 = from from-int-to-ints2 take from-int
      in let ints1-to-int = from ints1 take to-int
         in let ints2-to-int = from ints2 take to-int
            in let two1 = (to-ints1 2)
               in let two2 = (to-ints2 2)
                  in -((ints1-to-int two1), (ints2-to-int two2))")

(define p7
  "module sum-prod-maker
     interface
       ((ints : [opaque t
                 zero : t
                 succ : (t -> t)
                 pred : (t -> t)
                 is-zero : (t -> Bool)])
        => [plus : (from ints take t ->
                     (from ints take t ->
                       from ints take t))
            times : (from ints take t ->
                      (from ints take t ->
                        from ints take t))])
     body
       module-proc(ints : [opaque t
                           zero : t
                           succ : (t -> t)
                           pred : (t -> t)
                           is-zero : (t -> Bool)])
         [plus = letrec loop(x : from ints take t y : from ints take t) -> from ints take t =
                          if (from ints take is-zero y)
                          then x
                          else (loop (succ x) (pred y))
                 in loop
          times = letrec loop(x : from ints take t y : from ints take t) -> from ints take t =
                           if (from ints take is-zero y)
                           then x
                           else (loop (plus x x) (pred y))
                  in loop]")

(define p8
  "module two-times-ints
     interface
       ((ints : [opaque t
                 zero : t
                 succ : (t -> t)
                 pred : (t -> t)
                 is-zero : (t -> Bool)
                 to-int : (t -> Int)])
        => [zero : from ints take t
            succ : (from ints take t -> from ints take t)
            pred : (from ints take t -> from ints take t)
            is-zero : (from ints take t -> Bool)
            to-int : (from ints take t -> Int)])
     body
       module-proc(ints : [opaque t
                           zero : t
                           succ : (t -> t)
                           pred : (t -> t)
                           is-zero : (t -> Bool)])
         [zero = from ints take zero
          succ = letrec loop(x : from ints take t times : Int) -> from ints take t =
                          if zero?(times) then x else (loop (succ x) -(times, 1))]
                 in let entry = proc(x : from ints take t) -> from ints take t
                                  (loop x (from ints take to-int x))
          pred = letrec loop(x : from ints take t times : Int) -> from ints take t =
                          if zero?(times) then x else (loop (pred x) -(times, 1))
                 in let entry = proc(x : from ints take t) -> from ints take t
                                  (loop x (from ints take to-int x))
          is-zero = from ints take is-zero
          to-int = from ints take to-int")

(define p9
  "module equality-maker
     interface
       ((ints : [opaque t
                 zero : t
                 succ : (t -> t)
                 pred : (t -> t)
                 is-zero : (t -> Bool)])
        => [equal : (from ints take t ->
                      (from ints take t ->
                        Bool))])
     body
       module-proc(ints : [opaque t
                           zero : t
                           succ : (t -> t)
                           pred : (t -> t)
                           is-zero : (t -> Bool)])
         [equal = let is-zero = from ints take is-zero
                  in letrec loop(x-succ : from ints take t 
                                 y-pred : from ints take t
                                 x-pred : from ints take t
                                 y-succ : from ints take t) -> Bool
                              if (is-zero x-succ)
                              then if (is-zero y-succ)
                                   then zero?(0)
                                   else zero?(1)
                              else if (is-zero x-pred)
                                   then if (is-zero y-pred)
                                        then zero?(0)
                                        else zero?(1)
                                   else (loop (from ints take succ x-succ)
                                              (from ints take pred y-pred)
                                              (from ints take pred x-pred)
                                              (from ints take succ x-succ))]")
