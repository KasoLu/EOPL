(load "main.rkt")

(define p1
  "interface Tree
     method Int sum()
     method Bool equal(t : Tree)
   class Interior-node extends object implements Tree
     field Tree left
     field Tree right
     method Void init(l : Tree, r : Tree)
       begin
         set left = l; set right = r
       end
     method Tree getleft() left
     method Tree getright() right
     method Int sum() +(send left sum(), send right sum())
     method Bool equal(t : Tree)
       if instanceof t Interior-node
       then if send left equal(send cast t Interior-node getleft())
            then send right equal(send cast t Interior-node getright())
            else zero?(1)
       else zero?(1)
   class Leaf-node extends object implements Tree
     field Int value
     method Void init(v : Int) set value = v
     method Int sum() value
     method Int getvalue() value
     method Bool equal(t : Tree)
       if instanceof t Leaf-node
       then zero?(-(value, send cast t Leaf-node getvalue()))
       else zero?(1)
   let o1 = new Interior-node(
              new Interior-node(
                new Leaf-node(3),
                new Leaf-node(4)),
              new Leaf-node(5))
   in list(send o1 sum(), if send o1 equal(o1) then 100 else 200)")

(define p2
  "interface Summable
     method Int sum()
   interface Tree
   class List extends object implements Summable
     field list-val : Listof Int
     method Void init() set list-val = empty-list Int
     method Int sum()
       letrec Int loop(list : Listof Int, sum : Int) =
         if null?(list)
         then sum
         else (loop cdr(list) +(sum, car(list)))
   class BinTree extends object implements Tree, Summable
     field l : Tree
     field r : Tree
     method Void init(left : Tree, right : Tree)
       begin set l = left; set r = right end
     method Int sum()
       if instanceof l Summable
       then 
         if instanceof r Summable
         then +(send l sum(), send r sum())
         else 0
       else 0
   class BinTreeLeaf extends object implements Tree, Summable
     field v : Int
     method Void init(x : Int) set v = x
     method Int sum() v
   class GeneralTree extends object implements Tree, Summable
     field list : Listof Tree
     method Void init(list : Listof Tree) set list = list
     method Int sum()
       letrec loop(l : Listof Summable, sum : Int)
       if null?(list)
         sum
         (loop cdr(l) +(sum, send car(l) sum()))")

