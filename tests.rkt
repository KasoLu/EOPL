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
  "class C1 extends object
     method Int init() print(10)
   class C2 extends object
     method Int init() print(20)
   class S1 extends C1
     method Int init() print(11)
   class S2 extends C2
     method Int init() print(21)
   let oc1 = new C1(), oc2 = new C2(), os1 = new S1(), os2 = new S2()
   in begin
        cast oc1 object;
        cast oc1 C1;
        cast oc1 S1;
        cast oc2 C2;
        cast oc2 S2
      end")
