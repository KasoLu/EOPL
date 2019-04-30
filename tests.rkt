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
     field Int f1
     method Int init() print(10)
   let oc1 = new C1()
   in begin 
        fieldref oc1 f1;
        fieldset oc1 f1 = 20;
        fieldref oc1 f1
      end")
