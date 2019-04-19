(load "main.rkt")

(define p1
  "class c1 extends object
     field x
     field y
     method init()
       begin
         print(10);
         set x = 11;
         set y = 12
       end
     method m1()
       begin
         print(11);
         +(x, 10)
       end
     method m2()
       begin
         print(12);
         send self m3()
       end

   class c2 extends c1
     field y
     method init()
       begin
         super init();
         print(20);
         set y = 22
       end  
     method m1(u, v)
       begin
         print(21);
         +(x, u);
         +(y, v)
       end
     method m3()
       begin
         print(23);
         +(x, 20)
       end
    
   class c3 extends c2
     field x
     field z
     method init()
       begin
         super init();
         print(30);
         set x = 31;
         set z = 32
       end
     method m3()
       begin
         print(33);
         +(x, 30)
       end

   let o3 = new c3()
   in begin print(100); send o3 m1(7, 8) end")

(define p2
  "class queue extends object
   field buf
   field cnt
   method init(c)
     begin
       set buf = list();
       set cnt = c
     end
   method empty?()
     begin
       send self count();
       if null?(buf) then zero?(0) else zero?(1)
     end
   method enqueue(x)
     begin
       send self count();
       letrec loop(buf, x) = 
         if null?(buf) then list(x) else cons(car(buf), (loop cdr(buf) x))
       in set buf = (loop buf x)
     end
   method dequeue()
     begin
       send self count();
       if send self empty?()
       then 0
       else let elem = car(buf) in begin set buf = cdr(buf); elem end
     end
   method count()
     set cnt = +(cnt, 1)
   method get-count()
     cnt
   let q = new queue(0)
   in begin 
        print(send q enqueue(1));
        print(send q enqueue(2));
        print(send q dequeue());
        print(send q dequeue());
        print(send q dequeue());
        print(send q get-count());
        let p = new queue(send q get-count())
        in begin
             print(send p enqueue(1));
             print(send p enqueue(2));
             print(send p dequeue());
             print(send p dequeue());
             print(send p dequeue());
             print(send p get-count())
           end
      end")
