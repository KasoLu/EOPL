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
  "class point extends object
     field x
     field y
     method init(init-x, init-y)
       begin
         set x = init-x;
         set y = init-y
       end
     method getx() x
     method gety() y
     method similarpoints(pt)
       if equal?(send pt getx(), x)
       then equal?(send pt gety(), y)
       else zero?(1)
   class colorpoint extends point
     field color
     method init(init-x, init-y, init-color)
       begin
         set x = init-x;
         set y = init-y;
         set color = init-color
       end
     method set-color(c)
       set color = c
     method get-color()
       color
     method similarpoints(pt)
       if super similarpoints(pt)
       then 
         if instance-of pt colorpoint
         then equal?(send pt get-color(), color)
         else zero?(1)
       else zero?(1)
   let p1 = new point(10, 20), p2 = new point(30, 40),
       p3 = new colorpoint(10, 20, 123), p4 = new colorpoint(10, 20, 234)
   in begin
        print(send p1 similarpoints(p2));
        print(send p1 similarpoints(p3));
        print(send p3 similarpoints(p3));
        print(send p3 similarpoints(p4))
      end")
