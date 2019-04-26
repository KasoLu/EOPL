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
  "class c1 extends object
     field a
     method init() begin set a = 100; print(10) end
     method m1() print(a)
   class c2 extends object
     field a
     method init() begin set a = 200; print(20) end
     method m1() print(a)
   class s1 extends c1, c2
     field a
     method init()
       begin
         super init();
         named-send c2 self init();
         set a = 300;
         print(30)
       end
   class ss1 extends s1
     field a
     method init()
       begin
         super init();
         set a = 400;
         print(40)
       end
   let oss1 = new ss1()
   in send oss1 m1()")
