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
     field num
     method init() set num = 1
     method m1() print(10)
   class s1 extends c1
     field num
     method init() set num = 2
     method m1() print(20)
   class s2 extends s1
     field num
     method init() set num = 3
     method m1() print(30)
   let o = new s2()
   in begin
        named-send c1 o m1();
        named-send s1 o m1();
        named-send s2 o m1();
        print(named-fieldref s1 o num);
        named-fieldset s1 o num = 30;
        print(named-fieldref s1 o num)
      end")

