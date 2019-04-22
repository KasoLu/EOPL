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
     public method init()
       print(10)
     private method m1()
       print(11)
     protected method m2()
       begin
         print(12);
         send self m1()
       end
     public method m3() 
       begin
         print(13);
         send self m2()
       end
   class s1 extends c1
     public method init()
       print(20)
     public method m1()
       begin
         print(21)
       end
     private method m3()
       begin
         print(23);
         send self m2()
       end
   let oc1 = new c1(), os1 = new s1()
   in begin
        send oc1 m3();
        send os1 m1()
      end")
