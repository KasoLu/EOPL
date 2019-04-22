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
     field c1-field
     method init()
       set c1-field = 10
   
   class s1 extends c1
     field c1-field
     method init()
       set c1-field = 20
     method m1()
       begin
         superfieldset c1-field = 100;
         superfieldref c1-field
       end 
     method m2()
       c1-field
     method m3()
       superfieldref c1-field

   class s2 extends s1
     field c1-field
     method init()
       set c1-field = 30
     method m1()
       super m1()
     method m2()
       c1-field
     method m3()
       super m3()
     
   let o = new s2()
   in begin
        print(send o m2());
        print(send o m1());
        print(send o m3())
      end")

