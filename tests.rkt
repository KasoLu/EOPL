; res = (num-val 1)
(define p1
  "let x = 0
   in letrec
      even()
        = if zero?(x) then 1 else begin set x = -(x,1); (odd) end
      odd()
        = if zero?(x) then 0 else begin set x = -(x,1); (even) end
      in begin set x = 13; (odd) end")

; res = (num-val 12)
(define p2
  "let f = proc(x)
             proc(y)
               begin
                 set x = -(x, -1);
                 -(x, y)
               end
   in ((f 44) 33)")

; res = (num-val 12)
(define p3
  "let times4 = 0
   in begin
       set times4 = proc(x) if zero?(x) then 0 else -((times4 -(x,1)), -4);
       (times4 3)
      end")

; res = (num-val 73)
(define p4
  "let x = 0
   in let mut = mutex()
      in let incr_x = proc(id) proc(id par_id)
                        begin
                          print(-(id, -100));
                          print(-(par_id, -200));
                          wait(mut);
                          set x = -(x, -1);
                          signal(mut)
                        end
         in begin
              spawn((incr_x 100));
              spawn((incr_x 200));
              spawn((incr_x 300))
            end")
