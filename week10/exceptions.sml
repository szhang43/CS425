
exception Ex

fun f  x = raise Ex;

((f 0) handle Ex => 99) +
((f 0) handle Ex => 100) ;


let exception Ex
in  (f 0) handle Ex => 99
end;

(* Exceptions in ML are generative. *)

fun f n = let
             exception E
          in  if n = 0 then raise E
                       else  (f (n-1)) handle E => 99
          end;


f 1;

val test2 = (let fun f y = raise X  (* what is the handle after tha function definition? *)
               fun g h = h 1 handle X => 2 (* f is called here ---- why is f called here?*)
            in g f handle x => 4
         end)
         handle x => 6;

(*
You can write a table of exceptions : 
   ex. 
      handle E => e
      | E1 => e1
      | E2 => e2 
   calls the corresponding handle 
*)

(* Exceptions for efficciency *)

fun prod nil = 1
   | prod (x::xs) = x * prod xs ;; 

val test_3 = prod [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 10];

exception Zero; 

fun prod_e nil = 1
   | prod_e (0::xs) = raise Zero (* if the list is zero, raise an exception *)
   | prod_e (x::xs) = x * prod_e xs;

(* uncaught exception E *)


