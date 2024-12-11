fun fact n = if n = 1 then 1 else n * fact (n-1);
                                fn x => n * x; (* continuation *)

fun fact_iter(n, a) = if n = 1 then a else 
                                    fact_iter (n-1, n*a);

(* 
    tail recursion : when I call a function, 
    I have to active the new actiation frame, with 
    tail recurison you can reuse your frame. 
    suppose you have a loop: at the second iterations, 
    you don't have to redefine the variables, you reuse it
*)

(* continuation passing style *)

(*
    ex. 
        (2 + 1) + (3 + 4)
        eval (2 + 1) first, but what is the program counter after that operation?
        I can represent continuation with a function :
        use m to represent the result of (2 + 1)
        fn m => m + (3 + 4) we would call this the continuous of (2 + 1)
        points to the rest of the computation.
*)


fun fact_cps(n, k) = if n = 1 then k 1 else 
                                    fact_cps (n-1, fn x => k (n * x));

(*
    new parameter represents a continuation 
*)
fact_iter(5, 1);
fact_cps(5, fn x => x);

(fact 20) + 3;

(fact_cps(20, fn x => x + 3)) (* how can i involve fact_cps? the k is the continuation of
                                    eval (fact 20). Caller wants to add 3 *)

(* Question: what was the 9 example ?*)