(* datatype 'a option = NONE || Some of 'a *)
(* 
    Ex.
    Dejunctive type or none
    If we have 5; 
        then we have some 5;    
                        int option
*)

type 'a enf = string -> 'a option

val empty = fn _ => NONE

fun lookup (f, x) = f x
fun extend (f, k : string, x) =
      fn k' => if (k = k') then SOME x else f k'

datatype prop = Var of string | Or of prop*prop | And of prop*prop | Not of prop | If of prop*prop;

type assignment = bool env
(* 
    satisfy : prop * bool env -> (bool env) option 
        Determines whether there is a way to make the given proposition true, given 
        an aassignment specifying truth
        values for the variables we have seen so far. Returns some assignment if one 
        exists, and otherwise returns NONE. 
    
    Ex. Proposition Value 
    p v q -> is p or value q true? We don't have assignments for p or q yet     
                                            so we have to make assignments. 
        so: 
            p |- true 
            provide that p is true. 

            in an environment that p is false and q is false, then 
            p v q can not be true so we return NONE. 

            Suppose that in some other environment that p is true but 
            q is false. We return Some.
*)

fun satisfy (Var v, asn) = 
    (case lookup(asn, v) of 
        NONE        => (* We can make the formula consisting of jsut 
                            this variable) true by making the variable true 
                        *)
                        SOME(extend (asn, v, true))
        | SOME true  => (* 
                        We return the current assignment that says p is bounded to true 
                        *)
                        SOME asn
        | SOME false => (*
                            We alreadt decided this variable must be false, so we cant not the 
                            given formula true
                        *)
                        NONE)
    | satisfy (And(p1, p2), asn) =
        (* Try to find an assignment satisfying both p1 and p2. *)
        (case satisfy(p1, asn) of
            SOME asn' => satisfy(p2, asn') (* asn' is the new env variable i get from satisfy (p1, asn) *)
            | NONE    => NONE)
    (*
        Ex. 
            p1 ^ p2 
            satisfy(p1, empty) => (p1, true)
            satisfy(p2, (p1, true))
                (p2, true), (p1, true)

        For `p1 ^ p2` (AND operation):
        - If both `p1` and `p2` are satisfiable, the final environment supports:
            `(p1, true)` and `(p2, true)`.
    *)

    |satisfy (Or(p1, p2), asn) = 
        (* Try to satisfy p1. If that is impossible, satisfy p2 *)
        (case satisfy(p1, asn) of 
            SOME asn' => SOME asn' (* can not guarentee that this is the only to satisfy the formula *)
            |NONE => satisfy(p2, asn'))

    |satisfy (Not p, asn) = falsify (p, asn)
    |satisfy (If(p1, p2), asn) = satisfy(Or(Not p1, p2), asn)
        

(*
    Concept of backtracking is missing which is my (p V q) ^ ] p is not satisfied.
*)
