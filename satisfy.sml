fun satisfy (Var v, asn, k) =
  (case lookup (asn, v) of
       NONE        => k (extend (asn, v, true))  (* Extend assignment with v = true *)
     | SOME true   => k asn                     (* Variable already true in assignment *)
     | SOME false  => false)                    (* Contradiction: variable assigned false *)

| satisfy (And(p1, p2), asn, k) =
    satisfy (p1, asn, fn asn' => 
      satisfy (p2, asn', fn asn'' => k asn''))  (* Recursively satisfy p1 and p2 *)

| satisfy (Or(p1, p2), asn, k) =
    let
      val res = satisfy (p1, asn,
                         fn asn' => if (k asn') then true
                            else satisfy (p2, asn, fn asn'' => k asn''))
                satisfy (p2, asn, k)
            
    in
      case res of
           SOME asn' => k asn'                (* If p1 is satisfiable, return its result *)
         | NONE      => satisfy (p2, asn, k) (* Otherwise, try satisfying p2 *)
    end;

(* Example helper functions for satisfiability *)
val satisfiable = fn p => satisfy (p, empty, fn _ => true);
val falsifiable = fn p => satisfy (Not p, empty, fn _ => true);


(*
    Seg jump 
    long jump 
*)