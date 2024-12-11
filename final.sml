(* HW 3 *)

(* Question 1 *)
(* fun listmap f[] = []
    | listmap f (x::xs) = f x :: listmap f xs; *)

datatype tree = NIL | LEAF of int | CONS of (tree * tree);

fun treeMap  f NIL = NIL
    | treeMap f (LEAF a) = LEAF (f a)
    | treeMap f (CONS (t1, t2)) = CONS (treeMap f t1, treeMap f t2);
