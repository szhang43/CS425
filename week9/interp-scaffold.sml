(* Given datatypes *)
datatype term = AST_ID of string
              | AST_NUM of int
              | AST_TRUE
              | AST_FALSE
              | AST_SUCC
              | AST_PRED
              | AST_ISZERO
              | AST_IF of term * term * term
              | AST_FUN of string * term
              | AST_APP of term * term
              | AST_LET of string * term * term;

datatype result = RES_ERROR of string
                | RES_NUM of int
                | RES_TRUE
                | RES_FALSE
                | RES_SUCC
                | RES_PRED
                | RES_ISZERO
                | RES_FUN of (string * term)

(* Helpers previously given, and slightly modified.
   You will need to modify these when using static scope
   and call-by-name *)
exception UnboundID of string
exception Unimplemented

datatype env = Env of (string -> result);
fun emptyenvFun (x : string) : result = (print (x ^ " is unbound"); raise (UnboundID x));
val emptyenv = Env emptyenvFun

(* This is the environment from the typechecker
   Note that it returns string -> result so you will
   have to tag the return with Env *)
(* Env -> string -> result -> string -> result *)
fun update (Env e) (x : string) (v: term) y = if x = y then v else e y;

(* An alternative update function that returns an 
   already tagged environment*)
(* Env -> string -> result -> Env *)
(* fun update (Env e) (x : string) (v: result) = (Env (fn y => if x = y then v else e y)); *)

(* Use this to pull values from the environment, i is the variable string that you wish to find in the env *)
fun lookup (Env e, i) = e i;

(* Interpret function *)
(* Implement this *)
fun interp (env, (AST_ID i))            = interp(env, lookup(env, i))
  | interp (env, (AST_NUM n))           = RES_NUM n
  | interp (env, AST_TRUE)              = RES_TRUE
  | interp (env, AST_FALSE)             = RES_FALSE
  | interp (env, AST_SUCC)              = RES_SUCC
  | interp (env, AST_PRED)              = RES_PRED
  | interp (env, AST_ISZERO)            = RES_ISZERO
  | interp (env, (AST_IF (b, e1, e2)))  = (case interp(env, b) of
                                             RES_TRUE => interp(env, e1)
                                             | RES_FALSE => interp(env, e2)
                                             | _ => RES_ERROR "haha"
                                          )

  | interp (env, (AST_FUN (i, b)))      = RES_FUN(i, b)
  | interp (env, (AST_APP (f, p)))      =  (case interp(env, f) of 
                                             RES_SUCC => (case interp(env, p) of 
                                                RES_NUM n => RES_NUM(n + 1)
                                                | _ => RES_ERROR "You messed up!"
                                             )
                                             | RES_PRED => (case interp(env, p) of 
                                                RES_NUM n => RES_NUM (n - 1)
                                                | _ => RES_ERROR "You messed up!"
                                             )
                                             | RES_ISZERO => (case interp(env, p) of 
                                                RES_NUM n => if n = 0 then RES_TRUE else RES_FALSE
                                                | _ => RES_ERROR "You messed up!"
                                                )
                                             | RES_FUN(i , b) => interp(update env i p, b)
                                             )

  | interp (env, (AST_LET (i, e1, e2))) = interp(update env i e1, e2)
(* pass by value is eager, pass by name is lazy *)

(* Tests *)
(* ------------ *)

(* Dynamic vs static scope *)
(* let x = 0 
   in let f = fn z => x 
        in let x = 100 
            in f x *)
val test0 = AST_LET ("x", AST_NUM 0, 
               AST_LET("f",AST_FUN("z",AST_ID "x"),
                  AST_LET("x",AST_NUM 100,
                    AST_APP(AST_ID "f", AST_ID "x"))));

val res_test0 = interp (emptyenv, test0);

(* Call-By-Value vs Call-By-Name *)
(* (fn x => 42) ((fn x => x x) (fn x => x x)) *)
(*
val test1 =
    AST_APP
        (AST_FUN ("x",AST_NUM 42)
        ,AST_APP
             (AST_FUN ("x",AST_APP(AST_ID "x",AST_ID "x"))
             ,AST_FUN ("x",AST_APP(AST_ID "x",AST_ID "x"))));

val res_test1 = interp (emptyenv, test1);
*)

(* ((fn x => fn y => x 42) (fn y => y)) 0 *)
val test2 =
    AST_APP
        (AST_APP
             (AST_FUN ("x",AST_FUN ("y", AST_APP (AST_ID "x",AST_NUM 42))) ,AST_FUN ("y",AST_ID "y"))
        ,AST_NUM 0)

val res_test2 = interp (emptyenv, test2);

OS.Process.exit(OS.Process.success);
