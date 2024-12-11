datatype term = 
    I of string
  | B of bool
  | A of term * term
  | L of (string * term * term)
  | R of term

datatype typ =
    BOOL
  | REF

type env = (string * typ) list

exception VariableNotFound of string
exception TypeError of string

fun lookup (env: env, x: string): typ =
  case List.find (fn (y, _) => y = x) env of
      SOME (_, v) => v
    | NONE => raise VariableNotFound ("Variable not found: " ^ x)

fun update (env: env, x: string, v: typ): env =
  (x, v) :: List.filter (fn (y, _) => y <> x) env

fun typeOf (env: env, t: term): typ =
  case t of
      I x => lookup (env, x)
    | B _ => BOOL
    | R e =>
        (case typeOf (env, e) of
            REF => REF
          | _ => raise TypeError "Invalid argument to R: Must be REF")
    | A (e1, e2) =>
        (case typeOf (env, e1) of
            BOOL =>
              (case typeOf (env, e2) of
                  BOOL => BOOL
                | _ => raise TypeError "Invalid arguments, 2nd term must be BOOL")
          | _ => raise TypeError "Invalid arguments, 1st term must be BOOL")
    | L (i, e1, e2) =>
        let
          val t1 = typeOf (env, e1)
          val extendedEnv = update (env, i, t1)
        in
          typeOf (extendedEnv, e2)
        end

val testEnv: env = [("test", BOOL), ("Hello", REF), ("x", BOOL)]

val test1 = I "test"
val test2 = B true
val test3 = R (I "Hello")
val test4 = A(B true, B false)
val test5 = L("x", B false, I "x")

val type1 = typeOf(testEnv, test1)
val type2 = typeOf(testEnv, test2)
val type3 = typeOf(testEnv, test3)
val type4 = typeOf(testEnv, test4)
val type5 = typeOf(testEnv, test5)

val new = update(testEnv, "y", BOOL);

testEnv