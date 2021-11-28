(* For variable bindings, it is type checked and then the
   static environment is extended *)
(* The type checking occurs before the program runs *)
(* How to typecheck an expression? ML looks it up in the
   static environment, if it doesn't find it, fail, else
   check types depending on expression rules e.g.
   a + b expects a and b to be certain types. If a is int
   and b is a bool, then it doesn't typecheck i.e. it
   doesn't have a type *)

(* For variable bindings, the extression is evaluated and then
   the dynmaic environment is extended *)
(* Evaluation occurs as the program runs *)
(* no need to check for variable existence in the dynamic
   environment as in ML, it'll only run if the program typehecks *)

val x = 2;
(* static environment: x: int *)
(* dynamic environment: x --> 1 *)

val y = 2;
(* static environment: x: int, y: int *)
(* dynamic environment: x --> 1, y --> 2 *)

val z = x + y;
(* static environment: x: int, y: int, z: int *)
(* dynamic environment: x --> 1, y --> 2, z --> 3 *)

val abs_of_z = if z < 0 then 0 - z else z;
(* the then and else must be the same type, which is int *)
(* static environment: x: int, y: int, z: int, abs_of_z: int *)
(* dynamic environment: x --> 1, y --> 2, z --> 3, abs_of_z --> 3 *)


(* how does ML type check functions? *)
(* fun x0 (x1 : t1, ..., xn : tn) = e *)
(* 1. It can use what's in the static environment from earlier bindings *)
(* 2. It can use the arguemnts with their types. i.e. t1, ..., tn *)
(* 3. It can use the type of the function, x0, itself for recursion *)

(* NOTE: the types defined in the function arguments are not added to
         the static environment. They are only in-scope for e, the expression. *)
