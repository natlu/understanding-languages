(* function bodies can use any bindings in scope *)

(* with first class functions, functions can be passed around
   so what is the scope? *)

(* the scope is where the function was defined
   NOT where it was called *)
(* this is lexical scope *) (* as opposed to dynamic scope *)

val x = 1
fun f y = x + y
val x = 2
val z = f 10

(* z will evaluate to 1 + 10 = 11 *)


(* closures *)

(* how does ML evaluate functions using old environments?
   it keeps track of it *)

(* a function has 2 parts:
   1. the code and
   2. the environment from when the function was defined *)
(* this pair of parts is called a function closure *)
