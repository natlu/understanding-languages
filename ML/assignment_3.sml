exception NoAnswer

datatype pattern = Wildcard
		             | Variable of string
		             | UnitP
		             | ConstP of int
		             | TupleP of pattern list
		             | ConstructorP of string * pattern

datatype valu = Const of int
	            | Unit
	            | Tuple of valu list
	            | Constructor of string * valu

fun g f1 f2 p =
    let
	      val r = g f1 f2
    in
	      case p of
	          Wildcard          => f1 ()
	        | Variable x        => f2 x
	        | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	        | ConstructorP(_,p) => r p
	        | _                 => 0
    end

(* infix |> *)


(*** part 1 ***)

(* problem 1 *)
val only_capitals = List.filter(fn s => Char.isUpper(String.sub(s,0)))

(* problem 2 *)
val longest_string1 = foldl (fn (s,acc) => if String.size acc >= String.size s then acc else s) ""

(* problem 3 *)
val longest_string2 = foldl (fn (s,acc) => if String.size acc > String.size s then acc else s) ""

(* problem 4 *)
fun longest_string_helper f = foldl (fn (s,acc) => if f(String.size acc, String.size s) then acc else s) ""
val longest_string3 = longest_string_helper (fn (a,b) => a >= b)
val longest_string4 = longest_string_helper (fn (a,b) => a > b)

(* problem 5 *)
val longest_capitalized = longest_string3 o only_capitals

(* problem 6 *)
val rev_string = String.implode o rev o String.explode


(*** part 2 ***)

(* problem 7 *)
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs' => case f x of
                      SOME v => v
                    | NONE => first_answer f xs'

(* problem 8 *)
fun all_answers f xs =
    let
        fun helper acc xs =
            case xs of
                [] => SOME acc
              | x::xs' => case f x of
                              NONE => NONE
                            | SOME v => helper (v@acc) xs'
    in
        helper [] xs
    end


(**** for the challenge problem only ****)
(*
datatype typ = Anything
	           | UnitT
	           | IntT
	           | TupleT of typ list
	           | Datatype of string
*)
