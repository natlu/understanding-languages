exception NoAnswer

(*** section 1 ***)

(* problem 1 *)
val only_capitals = List.filter(fn s => Char.isUpper(String.sub(s,0)))

(* problem 2 *)
val longest_string1 = foldl (fn (s,acc) => if String.size acc >= String.size s then acc else s) ""

(* problem 3 *)
val longest_string2 = foldl (fn (s,acc) => if String.size acc > String.size s then acc else s) ""

(* problem 4 *)
fun longest_string_helper f = foldl (fn (s,acc) => if f(String.size s, String.size acc) then s else acc) ""
val longest_string3 = longest_string_helper (fn (a,b) => a > b)
val longest_string4 = longest_string_helper (fn (a,b) => a >= b)

(* problem 5 *)
val longest_capitalized = longest_string3 o only_capitals

(* problem 6 *)
val rev_string = String.implode o rev o String.explode


(*** section 2 ***)

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


(*** section 3 ***)

infix |>
fun x |> f = f x

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

(* problem 9 *)

(* part a *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(* part b *)
val count_wild_and_variable_lengths = g (fn _ => 1) String.size

(* part c *)
fun count_some_var (s,p) = g (fn _ => 0) (fn x => if x = s then 1 else 0) p

(* problem 10 *)
fun check_pat p =
    let
        fun get_var_strings p =
            case p of
                Variable v => [v]
              | TupleP ps => foldl (fn (p, acc) => acc@(get_var_strings p)) [] ps
              | ConstructorP (_, p) => get_var_strings p
              | _ => []
        fun has_repeats ss =
            case ss of
                [] => false
              | s::ss' => case List.exists (fn x => x = s) ss' of
                              true => true
                            | false => has_repeats ss'
    in
        p |> get_var_strings |> has_repeats |> not
    end

(* problem 11 *)
fun match (v, p) =
    case (v, p) of
        (_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                 then all_answers (fn (v, p) => match(v, p)) (ListPair.zip(vs, ps))
                                 else NONE
      | (Constructor (s2,v), ConstructorP (s1,p)) => if s1 = s2 then match(v, p) else NONE
      | (_, _) => NONE

(* problem 12 *)
fun first_match v ps =
    SOME (first_answer (fn p => match (v, p)) ps)
    handle NoAnswer => NONE
