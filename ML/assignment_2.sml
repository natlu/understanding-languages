
(* if you use this function to compare two strings, then you avoid several of
   the functions in problem 1 having polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* problem 1 *)

(* part a *)
fun all_except_option (s, ss) =
    let fun aux (ss, acc) =
            case ss of
                [] => []
              | hss::tss => if same_string(hss, s)
                            then acc @ tss
                            else aux(tss, hss::acc)
    in
        let val res = aux(ss, [])
        in
            case res of
                [] => NONE
              | _ => SOME res
        end
    end

(* part b *)
fun get_substitutions1 (subs, s) =
    case subs of
        [] => []
      | hsubs::tsubs => let val tail_val = get_substitutions1(tsubs, s)
                        in
                            case all_except_option(s, hsubs) of
                                NONE => [] @ tail_val
                              | SOME x => x @ tail_val
                        end

(* part c *)
fun get_substitutions2 (subs, s) =
    let fun aux (subs, acc) =
            case subs of
                [] => acc
              | hsubs::tsubs =>
                case all_except_option(s, hsubs) of
                    NONE => aux(tsubs, acc)
                  | SOME x => aux(tsubs, x@acc)
    in
        aux(subs, [])
    end

(* part d *)
fun similar_names (subs, {first=f,middle=m,last=l}) =
    let
        fun gen_names (names) =
            case names of
                [] => []
              | hnames::tnames => {first=hnames,middle=m,last=l} :: gen_names(tnames)
        val names = f::get_substitutions1(subs, f)
    in
        gen_names(names)
    end




(* assume that Num is always used with 2, 3, ..., 10 *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* problem 2 *)
