
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

(* part a *)
fun card_color card =
    case card of
        (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red
      | (Spades, _) => Black

(* part b *)
fun card_value card =
    case card of
        (_, Ace) => 11
      | (_, Num n) => n
      | _ => 10

(* part c *)
fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
      | hcs::tcs => if hcs = c
                    then tcs
                    else hcs::remove_card(tcs, c, e)

(* part d *)
fun all_same_color cs =
    case cs of
        [] => true
      | _::[] => true
      | hcs::(tcs::rest) => (card_color hcs = card_color tcs andalso all_same_color (tcs::rest))

(* part e *)
fun sum_cards cs =
    let fun f (cs, acc) =
            case cs of
                [] => acc
             | c::cs' => f(cs', card_value c + acc)
    in
        f(cs, 0)
    end

(* part f *)
fun score (cs, goal) =
    let
        val hand_sum = sum_cards cs
        val divider = if all_same_color cs then 2 else 1
    in
        if hand_sum > goal
        then (3 * (hand_sum - goal)) div divider
        else (goal - hand_sum) div divider
    end

(* part g *)
fun officiate (cs, ms, goal) =
    let
        fun state (hand, moves, cards) =
            case (moves, cards) of
                ([], _) => score(hand, goal)
              | (_, []) => score(hand, goal)
              | (Draw::moves', card::cards') => if sum_cards(card::hand) > goal
                                                then score(card::hand, goal)
                                                else state(card::hand, moves', cards')
              | ((Discard c)::moves', cards) => state(remove_card(hand, c, IllegalMove), moves', cards)
    in
        state([], ms, cs)
    end

