(*
case expr0 of
    pattern1 => expr1
  | pattern2 => expr2
    ...
  | patternn => exprn
*)

(* the pattern must match, and where constructors are involved, it must also come from the same constructor *)
(* it is used to find the variant you have *)
