(* ML does not support mutation *)

(* consider the function below which appends two lists together *)

fun append (xs : int list, ys : int list) =
    if null xs
    then ys
    else hd (xs) :: append (tl(xs), ys)

val x = [2,4]
val y = [5,3,1]
val z = append(x,y)


(* here, z will alias y, it doesn't create an entirely new list *)
(* if mutation was allowed, then a mutation on z may effect y*)
(* this is not a concern for ML as it does not allow mutation and
   so whether it does createa a new list or not doesn't matter *)
	      
(* other language such as Java allows for mutation and so keeping
   track of these aliases is important.
   to prevent unwanted mutations, programmers would have to use
   copy variable before exposing it to an API so that people
   cannot access the original variable *)


			
