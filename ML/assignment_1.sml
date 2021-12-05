
(* 1. takes 2 dates. Evaluates to true if d1 > d2, else false *)
fun is_older (d1 : int*int*int, d2 : int*int*int) =
    if #1 d1 <> #1 d2
    then #1 d1 < #1 d2
    else
        if #2 d1 <> #2 d2
        then #2 d1 < #2 d2
        else #3 d1 < #3 d2

(* 2. takes list of dates and a month.
   Returns how many dates in that list are in the month *)
fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else
        let val tl_ans = number_in_month(tl dates, month)
        in
            if #2 (hd dates) = month
            then 1 + tl_ans
            else 0 + tl_ans
        end

(* 3. how many dates in list of dates contain any of the months*)
fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* 4. *)
fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else
        let val tl_ans = dates_in_month(tl dates, month)
        in
            if #2 (hd dates) = month
            then (hd dates) :: tl_ans
            else tl_ans
        end

(* 5. *)
fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* 6. *)
fun get_nth (strings : string list, n : int) =
    if n <> 1
    then get_nth(tl strings, n - 1)
    else hd strings
            
(* 7. *)
   
  
