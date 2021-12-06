
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

(* 6. Don't need to consider the case where there are too few elements *)
fun get_nth (strings : string list, n : int) =
    if n <> 1
    then get_nth(tl strings, n - 1)
    else hd strings

(* 7. *)
fun date_to_string (date : int*int*int) =
    let
        val month_strings = [
            "January", "February", "March", "April", "May", "June", "July",
            "August", "September", "October", "November", "December"
        ]
    in
        get_nth(month_strings, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(* 8. Assume the entire list sums to more than the passed in value;
      it is okay for an exception to occur if this is not the case *)
fun number_before_reaching_sum (sum : int, ints : int list)=
    if sum - hd ints <= 0
    then 0
    else 1 + number_before_reaching_sum(sum - hd ints, tl ints)

(* 9. *)
fun what_month (day_of_year : int)=
    let
        val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day_of_year, days_in_month) + 1
    end


(* 10. *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(* 11. *)
fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else
        let
            fun oldest_nonempty (dates : (int*int*int) list) =
                if null (tl dates)
                then hd dates
                else
                    let val tl_ans = oldest_nonempty(tl dates)
                    in
                        if is_older(hd dates, tl_ans)
                        then hd dates
                        else tl_ans
                    end
        in
            SOME (oldest_nonempty dates)
        end

