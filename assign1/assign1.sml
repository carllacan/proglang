
(* function 1 *)
fun is_older (date2 : int*int*int, date1 : int*int*int) =
    if (#1 date1 > #1 date2) then true
    else 
	if (#1 date1 < #1 date2) then false
	else  (*if the years are equal*)
	    if (#2 date1 > #2 date2) then true	
	    else
		if (#2 date1 < #2 date2) then false
		else  (*if the months are equal*)
		    if (#3 date1 > #3 date2) then true
		    else false (* if the days are equal or 1 < 2*)

(* function 2 *)
fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates then 0 

    else 
	if #2 (hd dates) = month then 1 + number_in_month(tl dates, month)
	else number_in_month(tl dates, month) 

(* function 3 *)
fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* function 4 *)
fun dates_in_month  (dates : (int*int*int) list, month : int) =
    if null dates then []
    else 
	if #2 (hd dates) = month then [hd dates]@dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)

(* function 5 *)
fun dates_in_months  (dates : (int*int*int) list, months : int list) =
    if null months then []
    else dates_in_month(dates, hd months)@dates_in_months(dates, tl months)

(* function 6 *)
fun get_nth ( strings : string list, n : int)=
    if n=1 then hd strings else get_nth(tl strings, n-1)

(* function 7 *)
fun date_to_string (date : int*int*int)=
    get_nth(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)

(* function 8 *)
fun number_before_reaching_sum (sum : int, nums : int list)=
    if sum <= hd nums then 0
    else 1+number_before_reaching_sum(sum - hd nums, tl nums)

(* function 9 *)

fun what_month (day: int)=
    1+number_before_reaching_sum(day, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])

(* function 10 *)
fun month_range (day1 : int, day2 : int)=
    if (day1 > day2) then []
    else [what_month(day1)]@month_range(day1+1, day2)

(* function 11 *)
fun oldest (dates : (int*int*int) list)=
    if null dates
    then NONE 
    else 
	let val tl_ans = oldest(tl dates)
	in
	    if isSome tl_ans 
	       andalso is_older(valOf tl_ans, hd dates)
	    then  tl_ans
	    else SOME (hd dates)
	end

(* function 13 *)
fun reasonable_date (date : int*int*int) =
    
    (#1 date > 0) andalso (#3 date > 0) andalso
    (#2 date > 0) andalso #2 date <= 12 andalso 
    case #2 date of
	1  =>  #3 date <=31
      | 2  => (case ((#1 date mod 400 = 0 orelse #1 date mod 4 = 0) andalso #1 date mod 100 <> 0) of
                      true =>  ( #3 date <= 29)
                    | false => ( #3 date <= 28))
      | 3  =>  #3 date <=31
      | 4  => #3 date <=30
      | 5  =>  #3 date <=31
      | 6  => #3 date <=30
      | 7  => #3 date <=31
      | 8  => #3 date <=31
      | 9  => #3 date <=30
      | 10 => #3 date <=31
      | 11 => #3 date <=30
      | 12 => #3 date <=31
			    
