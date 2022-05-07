(* Q1. Write a function that takes two dates and evaluates to true or false.
 It evaluates to true if the first argument is a date that comes before the second argu.
 If same, return false.
 test1 = is_older ((1,2,3),(2,3,4)) = true *)
fun is_older (x:int*int*int, y:int*int*int) = 
    if (#1 x) < (#1 y)
    then true
    else
	if (#1 x) = (#1 y) andalso (#2 x) < (#2 y) 
	then true	
	else
	    if (#1 x) = (#1 y) andalso (#2 x) = (#2 y) andalso (#3 x) < (#3 y)
	    then true
	    else false
		
	    

(* Q2. a function that takes a list of dates and a month and returns how many dates in the list are in the given month.
 test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1 *)
fun number_in_month (dates: (int*int*int) list, month: int)=
    if null dates
    then 0
    else if (#2 (hd dates)) = month
    then
	let
	    val flag = 1 + number_in_month(tl dates, month)
	in
	    flag
	end
    else
	number_in_month((tl dates), month)

(* Q3. Write a function that takes a list of dates and a list od months
 and returns the number of dates in the list of dates that r in any of the months in the list of months
 test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3 *)
fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else 
	if null (tl months)
	then number_in_month(dates, hd months)
	else
	    number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* Q4. A function that takes a list of dates and a month and
 returns a list holding the dates from the argument list of dates that r in the month.
 The returned list should contain dates in the order they were originally given.
 test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)] *)
								
fun dates_in_month (xs : (int * int * int) list, n : int) =
    if null xs
    then []
    else 
	if #2 (hd xs) = n
	then (hd xs)::dates_in_month(tl xs, n)
	else dates_in_month(tl xs, n)

(* Q5. function that takes a lost of dates and a list of months and returns a list holding the dates from the
 argument list of dates that are in any of the months in the list of months
 test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)] *)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else
	if null (tl months)
	then dates_in_month(dates, hd months)
	else
	    dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* Q6. function that takes a list of strings and an int n and return the nth element of the list
 where the head of the list is 1st.
 test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there" *)
fun get_nth (xs : string list, n : int) =
    if n = 1
    then hd xs
    else get_nth(tl xs, n-1)

(* Q7. function that takes a date and returns a string of the form January 20. 2013.
 Use the operator ^ for concatenating strings and the livrary function Int.toString for converting an int to a string.
 test7 = date_to_string (2013, 6, 1) = "June 1, 2013" *)
fun date_to_string (date : (int * int * int) ) =
    get_nth(["January", "February", "March", "April", "May", "June", "July",
             "August", "September", "October", "November"], #2 date) 
    ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

						       

(* Q8. function that takes an int called sum, which u can assume is positive, and an int list, which u can
 assume contains all positive numbers, and return an int.
 test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3 *)
fun number_before_reaching_sum (sum : int, xs : int list) =
    let
	fun number_before_helper(count : int, counter : int, xs : int list) =
	    if (hd xs + counter) >= sum
	    then count
	    else number_before_helper(count + 1, counter + hd xs, tl xs)
    in
	number_before_helper(0, 0, xs)
    end


(* Q9. takes a day of year and returns what month that day is in.
Use a list holding 12 integers and your answer to previous problem.
 test9 = what_month 70 = 3 *)
fun what_month (day : int) =
    number_before_reaching_sum(day, [31, 28, 31, 30, 31, 30, 31, 
				     31, 30, 31, 30, 31]) + 1;

(* Q10. function that takes two day of the year day1 and day2 and returns what month that day is an int list
[month of day1, month of day1+1, .. to month of day2].
  test10 = month_range (31, 34) = [1,2,2,2] *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1, day2)


(* Q11. takes a list of dates and evaluate to an (int*int*int) option.
 It evaluate to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.
 test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31) *)
fun oldest (xs : (int * int * int) list ) =
    if null xs
    then NONE
    else
	let
	    fun oldest_helper(xs : (int * int * int) list, date : (int * int * int)) =
		if null xs
		then true
		else 
		    if is_older(date, hd xs)
		    then oldest_helper(tl xs, date)
		    else false
	    fun oldest_helper_2(xs : (int * int * int) list) =
		if oldest_helper(tl xs, hd xs)
		then hd xs
		else oldest_helper_2(tl xs)
	in
	    SOME (oldest_helper_2(xs))
	end

	
	
	
