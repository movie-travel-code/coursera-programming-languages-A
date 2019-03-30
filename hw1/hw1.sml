(*Assignment problem 1*)
fun is_older(first : int*int*int, second : int*int*int) = 
  let
    val days_map = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    fun get_nth(index: int, months: int list) = 
      if index = 1
      then hd months
      else get_nth(index - 1, tl months) 
  in
    let
      fun days(year: int, month: int, day: int) = 
        year * 365 + get_nth(month, days_map) + day
    in
      days first < days second
    end
  end

(*Assignment problem 2*)
fun number_in_month(dates : (int*int*int) list, month : int) = 
  if null dates 
  then 0
  else 
    let val tl_num = number_in_month(tl dates, month)
    in if #2 (hd dates) = month
      then 1 + tl_num
      else tl_num
    end

(*Assignment problem 3*)
fun number_in_months(dates : (int*int*int) list, month : int list) = 
  if null dates
  then 0
  else if null month
        then 0
        else (* Ok, we have two nonempty lists *)
          number_in_month(dates, hd month) + number_in_months(dates, tl month)

(*Assignment problem 4*)
fun dates_in_month(dates : (int*int*int) list, month : int) = 
  if null dates
  then []
  else
  let 
    val x = dates_in_month(tl dates, month)
  in
    if #2 (hd dates) = month
    then (hd dates) :: x
    else x
  end

(*Assignment problem 5*)
fun dates_in_months(dates : (int * int * int) list, months : int list) = 
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(*Assignment problem 6*)
(*TODO Test the corner cases?*)
fun get_nth(l: string list, index: int) = 
  if index = 1
  then hd l
  else get_nth(tl l, index - 1)

(*Assignment problem 7*)
fun date_to_string(date: int * int * int) = 
  let 
    val months = ["January ", "February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "]
    val month_str = get_nth(months, #2 date)
    val day_str = Int.toString(#3 date)
    val year_str = Int.toString(#1 date)
    val con_str = ", "
  in 
    month_str ^ day_str ^ con_str ^ year_str
  end

(*Assignment problem 8*)
fun number_before_reaching_sum(sum: int, numbers: int list) = 
  if sum - hd numbers > 0
  then number_before_reaching_sum(sum - hd numbers, tl numbers) + 1
  else 0

(*Assignment problem 9*)
fun what_month(day: int) = 
  let 
    val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, months) + 1
  end

(*Assignment problem 10*)
fun month_range(day1: int, day2: int) = 
  if day1 > day2
  then []
  else 
    let 
      val h = what_month(day1)
      val t = month_range(day1 + 1, day2)
    in
      h :: t
    end

(*Assignment problem 11*)
fun oldest(dates: (int * int * int) list) = 
  if null dates 
  then NONE
  else
    if null (tl dates)
    then SOME (hd dates)
    else (*Now the dates at list has two entries*)
      let 
        val h = hd dates
        (*t cannot be none*)
        val t = oldest(tl dates)
      in 
        if is_older(h, valOf t)
        then SOME h
        else t
      end