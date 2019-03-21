(*Assignment problem 1*)
fun is_older(first : int*int*int, second : int*int*int) = 
  if #1 first < #1 second
  then true (* Give a quick answer based on the year*)
  else if #1 first > #1 second
      then false
  else if #2 first < #2 second
      then true
      else if #2 first > #2 second
          then false
      else if #3 first < #3 second
          then true
          else if #3 first = #3 second
              then false (* if two dates have same year, month, day, then return false*)
              else false

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
    if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

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
