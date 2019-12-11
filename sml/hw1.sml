val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
val months_strings = ["January", "February", "March", "April", "May", "June",
 "July", "August", "September", "October", "November", "December"]

exception OutOfRange;

fun get_element_by_index (l: int list, index: int) =
  if index < 0 orelse null l
  then raise OutOfRange
  else
    if index = 0
    then hd l
    else get_element_by_index(tl l, index - 1)

fun get_year(date: int * int * int) = #1 date
fun get_month(date: int * int * int) = #2 date
fun get_day(date: int * int * int) = #3 date

fun date_is_reasonable(date: int * int * int) = 
  let
    fun is_reasonable_year(year: int) = 
      year > 0
    fun is_reasonable_month(month: int) =
      month >= 1 andalso month <= 12
    fun is_reasonable_day(year: int, month: int, day: int) = 
      let
        fun get_days_in_month(month: int, year: int) =
          let
            fun is_leap_year(year: int) = 
              (year mod 400) = 0 orelse ((year mod 4 = 0) andalso (year mod 100 <> 0))
            val days = get_element_by_index(days_in_month, month - 1)
          in
            if month = 2 andalso is_leap_year(year)
            then days + 1
            else days
          end
      in
        day > 0 andalso day <= get_days_in_month(month, year)
      end
  in
    is_reasonable_year(get_year(date)) andalso
    is_reasonable_month(get_month(date)) andalso
    is_reasonable_day(date)
  end

fun get_day_of_year(date: int * int * int) =
  let
    fun sum_up_first_n_elements_of_list(l: int list, n: int) =
      if n = 0 orelse null l
      then 0
      else hd l + sum_up_first_n_elements_of_list(tl l, n - 1)
  in
    sum_up_first_n_elements_of_list(days_in_month, get_month(date) - 1) +
    get_day(date)
  end

fun is_older(date1: int * int * int, date2: int * int * int) =
  get_year(date1) < get_year(date2) orelse
  (get_year(date1) = get_year(date2) andalso get_day_of_year(date1) < get_day_of_year(date2))

fun number_in_month(dates: (int * int * int) list, month: int) =
  if null dates
  then 0
  else 
    if get_month(hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

fun number_in_months(dates: (int * int * int) list, months: int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int * int * int) list, month: int) = 
  if null dates
  then []
  else
    if get_month(hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month) 
    else dates_in_month(tl dates, month)

fun dates_in_months(dates: (int * int * int) list, months: int list) = 
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings: string list, n: int) = 
  if n < 1 orelse null strings
  then raise OutOfRange
  else
    if n = 1
    then hd strings
    else get_nth(tl strings, n - 1)

fun date_to_string(date: int * int * int) = 
  if not (date_is_reasonable(date))
  then "Unreasonable date"
  else get_nth(months_strings, get_month(date)) ^ " "
    ^ Int.toString(get_day(date)) ^ ", " ^ Int.toString(get_year(date))

fun number_before_reaching_sum(sum: int, xs: int list) =
  if sum <= (hd xs)
  then 0
  else 1 + number_before_reaching_sum(sum - (hd xs), tl xs)

fun what_month(day_of_year: int) =
  if day_of_year <= 0 orelse day_of_year > 365
  then raise OutOfRange
  else number_before_reaching_sum(day_of_year, days_in_month) + 1

fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates: (int * int * int) list) = 
  if null dates
  then NONE
  else
    let
      fun unsafe_oldest(dates: (int * int * int) list) =
        if null (tl dates)
        then hd dates
        else
          let
            val oldest_in_tail = unsafe_oldest(tl dates)
          in
            if is_older(hd dates, oldest_in_tail)
            then hd dates
            else oldest_in_tail
          end
    in
      SOME(unsafe_oldest(dates))
    end

fun distinct(xs: int list) =
  let
    fun distinct_internal(xs: int list, to_exclude: int list) =
      let
        fun contains(ys: int list, y: int) = 
          not (null ys) andalso (hd ys = y orelse contains(tl ys, y))
      in
        if null xs
        then []
        else
          if contains(to_exclude, hd xs)
          then distinct_internal(tl xs, to_exclude)
          else hd xs :: distinct_internal(tl xs, hd xs :: to_exclude)
      end
  in
    distinct_internal(xs, [])
  end

fun number_in_months_challenge(dates: (int * int * int) list, months: int list) = 
  number_in_months(dates, distinct(months))

fun dates_in_months_challenge(dates: (int * int * int) list, months: int list) = 
  dates_in_months(dates, distinct(months))

  
