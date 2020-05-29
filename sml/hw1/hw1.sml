fun get_year (date: int * int * int) = #1 date
fun get_month (date: int * int * int) = #2 date
fun get_day (date: int * int * int) = #3 date

(*1*)
fun is_older (d1: int * int * int, d2: int * int * int) = 
  get_year(d1) < get_year(d2) orelse 
  (get_year(d1) = get_year(d2) andalso get_month(d1) < get_month(d2)) orelse
  (get_month(d1) = get_month(d2) andalso get_day(d1) < get_day(d2))

(*2*)
fun number_in_month (dates: (int * int * int) list, month: int) =
  if null dates
  then 0
  else number_in_month(tl dates, month) + (if get_month(hd dates) = month then 1 else 0)

(*3*)
fun number_in_months (dates: (int * int * int) list, months: int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(*4*)
fun dates_in_month (dates: (int * int * int) list, month: int) =
  if null dates
  then []
  else
    let
      val tl_result = dates_in_month(tl dates, month)
    in
      if get_month(hd dates) = month
      then hd dates :: tl_result
      else tl_result
    end

(*5*)
fun dates_in_months (dates: (int * int * int) list, months: int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(*6*)
exception OutOfRange
fun get_nth (xs: string list, n: int) =
  if n < 1 orelse null xs
  then raise OutOfRange
  else
    if n = 1
    then hd xs
    else get_nth(tl xs, n - 1)

(*7*)
fun date_to_string (date: int * int * int) =
  let
    val month_names = ["January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"]
  in
    get_nth(month_names, get_month(date)) ^ " " ^ Int.toString(get_day(date)) ^
    ", " ^ Int.toString(get_year(date))
  end

(*8*)
fun number_before_reaching_sum (sum: int, xs: int list) =
  if hd xs >= sum
  then 0
  else 1 + number_before_reaching_sum (sum - hd xs, tl xs)

(*9*)
fun what_month (day_of_year: int) =
  let
    val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day_of_year, days_in_month) + 1
  end

(*10*)
fun month_range (day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

(*11*)
fun oldest (dates: (int * int * int) list) =
  if null dates
  then NONE
  else
    let
      fun oldest_notempty(dates: (int * int * int) list) =
        if null (tl dates)
        then hd dates
        else
          let
            val tl_oldest = oldest_notempty(tl dates)
          in
            if is_older(hd dates, tl_oldest)
            then hd dates
            else tl_oldest
          end
    in
      SOME(oldest_notempty dates)
    end

(*12*)
fun distinct (xs: int list) =
  let
    fun in_list (ys: int list, x: int) =
      not (null ys) andalso (hd ys = x orelse in_list(tl ys, x))

    fun distinct_with_acc (xs: int list, acc: int list) =
      if null xs
      then acc
      else
        if in_list(acc, hd xs)
        then distinct_with_acc(tl xs, acc)
        else distinct_with_acc(tl xs, acc @ [hd xs])
  in
    distinct_with_acc(xs, [])
  end

fun number_in_months_challenge (dates: (int * int * int) list, months: int list) =
  number_in_months(dates, distinct(months))

fun dates_in_months_challenge (dates: (int * int * int) list, months: int list) =
  dates_in_months(dates, distinct(months))

(*13*)
fun reasonable_date(date: (int * int * int)) =
  let
    fun reasonable_year (year: int) = year > 0
    fun reasonable_month (month: int) = month >= 1 andalso month <= 12
    fun is_leap_year (year: int) = (year mod 400) = 0 orelse ((year mod 4) = 0
    andalso (year mod 100) <> 0)
    fun get_days_in_month (month: int, leap: bool) =
      let
        val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        fun get_nth (xs: int list, n: int) = 
          if n = 1 then hd xs else get_nth(tl xs, n - 1)
      in
        if month = 2 andalso leap
        then get_nth(days_in_month, month) + 1
        else get_nth(days_in_month, month)
      end
    fun reasonable_day(day: int, month: int, year: int) =
      day >= 1 andalso day <= get_days_in_month(month, is_leap_year(year))
  in
    reasonable_year(get_year(date)) andalso reasonable_month(get_month(date))
    andalso reasonable_day(get_day(date), get_month(date), get_year(date))
  end


