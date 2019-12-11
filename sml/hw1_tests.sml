use "hw1.sml";

val test_date_is_reasonable =
  date_is_reasonable(1, 2, 3) = true andalso
  date_is_reasonable(0, 2, 3) = false andalso
  date_is_reasonable(1, 0, 3) = false andalso
  date_is_reasonable(1, 13, 3) = false andalso
  date_is_reasonable(1, 1, 3) = true andalso
  date_is_reasonable(1, 12, 3) = true andalso
  date_is_reasonable(1, 1, 0) = false andalso
  date_is_reasonable(1, 1, 32) = false andalso
  date_is_reasonable(1, 1, 31) = true andalso
  date_is_reasonable(1, 2, 29) = false andalso
  date_is_reasonable(2012, 2, 29) = true andalso
  date_is_reasonable(2000, 2, 29) = true andalso
  date_is_reasonable(1900, 2, 29) = false

val test_day_of_year =
  get_day_of_year(1, 2, 2) = 33 andalso
  get_day_of_year(2010, 12, 31) = 365

val test_is_older = 
  is_older((1998, 12, 1), (2010, 1, 2)) = true andalso
  is_older((1998, 1, 1), (1998, 1, 2)) = true andalso
  is_older((1998, 1, 1), (1998, 1, 1)) = false andalso
  is_older((1999, 1, 1), (1998, 1, 1)) = false

val test_number_in_month = 
  number_in_month([], 10) = 0 andalso
  number_in_month([(1998, 10, 1)], 10) = 1 andalso
  number_in_month([(1, 10, 2), (1, 10, 3)], 10) = 2 andalso
  number_in_month([(1, 10, 2), (2, 9, 3), (1, 10, 3)], 10) = 2 andalso
  number_in_month([(1, 10, 2), (2, 9, 3), (1, 10, 3)], 9) = 1

val test_number_in_months = 
  number_in_months([], []) = 0 andalso
  number_in_months([(1998, 10, 1)], [10]) = 1 andalso
  number_in_months([(1, 10, 2), (1, 10, 3)], [10]) = 2 andalso
  number_in_months([(1, 10, 2), (2, 9, 3), (1, 10, 3)], [10]) = 2 andalso
  number_in_months([(1, 10, 2), (2, 9, 3), (1, 10, 3)], [9]) = 1 andalso
  number_in_months([(1, 10, 2), (2, 9, 3), (1, 10, 3)], [9, 10]) = 3


val test_dates_in_month = 
  dates_in_month([], 10) = [] andalso
  dates_in_month([(1998, 10, 1)], 10) = [(1998, 10, 1)] andalso
  dates_in_month([(1, 10, 2), (1, 10, 3)], 10) = [(1, 10, 2), (1, 10, 3)] andalso
  dates_in_month([(1, 10, 2), (2, 9, 3), (1, 10, 3)], 10) = [(1, 10, 2), (1, 10, 3)] andalso
  dates_in_month([(1, 10, 2), (2, 9, 3), (1, 10, 3)], 9) = [(2, 9, 3)]

val test_dates_in_months = 
  dates_in_months([], [10]) = [] andalso
  dates_in_months([(1998, 10, 1)], [10]) = [(1998, 10, 1)] andalso
  dates_in_months([(1, 10, 2), (1, 10, 3)], [10]) = [(1, 10, 2), (1, 10, 3)] andalso
  dates_in_months([(1, 10, 2), (2, 9, 3), (1, 10, 3)], [10]) = [(1, 10, 2), (1, 10, 3)] andalso
  dates_in_months([(1, 10, 2), (2, 9, 3), (1, 10, 3), (1, 2, 3)], [10, 9]) =
  [(1, 10, 2), (1, 10, 3), (2, 9, 3)]

val test_get_nth =
  get_nth(["1", "2", "3"], 1) = "1" andalso
  get_nth(["1", "2", "3"], 2) = "2" andalso
  get_nth(["1", "2", "3"], 3) = "3"

val test_date_to_string =
  date_to_string(0, 0, 0) = "Unreasonable date" andalso
  date_to_string(2019, 12, 10) = "December 10, 2019" andalso
  date_to_string(1, 3, 8) = "March 8, 1" 

val test_number_before_reaching_sum =
  number_before_reaching_sum(10, [1, 2, 3, 4, 5]) = 3 andalso
  number_before_reaching_sum(1, [1, 2, 3, 4, 5]) = 0 andalso
  number_before_reaching_sum(2, [1, 2, 3, 4, 5]) = 1 andalso
  number_before_reaching_sum(5, [1, 2, 3, 4, 5]) = 2 

val test_what_month  =
  what_month(1) = 1 andalso
  what_month(33) = 2 andalso
  what_month(256) = 9 andalso
  what_month(365) = 12

val test_oldest = 
  oldest([]) = NONE andalso
  oldest([(1, 1, 1)]) = SOME((1, 1, 1)) andalso
  oldest([(2, 2, 2), (1, 1, 1)]) = SOME((1, 1, 1)) andalso
  oldest([(2, 2, 2), (1, 1, 1), (1, 6, 12)]) = SOME((1, 1, 1))
