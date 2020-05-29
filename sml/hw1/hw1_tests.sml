use "hw1.sml";

val test_reasonable_date =
  reasonable_date(1, 2, 3) = true andalso
  reasonable_date(0, 2, 3) = false andalso
  reasonable_date(1, 0, 3) = false andalso
  reasonable_date(1, 13, 3) = false andalso
  reasonable_date(1, 1, 3) = true andalso
  reasonable_date(1, 12, 3) = true andalso
  reasonable_date(1, 1, 0) = false andalso
  reasonable_date(1, 1, 32) = false andalso
  reasonable_date(1, 1, 31) = true andalso
  reasonable_date(1, 2, 29) = false andalso
  reasonable_date(2012, 2, 29) = true andalso
  reasonable_date(2000, 2, 29) = true andalso
  reasonable_date(1900, 2, 29) = false

val test_is_older = 
  is_older((1998, 2, 7), (2000, 1, 1)) = true andalso
  is_older((1998, 2, 7), (1998, 3, 1)) = true andalso
  is_older((1998, 2, 7), (1998, 2, 8)) = true andalso
  is_older((1998, 2, 7), (1998, 2, 7)) = false andalso
  is_older((1998, 2, 7), (1998, 2, 6)) = false andalso
  is_older((1998, 2, 7), (1998, 1, 1)) = false andalso
  is_older((1998, 2, 7), (1997, 1, 1)) = false


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
  
val test_month_range  = month_range (31, 34) = [1,2,2,2]

val test_oldest = 
  oldest([]) = NONE andalso
  oldest([(1, 1, 1)]) = SOME((1, 1, 1)) andalso
  oldest([(2, 2, 2), (1, 1, 1)]) = SOME((1, 1, 1)) andalso
  oldest([(2, 2, 2), (1, 1, 1), (1, 6, 12)]) = SOME((1, 1, 1))

val test_challenge_1 = number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31)
                                           ,(2011,4,28)],[2,3,4,2,3])
               = 3


