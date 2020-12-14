use "calendar.sml";
(* Test for is_order*)
val is_older_test_1 = is_older((2020, 1, 1), (2021, 1, 1)) = true
val is_older_test_3 = is_older((2020, 1, 1), (2020, 1, 1)) = false
val is_older_test_4 = is_older((2020, 1, 2), (2020, 1, 1)) = false
val is_older_test_5 = is_older((2020, 2, 1), (2020, 1, 1)) = false
val is_older_test_6 = is_older((2020, 2, 1), (2020, 1, 1)) = false
val is_older_test_7 = is_older((5, 4, 4), (4, 5, 4)) = false

(* Test for number_in_month *)
val number_in_month_test_1 = number_in_month([(2020, 1, 1)], 1) = 1
val number_in_month_test_2 = number_in_month([(2020, 2, 1), (2020, 2, 2)], 1) = 0
val number_in_month_test_3 = number_in_month([(2020, 2, 1), (2020, 1, 2)], 1) = 1
val uumber_in_month_verbose = number_in_month([(2020, 2, 1), (2020, 2, 15), (2020, 2, 16), (2020, 1, 15)], 2) = 3
val number_in_month_test_empty = number_in_month([], 1) = 0
(* Test for number_in_months*)
val number_in_months_test_basic = number_in_months([(2020, 1, 1), (1, 2, 2020)], [1, 2]) = 2
val number_in_months_test_empty = number_in_months([(2020, 1, 1), (1, 2, 2020)], []) = 0
val number_in_months_test_zero = number_in_months([(2020, 1, 1), (1, 2, 2020)], [3, 4]) = 0
val number_in_months_test_one = number_in_months([(2020, 1, 1), (1, 2, 2020)], [2]) = 1
val number_in_months_verbose = number_in_months([(1,2,25),(3,5,26),(1,12,29),(3,2,28),(1,2,27),(1,2,25),(6,7,8)], [2]) = 4
(* Test for dates in month *)
val test_dates_in_month_basic = dates_in_month([(2020, 1, 1), (2021, 1, 2)], 1) = [(2020, 1, 1), (2021, 1, 2)]

val test_dates_in_month_false = dates_in_month([(2020, 1, 1), (2021, 1, 2)], 3) = []

(* Test for dates in months*)
val test_dates_in_months_basic = dates_in_months([(2020, 1, 1), (2020, 2, 1)], [1, 2]) = [(2020, 2, 1), (2020, 1, 1)]
val test_dates_in_months_false = dates_in_months([(2020, 1, 1)], [2]) = []
(* test get_nth *)
val test_get_nth_basic = get_nth(["haha", "sml"], 2) = "sml"
val test_get_nth_verbose = get_nth(["haha", "sml", "hahaa", "Yeah", "Good", "ILSML"], 4) = "Yeah"

(* test date_to_string *)
val test_date_to_string = date_to_string((2020, 1, 1)) = "January 1, 2020"
val test_date_to_string_verbose = date_to_string((1,2,25)) = "February 25, 1"

(* test number before reaching sum*)
val test_num_before_sum_basic = number_before_reaching_sum(4, [1, 2, 3, 4]) = 2 
(* test what_month *)
val test_what_month_basic = what_month(23) = 1
val test_what_month_mean = what_month(61) = 3
val test_month_range_basic = month_range(1, 2) = [1, 1]
val test_month_range_verbose = month_range(1, 10) = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]

(* test oldest *)
val test_oldest_basic = valOf (oldest([(2020, 1, 1), (2020, 2, 1)])) = (2020, 1, 1)
val test_oldest_verbose = valOf (oldest([(2020, 1, 1), (2020, 2, 1), (2019, 1, 1)])) = (2019, 1, 1)
(* test cumulative_sum *)
val test_cumulative_sum_basic = cumulative_sum([1, 2, 3]) = [1, 3, 6]
val test_cumulative_sum_verbose = cumulative_sum([1, 2, 3, 5, 6, 7]) = [1, 3, 6, 11, 17, 24]
(* test challenge exercise 1*)
val test_challenge_1_basic = number_in_months_challenge([(1, 1, 2020)], [1, 1, 1]) = 1
val test_challenge_1_verbose = number_in_months_challenge([(2020, 1, 1), (2020, 2, 1)], [1, 1, 1, 2, 2, 2]) = 2
(* test challenge exercise 2*)
val test_challenge_2_basic = dates_in_months_challenge([(2020, 1, 1)], [1, 1, 1]) = [(2020, 1, 1)]
val test_challenge_2_verbose = dates_in_months_challenge([(2020, 1, 1), (2020, 2, 1)], [1, 1, 1, 2, 2, 2]) = [(2020, 2, 1), (2020, 1, 1)]

val test_month_range_verbose_2 = month_range(31, 32) = [1, 2] 

val test_number_before_reaching_verbose = number_before_reaching_sum(1, [2]) = 0
val test_number_before_reaching_verbose1 = number_before_reaching_sum(5, [3,1,2]) = 2

val test_what_month_verbose = what_month(60) = 3
val test_what_month_verbose1 = what_month(32) = 2
