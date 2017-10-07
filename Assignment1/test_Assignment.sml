
use "Assignment1.sml";

val list_dates = [(1, 5, 6), (1, 4, 3),
    (1, 3, 4), (3, 5, 8), (12, 5, 18),
    (11, 5, 4), (3, 2, 8), (3, 2, 8)]
val test1 = number_in_month(list_dates, 12)
val list_dates2 = []
val test2 = number_in_month(list_dates2, 12)
val test3 = number_in_months(list_dates, [4,3,2,12])
val test4 = dates_in_month(list_dates, 2)




val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
