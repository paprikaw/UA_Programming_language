(* Date has the formats: year, month, day *)
fun is_older (date_1 : int*int*int, date_2 : int*int*int)=
    let
        val date_1_day = 365 * (#1 date_1) + (#2 date_1) * 31 + (#3 date_1)
        val date_2_day = 365 * (#1 date_2) + (#2 date_2) * 31 + (#3 date_2)
    in
        date_1_day < date_2_day
    end

fun number_in_month (dates : (int*int*int) list, month : int)=
    if null dates 
    then
        0
    else if #2 (hd dates) = month
    then
        1 + number_in_month(tl dates, month)
    else
        number_in_month(tl dates, month)

fun number_in_months (dates : (int*int*int) list, months : int list)=
    if null months then
        0
    else
        number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int)=
    if null dates
    then
        []
    else if month = #2 (hd dates)
    then
        (hd dates) :: dates_in_month(tl dates, month) 
    else
        dates_in_month(tl dates, month)



fun dates_in_months(dates : (int*int*int) list, months : int list)=
    let
        fun get_dates_list_from_months(dates : (int*int*int) list, months : int list)=
            if null months
            then
                [] 
            else
                dates_in_month(dates, hd months) 
        val date_list = get_dates_list_from_months(dates, months)
    in
        if null months
        then
            []
        else if null date_list 
        then
            dates_in_months(dates, tl months)
        else
            dates_in_months(dates, tl months)@date_list
    end

fun get_nth(list_string : string list, index : int)=
    if index = 1
    then
        hd list_string
    else
        get_nth(tl list_string, index - 1)

fun date_to_string(dates : int*int*int)=
    let
        val month_list = ["January", "February",  "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
        val month = get_nth(month_list, (#2 dates))
        val day = Int.toString(#3 dates)
        val year = Int.toString(#1 dates)
    in
        month ^ " " ^ day ^ ", " ^ year
    end
       

fun number_before_reaching_sum(sum : int, num_list : int list)=
    let 
        fun recurr_helper(num_list : int list, curr_sum : int)=
            if null num_list orelse curr_sum >= sum then
                0
            else
                1 + recurr_helper(tl num_list, curr_sum + (hd num_list))
    in
        recurr_helper(tl num_list, hd num_list)
    end

fun what_month(day : int)=
    let
        val month_list = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, month_list) + 1
    end

fun month_range(day_1 : int, day_2 : int)=
    if day_1 > day_2 then []
    else if day_1 = day_2
    then
        [what_month(day_1)]
    else
        [what_month(day_1)]@month_range(day_1 + 1, day_2)


fun oldest(dates : (int*int*int) list)=
    let
        fun recurr_oldest(dates : (int*int*int) list, oldest_date : int*int*int)=
            if null dates then
                oldest_date
            else if is_older(hd dates, oldest_date)
            then
                recurr_oldest(tl dates, hd dates)
            else
                recurr_oldest(tl dates, oldest_date)
    in
        if null dates then
            NONE
        else
            SOME (recurr_oldest(dates, hd dates))
    end
fun cumulative_sum(num_list : int list)=
    let
        fun recurr_cumulative_sum(num_list : int list, sum : int)=
            if null num_list then
                [sum]
            else
                sum :: recurr_cumulative_sum(tl num_list, sum + (hd num_list))
    in
        recurr_cumulative_sum(tl num_list, hd num_list)
    end
        

fun remove_duplicates(months : int list)=
    let
        fun filter_list(remain_list : int list, curr_month : int)=
            if null remain_list then
                []
            else if curr_month = hd remain_list then
                filter_list(tl remain_list, curr_month)
            else
                (hd remain_list) :: filter_list(tl remain_list, curr_month)
    in
        if null months then
            []
        else
        hd months :: remove_duplicates(filter_list(tl months, hd months))
    end

fun number_in_months_challenge (dates : (int*int*int) list, months : int list)=
    number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list)=
    dates_in_months(dates, remove_duplicates(months))

