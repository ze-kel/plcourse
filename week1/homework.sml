
fun list_product(xs: int list) =
    if null xs
    then 1
    else hd xs * list_product(tl xs)


fun is_older(d1: int * int * int, d2: int * int * int) = 
    if(#1 d1 <> #1 d2) then #1 d1 < #1 d2
    else 
    if(#2 d1 <> #2 d2) then #2 d1 < #2 d2
    else 
    if(#3 d1 <> #3 d2) then #3 d1 < #3 d2 else false


fun number_in_month(l: (int * int * int) list, month: int) = 
    if null l
    then 0
    else
        let
            val currentM = hd l
            val currentVal = if #2 currentM = month then 1 else 0
        in
            currentVal + number_in_month(tl l, month)
        end

fun number_in_months(l: (int * int * int) list, months: int list) = 
    if null months
    then 0
    else
        number_in_month(l, hd months) + number_in_months(l, tl months)

fun dates_in_month(l: (int * int * int) list, month: int) = 
    if null l
    then []
    else
        let 
            val curDate = hd l
            val doMatch = #2 curDate = month
        in
        if
            doMatch
        then
            curDate :: dates_in_month(tl l, month)
        else
            dates_in_month(tl l, month)
        end


fun dates_in_months(l: (int * int * int) list, months: int list) =
    if null months
    then []
    else dates_in_month(l, hd months) @ dates_in_months(l, tl months)


fun get_nth(strings: string list, number: int) =
    if number <= 1
    then hd strings
    else get_nth(tl strings, number - 1)


val monthsArr = ["January","February","March","April","May","June","July","August","September","October","November","December"]


fun date_to_string(date: (int * int * int)) =
    let
        val month = get_nth(monthsArr, #2 date)
        val year = Int.toString(#1 date)
        val day = Int.toString(#3 date)
    in
        month ^ " " ^ day ^ ", " ^ year
    end


fun number_before_reaching_sum(sum: int, nums: int list) =
    let 
        fun number_before_reaching_sum_count(sum: int, nums: int list, els: int) =
            if sum - hd nums <= 0
            then els
            else
            number_before_reaching_sum_count(sum - hd nums, tl nums, els+1)
    in
    number_before_reaching_sum_count(sum, nums, 0)
    end


val daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun what_month(day: int) =
    number_before_reaching_sum(day, daysInMonth) + 1

fun month_range(day1: int, day2:int) =
    if day1 > day2
    then []
    else
        what_month(day1) :: month_range(day1+1, day2)


fun oldest(l: (int * int * int) list) =
    if null l
    then NONE
    else
    let
        fun oldestInternal(l: (int * int * int) list, maxD: (int * int * int)) =
        if null l
        then maxD
        else 
        let 
            val curHead = hd l
            val oldestD = if is_older(curHead, maxD) then curHead else maxD
        in
            oldestInternal(tl l, oldestD)
        end
    in
    SOME (oldestInternal(tl l, hd l))
    end
