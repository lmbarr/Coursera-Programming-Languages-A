fun alternate(list_ints: int list) =
    if null list_ints
    then 0
    else hd list_ints - alternate(tl list_ints)

fun min_max(list_ints: int list) =
    if null list_ints
    then (0, 0)
    else
    let val ans = min_max(tl list_ints)
    in
        if hd list_ints > #1 ans
        then (hd list_ints,)
        else
    end
