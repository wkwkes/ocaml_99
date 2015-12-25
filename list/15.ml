open List

let replicate l n =
    let rec sub res n =
        if n = 0 then res else sub ((@) res l) (n-1)
    in sub [] n;;
