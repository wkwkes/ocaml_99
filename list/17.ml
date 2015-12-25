let split l n =
    let rec sub res n = function
        [] -> (List.rev res, [])
      | x::xs -> if n = 0 then (res, (x::xs)) else sub (x::res) (n - 1) xs
    in sub [] n l;;
