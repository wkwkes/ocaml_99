let slice l m n =
    let rec sub res i j = function
        [] -> List.rev res
      | x::xs -> if i = 0 then (if j = 0 then List.rev res else sub (x::res) 0 (j - 1) xs)
                          else sub res (i - 1) j xs
    in sub [] m n l;;
