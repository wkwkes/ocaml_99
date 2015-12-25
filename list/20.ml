let remove_at n l = 
    let rec sub res m = function
        [] -> List.rev res
      | x::xs -> if m = n then sub res (m + 1) xs
                          else sub (x::res) (m + 1) xs
    in sub [] 0 l;;
