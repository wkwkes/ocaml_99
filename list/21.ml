let insert_at inp n l = 
    let rec sub res num = function
        [] -> if num = n then List.rev (inp::res) else List.rev res
      | x::xs -> if num = n then sub (inp::res) (num + 1) (x::xs)
                            else sub (x::res) (num + 1) xs
    in sub [] 0 l;;
