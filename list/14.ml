let duplicate l = 
    let rec sub res = function
        [] -> res
      | x::xs -> sub (x::x::res) xs
    in List.rev (sub [] l);;
