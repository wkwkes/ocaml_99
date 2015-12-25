let drop l n = 
    let rec sub res num = function
        [] -> res
      | x::xs -> if num = n then sub res 1 xs else sub (x::res) (num + 1) xs
    in List.rev (sub [] 1 l);;
