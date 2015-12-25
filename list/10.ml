let encode l =
    let rec enc res = function
        [] -> List.rev res
      | x::xs -> let (num, str, ret) = sub 1 x xs in enc ((num, str)::res) ret 
    and sub num str = function
        [] -> (num, str, [])
      | y::ys -> if y = str then sub (num + 1) str ys else (num, str, y::ys)
    in enc [] l;;
