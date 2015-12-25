
let rec pack l =
    let rec p ans = function 
        [] -> List.rev(ans)
      | a::rest -> let (ret, lis) = sub [a] a rest in
                     p (ret::ans) lis 
    and sub res str = function
        [] -> (res, [])
      | a::rest -> if a = str then sub (a::res) str rest
                              else (res, a::rest)
    in p [] l;;

