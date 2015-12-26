let min_fac n =
    let rec sub m =
        if n mod m = 0 then m else sub (m + 1)
    in sub 2;;

let factors' n = 
    let rec sub m res =
        if m = 1 then List.rev res
        else let num = min_fac m 
             in sub (m / num) (num::res)
    in sub n [];;

let factors n = 
    let rec sub res str num = function
        [] -> List.rev ((str, num)::res)
      | x::xs -> if x = str then sub res str (num + 1) xs
                            else sub ((str, num)::res) x 1 xs
    in let lis = factors' n in
    sub [] (List.hd lis) 1 (List.tl lis);;

let rec exp x n = 
    let rec sub res n = 
        if n = 0 then res
        else sub (res * x) (n - 1)
    in sub 1 n;;

let phi_improved n = 
    let lis = factors n in
    let rec sub res = function
        [] -> res
      | x::xs ->  let (p, m) = x in sub (res * ((p - 1) * exp p (m - 1))) xs 
    in sub 1 lis;;
