let min_fac n =
    let rec sub m =
        if n mod m = 0 then m else sub (m + 1)
    in sub 2;;

let factors n = 
    let rec sub m res =
        if m = 1 then List.rev res
        else let num = min_fac m 
             in sub (m / num) (num::res)
    in sub n [];;
