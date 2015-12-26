
let str_of_lis lis =
    let rec sub res = function
        [] -> res
      | x::xs -> sub (res ^ x) xs
    in sub "" lis;;

let gray n = 
    let rec sub m res =
        if m = n then res
        else let f l b = List.map (fun x -> b::x) l in
            sub (m + 1) ((f res "0") @ (f res "1"))
    in List.map str_of_lis (sub 0 [[]]);;
