
let is_prime n =
    if n = 1 then false else 
    let v = int_of_float (sqrt (float_of_int n)) + 1 in
    let rec sub div =
        if div >= v then true
                   else (if n mod div = 0 then false else sub (div+1))
    in sub 2;;

let goldbach n = 
    let rec sub num = 
        if is_prime num && is_prime (n - num) then (num, n - num)
        else sub (num + 1)
    in sub 1;;

let goldbach' n lim = 
    let rec sub num =
        if num >= n then (0, 0) else
        if is_prime num && is_prime (n - num) && num > lim && (n - num) > lim then (num, n - num)
        else sub (num + 1)
    in sub 1;;


let goldbach_list l u =
    let rec sub n res =
        if n < l then res
        else if n mod 2 = 0 
             then sub (n - 1) ((n, goldbach n)::res)
             else sub (n - 1) res
    in sub u [];;

let goldbach_limit l u lim =
    let rec sub n res =
        if n < l then res
        else if n mod 2 = 0
             then let x = goldbach' n lim in
             (if x = (0, 0) then sub (n - 1) res
                  else sub (n - 1) ((n, x)::res))
             else sub (n - 1) res
    in sub u [];;

