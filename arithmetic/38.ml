
let is_prime n =
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
