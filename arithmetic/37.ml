
let is_prime n =
    let v = int_of_float (sqrt (float_of_int n)) + 1 in
    let rec sub div =
        if div >= v then true
                   else (if n mod div = 0 then false else sub (div+1))
    in sub 2;;

let all_primes l u =
    let rec sub res n =
        if n < l then res
        else if is_prime n then sub (n::res) (n - 1)
             else sub res (n - 1)
    in sub [] u;;
