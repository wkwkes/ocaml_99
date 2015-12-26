let rec gcd a b = 
        if a < b then gcd b a else
            let c = a mod b in if c = 0 then b else gcd b c;; 

let phi n =
    let rec sub m res =
        if m = 0 then res
        else let l = if gcd n m = 1 then 1 else 0
             in sub (m - 1) (res + l)
    in sub n 0
