let rec gcd a b = 
        if a < b then gcd b a else
            let c = a mod b in if c = 0 then b else gcd b c;; 

let coprime a b = 
    gcd a b = 1;;
