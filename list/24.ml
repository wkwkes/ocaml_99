let lotto_select n m = 
    let for_rand = Random.init 3 in
    let rec sub res num = 
        if num = n then res else sub ((Random.int m) :: res) (num + 1)
    in sub [] 0;;
