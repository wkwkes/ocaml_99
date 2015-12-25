
let rec at k l =
    if k = 0 then List.hd l else at (k - 1) (List.tl l);;

let rand_select l n = 
    let token = Random.init 3 in
    let rec sub res num = 
        if num = n then List.rev res 
                   else sub ((at (Random.int (List.length l)) l)::res) (num + 1)
    in sub [] 0;;
       


