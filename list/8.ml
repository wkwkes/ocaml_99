
let compress l = 
    let rec sub res str = function
        [] -> res
      | head::rest -> if str = head then sub res str rest 
                                    else sub (head::res) head rest 
    in if l = [] then [] else List.rev (sub [List.hd l] (List.hd l) (List.tl l));;
