open List

let slice l m n = 
    let rec sub res i j = function
        [] -> List.rev res 
      | x::xs -> if i = 0 then (if j = 0 then List.rev res else sub (x::res) 0 (j - 1) xs) 
                          else sub res (i - 1) j xs
    in sub [] m n l;;

let rotate l m =
    let len = List.length l in
    let rec sub1 n = 
        if n >= 0 && n < len then n 
        else (if n < 0 then sub1 (n + len) else sub1 (n - len)) in
    let pt = sub1 m in 
    (slice l pt len) @ (slice l 0 pt);;

