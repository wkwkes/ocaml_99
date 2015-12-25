let insert k inp l =
    let rec sub n res = function
        [] -> res
      | x::xs -> if n = k then sub (n + 1) (inp::res) (x::xs) 
                          else sub (n + 1) (x::res) xs
    in sub 0 [] l;;

let permutation l =
    let rec sub res = function
        [] -> res
      | x::xs -> if res = [] then sub [x] xs 
                             else (let num = Random.int (List.length res) 
                                  in sub (insert num x res) xs)
    in sub [] l;;


