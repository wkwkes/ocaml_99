(*解けなかった*)
let extract n l =
    let rec sub num res func = function
      | [] -> res
      | x::xs ->
        if num = 1 then sub num (func [x] res) func xs else
          let new_func y = func (x::y) in
          sub num (sub (num-1) res new_func xs) func xs
    in let func x res = x::res 
    in sub n [] func l;;
