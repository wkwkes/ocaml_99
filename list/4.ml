let length l = 
    let rec sub n = function
        [] -> n
      | _::rest -> sub (n-1) rest
    in sub 0 l;;

