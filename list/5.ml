let reverse l = 
    let rec sub l1 = function
        [] -> l1
      | a::rest -> sub (a::l1) rest
    in sub [] l;;

