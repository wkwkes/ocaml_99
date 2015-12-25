open List

type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let rec flatten l = 
    let f res = function
        [] -> res
  | (One a)::rest -> (rev (flatten rest)) @ (a::res)
  | (Many l)::rest -> (rev (flatten rest)) @ (rev (flatten l)) @ res
  in rev(f [] l);;
