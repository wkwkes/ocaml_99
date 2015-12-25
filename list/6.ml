let reverse l = 
    let rec sub l1 = function
        [] -> l1
                          | a::rest -> sub (a::l1) rest
    in sub [] l;; 

let rec leq l1 l2 = match (l1, l2) with
    ([], []) -> true
  | (([], _) | (_, [])) -> false
  | (a::ra, b::rb) -> if a = b then leq ra rb else false;;

let is_palindrome l =
    leq (reverse l) l;; 
