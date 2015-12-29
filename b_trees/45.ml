type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec is_mirror t1 t2 = 
    match (t1, t2) with
    (Empty, Empty) -> true;
  | (Node (_, t11, t12), Node (_, t21, t22))
    -> (is_mirror t11 t21) && (is_mirror t12 t22)
  | _ -> false

let is_symmetric = function
    Empty -> true
  | Node (_, t1, t2) -> is_mirror t1 t2;;
