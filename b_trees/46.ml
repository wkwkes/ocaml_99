
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let insert tree n =
    let rec sub n = function
        Empty -> Node (n, Empty, Empty)
      | Node (x, t1, t2) -> if x > n then Node (x, sub n t1, t2)
                            else Node (x, t1, sub n t2)
    in sub n tree;;

let construct lis =
    List.fold_left insert Empty lis;;


let rec is_mirror t1 t2 = 
    match (t1, t2) with
    (Empty, Empty) -> true;
  | (Node (_, t11, t12), Node (_, t21, t22))
    -> (is_mirror t11 t21) && (is_mirror t12 t22)
  | _ -> false

let is_symmetric = function
    Empty -> true
  | Node (_, t1, t2) -> is_mirror t1 t2;;

