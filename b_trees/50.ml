type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let rec count_leaves = function
    Node (_, Empty, Empty) -> 1 
  | Node (_, t1, t2) -> (count_leaves t1) + count_leaves t2
  | _ -> 0;;
