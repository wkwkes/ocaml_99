

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let rec internals = function
    Node (_, Empty, Empty) -> [] 
  | Node (x, t1, t2) -> x::((internals t1) @ (internals t2))
  | _ -> [];;

let example_tree = Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e',
Empty, Empty)),
Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)));;
