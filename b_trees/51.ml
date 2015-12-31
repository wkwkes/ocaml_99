
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let rec leaves = function
    Node (x, Empty, Empty) -> [x]
  | Node (x, t1, t2) -> (leaves t1) @ (leaves t2)
  | _ -> [];;

let example_tree =
        Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
                   Node('c', Empty, Node('f', Node('g', Empty, Empty),
                   Empty)));;
