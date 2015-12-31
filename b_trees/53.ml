

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let at_level tr l = 
    let rec sub lev = function
        Node (x, t1, t2) -> if lev = 1 then [x] 
                            else (sub (lev - 1) t1) @ (sub (lev - 1) t2)
      | Node (x, t1, Empty) | Node (x, Empty, t1) -> if lev = 1 then [x]
                                                 else sub (lev - 1) t1
      | Node (x, Empty, Empty) -> if lev = 1 then [x] else []
      | _ -> []
    in sub l tr;;

let example_tree =
        Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
                   Node('c', Empty, Node('f', Node('g', Empty, Empty),
                   Empty)));;
