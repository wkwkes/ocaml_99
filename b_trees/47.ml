type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let add_trees_with left right all =
    let add_right_tree all l =
      List.fold_left (fun a r -> Node('x', l, r) :: a) all right in
    List.fold_left add_right_tree all left
  
  let rec cbal_tree n =
    if n = 0 then [Empty]
    else if n mod 2 = 1 then
      let t = cbal_tree (n / 2) in
      add_trees_with t t []
    else 
      let t1 = cbal_tree (n / 2 - 1) in
      let t2 = cbal_tree (n / 2) in
      add_trees_with t1 t2 (add_trees_with t2 t1 []);;
      
let rec is_mirror t1 t2 = 
    match (t1, t2) with
    (Empty, Empty) -> true;
  | (Node (_, t11, t12), Node (_, t21, t22))
    -> (is_mirror t11 t21) && (is_mirror t12 t22)
  | _ -> false

let is_symmetric = function
    Empty -> true
  | Node (_, t1, t2) -> is_mirror t1 t2;;
  
let sym_cbal_trees n = 
    List.filter is_symmetric (cbal_tree n);;
    
let range f e =
    let rec sub res p =
        if p = e then List.rev (p::res)
        else sub (p::res) (p + 1)
    in sub [] f;;