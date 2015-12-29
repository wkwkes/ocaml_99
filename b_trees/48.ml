type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec m_hbal t1 t2 =
    let rec sub s1 lis = function
        [] -> lis
      | hd::tl -> sub s1 (Node ('x', s1, hd)::lis) tl
    in sub t1 [] t2;;

let app t1 t2 =
    List.flatten (List.map (fun x -> m_hbal x t2) t1);;

let rec hbal_tree n =
    let rec sub m =
    if m = 0 then [Empty]
        else let tr1 = (sub (m - 1)) in
             if m = 1 then app tr1 tr1 
             else  let tr2 = (sub (m - 2)) in
                   (app tr1 tr1) @ (app tr1 tr2) @ (app tr2 tr1)
    in sub n;;

