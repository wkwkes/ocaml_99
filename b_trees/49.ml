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

let min_codes h =
    if h = 0 then 1 else 
    if h = 1 then 2 else
    let rec sub r1 r2 n =
        if n = h then r1 + r2 + 1
        else sub (r1 + r2 + 1) r1 (n + 1)
    in sub 2 1 2;;

let max_height n =
    let rec sub h =
        if n < min_codes h then h - 1
        else sub (h + 1)
    in sub 0;;
