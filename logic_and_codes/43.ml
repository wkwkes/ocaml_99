type 'a tree = Lf of string * int | Br of int * 'a tree * 'a tree ;;

let tes = List.map (fun x -> Lf (fst x, snd x)) [ ("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5) ];;

(*'a tree list -> int(nodeの値の最大値)*)
let max_ lis =
    let rec s res = function
        [] -> res
      | x::rest -> (match x with
                   Lf (_, b) | Br (b, _, _) -> if b > res then s b rest else s res rest)
    in s (-1) lis;;

let era lis mv =
    let rec sub res = function
        [] -> res
      | x::xs -> if x = mv then sub res xs else sub (x::res) xs
    in sub [] lis;;

let extra = function
    Lf (_, a) -> a
  | Br (a, _, _) -> a

(*小さいふたつをとってくる*)
let min_two lis =
    if List.length lis = 2 then (List.hd lis, List.hd (List.tl lis), [])
    else let rec sub res1 res2 rest = function
             [] -> (res1, res2, rest)
           | x::xs -> match x with
           Lf (_, n) | Br (n, _, _) ->
                  let n1 = extra res1 in
                  let n2 = extra res2 in
                  if n < n1 then sub x res1 (res2::rest) xs 
                  else if n < n2 then sub res1 x (res2::rest) xs else sub res1 res2 (x::rest) xs
             in let mv = Lf ("", max_ lis)
             in let (r1, r2, li) = sub mv mv [] lis
             in (r1, r2, era li mv);;

let huff_tree lis =
    let t_lis = List.map (fun x -> Lf (fst x, snd x)) lis in
    let rec sub tree =
        if List.length tree = 1 then List.hd tree
        else let (t1, t2, rest) = min_two tree in
        sub ((Br ((extra t1) + (extra t2), t2, t1))::rest)
    in sub t_lis;;

let hffcode_of_hfftree lis =
    let rec sub res path = function
        Lf (str, _) -> (str, path)::res
      | Br (_, b1, b2) -> let f s b = sub res (path ^ s) b in
                          (f "0" b1) @ (f "1" b2)
    in sub [] "" lis;;

let huffman fs =
    hffcode_of_hfftree (huff_tree fs);;
