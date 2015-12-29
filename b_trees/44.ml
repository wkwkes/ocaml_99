(*
 * それっぽいけど遅すぎるので書き直す*)


type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let mkt n = 
    let rec sub k =
        if k = 0 then Node ('x', Empty, Empty)
        else Node ('x', sub (k - 1), sub (k - 1) )
    in sub n;;

let tt = mkt 2;;


let exp2 n =
    let rec sub res m =
        if m = 0 then res
        else sub (res * 2) (m - 1)
    in sub 1 n;;
        

(*1~m(include) からn個をとる組み合わせを列挙*)
let extract n m =
    let rec sub k res func num =
        if num = 0 then res
        else if k = 1 then sub k (func [num] res) func (num - 1)
             else let new_func x = func (num::x) in
             sub k (sub (k - 1) res new_func (num - 1)) func (num - 1)
    in sub n [] (fun x y -> (x::y)) m;;

(*充足するnode数 *-> (高さ, 葉数) *)
let nodes m =
    let rec sub n exp sum =
        if sum >= m then (n, exp) else sub (n + 1) (exp * 2) (sum + exp * 2)
    in sub 0 1 1;;

let lf_num h =
    let rec sub res n =
        if n = 0 then res else sub (2 * res) (n - 1)
    in sub 1 h;;

let node_num h =
    let rec sub res n =
        if n = 0 then res + 1 else sub (res + lf_num n) (n - 1)
    in sub 0 h;;

(*高さhのtreeの左からl個目の葉をEmptyにする*)
let rm_lf l h tree =
    let rec sub l h t =
        if h = 0 then Empty
        else match t with 
        Node (x, t1, t2) 
         -> let lfs = lf_num (h - 1) in
             if l <= lfs then Node (x, sub l (h - 1) t1, t2)
             else Node (x, t1, sub (l-lfs) (h - 1) t2)
      | _ -> Node ('e', Empty, Empty)
    in sub l h tree;;

let cbal_tree n =
    let (hight, lfs) = nodes n in
    let tree = mkt hight in
    let lis = extract (lfs - (n + 1 - exp2 hight)) lfs in
    let rec sub res = function
        [] -> res
      | x::xs -> let rec sub2 tr = function
                   [] -> tr
                 | y::ys -> sub2 (rm_lf y hight tr) ys
                 in sub ((sub2 tree x)::res) xs
    in sub [] lis;;
