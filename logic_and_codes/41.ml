
type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let rec fnd lisb tar = function
    [] -> false
  | x::xs -> if x = tar then List.hd lisb else fnd (List.tl lisb) tar xs;;

let mk_bls n =
    let rec sub m res =
        if m = n then res
        else let f b lis = List.map (fun x -> b::x) lis in 
        sub (m + 1) ((f true res) @ (f false res))
    in sub 0 [[]];;

let truth_val lis_str lis_bool expr =
    let rec sub = function
        Var x -> fnd lis_bool x lis_str
      | Not x -> not (sub x)
      | And (x, y) -> sub x && sub y
      | Or (x, y) -> sub x || sub y
    in sub expr;;

let mk_re_lis lis_str lis_bool =
    let rec sub res lisa lisb = 
        if lisa = [] then List.rev res
        else sub ((List.hd lisa, List.hd lisb)::res) (List.tl lisa) (List.tl lisb)
    in sub [] lis_str lis_bool;;

let table lis_str expr =
    List.map (fun lis -> (mk_re_lis lis_str lis, truth_val lis_str lis expr))
    (mk_bls (List.length lis_str));;

let sat lis_str expr =
    let rec sub = function
        [] -> []
      | x::xs -> if truth_val lis_str x expr 
                 then mk_re_lis lis_str x   
                 else sub xs
    in sub (mk_bls (List.length lis_str));;
