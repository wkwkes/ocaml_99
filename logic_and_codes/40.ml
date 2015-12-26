type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let truth_val a str_a b str_b expr =
    let rec sub = function
        Var x -> if x = str_a then a else b
      | Not x -> not (sub x)
      | And (x, y) -> sub x && sub y
      | Or (x, y) -> sub x || sub y
    in sub expr;;

let table2 str_a str_b expr =
    let f a b = (a, b, truth_val a str_a b str_b expr) in
    (f true true)::(f true false)::(f false true)::(f false false)::[];;
