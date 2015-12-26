
let head = function
    [] -> []
  | x::_ -> x;;

let min_ f l = 
    let rec sub res = function 
        [] -> res
      | x::xs -> if res > f x then sub (f x) xs else sub res xs
    in sub (f (head l)) l;;

let max_ f l = 
    let rec sub res = function 
        [] -> res
      | x::xs -> if res > f x then sub (f x) xs else sub res xs
    in sub (f (head l)) l;;

let pivot f l =
    let acc = ((max_ f l) + (min_ f l)) / 2 in
    let rec sub piv = function
        [] -> piv
      | x::xs -> if f x > piv then f x else sub piv xs
    in sub acc l;;

let freq' l = 
    let arr = Array.make (5 + max_ List.length l) 0 in 
    let rec sub = function
        [] -> arr
      | x::xs -> let ret = Array.get arr (List.length x) in Array.set arr (List.length x) (1 + ret); sub xs
    in sub l;;

let freq x = let arr = freq' l in Array.get arr (List.length x);;

let rec gen_sort func l =
    let piv = pivot func l in
    let rec sub la lb = function
        [] -> if la = [] then lb else (gen_sort func la) @ (gen_sort func lb)
      | x::xs -> if (func x) >= piv then sub la (x::lb) xs else sub (x::la) lb xs
    in if l = [] then [] else sub [] [] l;;

let rec length_sort l = 
    gen_sort List.length l;;

let rec frequency_sort l = 
    gen_sort freq l;;
