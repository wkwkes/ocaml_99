
let decode l = 
    let rec sub res = function
        [] -> res
      | (One str)::rest -> sub (str::res) rest
      | (Many (num, str))::rest -> sub (sub2 num str res) rest
    and sub2 num str res = match num with
        0 -> res
      | n -> sub2 (n - 1) str (str::res)
    in List.rev (sub []  l);;
