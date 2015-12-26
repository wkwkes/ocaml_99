let timeit func num =
    let start = Sys.time () in
    let hoge = func num in
    let last = Sys.time () in
    last -. start;;


(*# timeit phi_improved 10090;;
* - : float = 8.40000000000007407e-05
* # timeit phi 10090;;
* - : float = 0.00486100000000000421  *)
    
