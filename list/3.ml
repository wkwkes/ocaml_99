let rec at k = function
    [] -> None
  | (a::_) when k = 0 -> Some a
  | a::rest -> at (k - 1) rest;;
