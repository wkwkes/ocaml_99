let rec last_two = function
    ([] | _::[]) -> None
  | a::b::[] -> Some (a, b)
  | c::rest  -> last_two rest;;
