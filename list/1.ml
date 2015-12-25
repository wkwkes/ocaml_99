let rec last = function
    [] -> None
  |  a::[] -> Some a
  | head::rest -> last rest;;
