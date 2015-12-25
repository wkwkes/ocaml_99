let range i j =
    let diff = if i > j then -1 else 1 in
    let rec sub res f =
        if f = j then List.rev (f::res) else sub (f::res) (f + diff)
    in sub [] i;;
