let reduce ~unit ~both = function
  | [] -> unit
  | [x] -> x
  | xs ->
    let xs = List.rev xs in
    match xs with
    | x :: xs ->
      List.fold_right both (List.rev xs) x (* omits hash_empty *)
    | [] -> assert false
