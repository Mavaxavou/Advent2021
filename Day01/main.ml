let count_increases (xs : int list) =
  let rec aux acc = function
    | [] | [_] -> acc
    | x1 :: x2 :: xs -> aux (if x1 < x2 then acc + 1 else acc)  (x2 :: xs)
  in aux 0 xs

let count_increases_group (xs : int list) =
  let rec aux acc = function
    | [] | [_] | [_ ; _] -> acc
    | x1 :: x2 :: x3 :: xs -> aux (x1 + x2 + x3 :: acc) (x2 :: x3 :: xs)
  in aux [] xs |> List.rev |> count_increases

let () =
  let lines = Utils.read_lines "Day01/data" in
  match Utils.convert_data int_of_string_opt lines with
  | None -> Format.printf "There is an error in the data@."
  | Some data ->
    Format.printf "There is %d increases@." (count_increases data) ;
    Format.printf "There is %d grouped increases@." (count_increases_group data)
