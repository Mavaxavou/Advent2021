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

let read_lines ch =
  let rec aux acc = try aux (input_line ch :: acc) with End_of_file -> acc in
  List.rev (aux [])

let convert_data (f : 'a -> 'b option) (data : 'a list) : 'b list option =
  try Some (List.map (fun elt -> f elt |> Option.get) data)
  with Invalid_argument _ -> None

let () =
  let channel = open_in "data" in
  let lines = read_lines channel in
  close_in channel ;
  match convert_data int_of_string_opt lines with
  | None -> Format.printf "There is an error in the data@."
  | Some data ->
    Format.printf "There is %d increases@." (count_increases data) ;
    Format.printf "There is %d grouped increases@." (count_increases_group data)
