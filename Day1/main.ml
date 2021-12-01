let count_increases (xs : int list) =
  let rec aux acc = function
    | [] | [_] -> acc
    | x1 :: x2 :: xs -> aux (if x1 <= x2 then acc + 1 else acc)  (x2 :: xs)
  in aux 0 xs

let count_increases_group (xs : int list) =
  let rec aux acc = function
    | [] | [_] | [_ ; _] | [_ ; _ ; _] -> acc
    | x1 :: x2 :: x3 :: x4 :: xs ->
      let g1 = x1 + x2 + x3 in
      let g2 = x2 + x3 + x4 in
      let acc = if g1 < g2 then 1 + acc else acc in
      aux acc (x2 :: x3 :: x4 :: xs)
  in aux 0 xs

let read_lines ch =
  let rec aux acc = try aux (input_line ch :: acc) with End_of_file -> acc in
  List.rev (aux [])

let convert_data (f : 'a -> 'b option) (data : 'a list) : 'b list option =
  let module E = struct exception IsNone end in
  let f elt = match f elt with None -> raise E.IsNone | Some v -> v in
  try Some (List.map f data) with E.IsNone -> None

let () =
  let channel = open_in "data" in
  let lines = read_lines channel in
  close_in channel ;
  match convert_data int_of_string_opt lines with
  | None -> Format.printf "There is an error in the data@."
  | Some data ->
    Format.printf "There is %d increases@." (count_increases data) ;
    Format.printf "There is %d grouped increases@." (count_increases_group data)
