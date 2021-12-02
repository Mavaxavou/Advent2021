let read_lines path =
  let ch = open_in path in
  let rec aux acc = try aux (input_line ch :: acc) with End_of_file -> acc in
  let result = List.rev (aux []) in
  close_in ch ; result

let convert_data (f : 'a -> 'b option) (data : 'a list) : 'b list option =
  try Some (List.map (fun elt -> f elt |> Option.get) data)
  with Invalid_argument _ -> None
