let map_int (f : int -> 'a) (n : int) : 'a list =
  let rec aux acc i = if i >= n then acc else aux (f i :: acc) (i + 1) in
  if n < 0 then [] else aux [] 0

let read_lines path = Arg.read_arg path |> Array.to_list

let convert_data (f : 'a -> 'b option) (data : 'a list) : 'b list option =
  try Some (List.map (fun elt -> f elt |> Option.get) data)
  with Invalid_argument _ -> None
