type position = { horizontal : int ; depth : int ; aim : int }
type movement = Forward of int | Down of int | Up of int

let compute_result position =
  position.horizontal * position.depth

let move position = function
  | Forward n -> { position with horizontal = position.horizontal + n }
  | Down n -> { position with depth = position.depth + n }
  | Up n -> { position with depth = position.depth - n }

let move_with_aim position = function
  | Down n -> { position with aim = position.aim + n }
  | Up n -> { position with aim = position.aim - n }
  | Forward n ->
    let horizontal = position.horizontal + n in
    let depth = position.depth + position.aim * n in
    { horizontal ; depth ; aim = position.aim }

let convert (s : string) : movement option =
  let cast f size = int_of_string_opt size |> Option.map f in
  match String.split_on_char ' ' s with
  | "forward" :: size :: [] -> cast (fun n -> Forward n) size
  | "down" :: size :: [] -> cast (fun n -> Down n) size
  | "up" :: size :: [] -> cast (fun n -> Up n) size
  | _ -> None

let () =
  let lines = Input.read_lines "Day02/data" in
  match Input.convert_data convert lines with
  | None -> Format.printf "Invalid data@."
  | Some data ->
    let start = { horizontal = 0 ; depth = 0 ; aim = 0 } in
    let position_no_aim = List.fold_left move start data in
    let result_no_aim = compute_result position_no_aim in
    Format.printf "Result without aim: %d@." result_no_aim ;
    let position_with_aim = List.fold_left move_with_aim start data in
    let result_with_aim = compute_result position_with_aim in
    Format.printf "Result with aim: %d@." result_with_aim
