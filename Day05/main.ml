open Utils

type 'n grid = (int, 'n, 'n) Matrix.t
type 'n point = ('n, 'n) Matrix.pos
type 'n line = 'n point * 'n point

let empty size = Matrix.make 0 size size



let points_of_line keeps_diagonals (start, stop) =
  match Matrix.(start.line = stop.line, start.column = stop.column) with
  | true, true -> [start]
  | true, false ->
    let columns = Finite.range start.column stop.column in
    List.map (fun column -> Matrix.{ line = start.line ; column }) columns
  | false, true ->
    let lines = Finite.range start.line stop.line in
    List.map (fun line -> Matrix.{ line ; column = start.column }) lines
  | false, false ->
    if keeps_diagonals then
      let lines = Finite.range start.line stop.line in
      let columns = Finite.range start.column stop.column in
      if List.(length lines = length columns) then
        List.map2 (fun line column -> Matrix.{ line ; column}) lines columns
      else []
    else []

let draw_hv_line size keeps_diagonals grid line =
  let points = points_of_line keeps_diagonals line in
  List.iter (fun p -> Matrix.edit_in_place ((+) 1) p grid) points ; grid



let string_to_point (size : 'n nat) (s : string) : 'n point option =
  match String.split_on_char ',' s with
  | line :: column :: [] ->
    let (>>=) opt f = Option.bind opt f in
    (int_of_string line |> Finite.of_int size) >>= fun line ->
    (int_of_string column |> Finite.of_int size) >>= fun column ->
    Some Matrix.{ line ; column }
  | _ -> None

let string_to_line (size : 'n nat) (s : string) : 'n line option =
  match String.split_on_char ' ' s with
  | start :: "->" :: stop :: [] ->
    let (>>=) opt f = Option.bind opt f in
    (string_to_point size start) >>= fun start ->
    (string_to_point size stop) >>= fun stop ->
    Some (start, stop)
  | _ -> None

let () =
  let lines = Utils.read_lines "Day05/data" in
  let (N size) = of_int 1000 in
  match Utils.convert_data (string_to_line size) lines with
  | None -> assert false
  | Some data ->
    let no_diag = List.fold_left (draw_hv_line size false) (empty size) data in
    let with_diag = List.fold_left (draw_hv_line size true) (empty size) data in
    let f count sum = if count >= 2 then 1 + sum else sum in
    Format.printf "%d overlaps without diagonals@." (Matrix.fold f no_diag 0) ;
    Format.printf "%d overlaps with diagonals@." (Matrix.fold f with_diag 0)
