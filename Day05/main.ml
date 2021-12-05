open Utils

type 'n grid = (int, 'n) Matrix.t
type 'n point = 'n Matrix.pos
type 'n line = 'n point * 'n point

let empty size = Matrix.make 0 size



type 'n hv_line =
  | Ignored
  | Horizontal of 'n horizontal
  | Vertical of 'n vertical
  | Dot of 'n Matrix.pos

and 'n horizontal =
  { hstart : 'n Finite.t ; hstop : 'n Finite.t ; horizontal : 'n Finite.t }

and 'n vertical =
  { vstart : 'n Finite.t ; vstop : 'n Finite.t ; vertical : 'n Finite.t }

let horizontal_or_vertical (start, stop) =
  match Matrix.(start.line = stop.line, start.column = stop.column) with
  | true , true  -> Dot start
  | false, false -> Ignored
  | true , false ->
    let hstart = start.column and hstop = stop.column in
    Horizontal { hstart ; hstop ; horizontal = start.line }
  | false, true  ->
    let vstart = start.line and vstop = stop.line in
    Vertical { vstart ; vstop ; vertical = start.column }

let draw_hv_line size grid line =
  match horizontal_or_vertical line with
  | Ignored -> grid
  | Dot p -> Matrix.edit ((+) 1) p grid
  | Horizontal { hstart ; hstop ; horizontal = line } ->
    let range = Finite.range hstart hstop in
    let ps = List.map (fun column -> Matrix.{ line ; column }) range in
    List.iter (fun p -> Matrix.edit_in_place ((+) 1) p grid) ps ; grid
  | Vertical { vstart ; vstop ; vertical = column } ->
    let range = Finite.range vstart vstop in
    let ps = List.map (fun line -> Matrix.{ line ; column }) range in
    List.iter (fun p -> Matrix.edit_in_place ((+) 1) p grid) ps ; grid



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
    let grid = empty size in
    Format.printf "Start computation...@." ;
    let grid = List.fold_left (draw_hv_line size) grid data in
    Format.printf "Done...@." ;
    let f count sum = if count >= 2 then 1 + sum else sum in
    Format.printf "%d overlaps@." (Matrix.fold f grid 0)
