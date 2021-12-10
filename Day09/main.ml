open Utils



type ten = zero succ succ succ succ succ succ succ succ succ succ
let ten =
  Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))))

type height = ten Finite.t
type ('l, 'c) heightmap = (height, 'l, 'c) Matrix.t

let is_low_point (pos : ('l, 'c) Matrix.pos) (hm : ('l, 'c) heightmap) =
  let height = Matrix.get hm pos in
  let f opt = Option.(map (Finite.(<) height) opt |> value ~default:true) in
  Matrix.adjacents_values pos hm |> List.for_all f

let risk_levels_sum (hm : ('l, 'c) heightmap) =
  let is_low_point pos _ = is_low_point pos hm in
  let risk_level (_, n) = 1 + Finite.to_int n in
  Matrix.filter is_low_point hm |> List.map risk_level |> List.fold_left (+) 0

let explore_basin nb_lines nb_columns hm start =
  let basin = Matrix.make false nb_lines nb_columns in
  let nine = Option.get @@ Finite.of_int ten 9 in
  Matrix.edit_in_place (fun _ -> true) start basin ;
  let rec aux = function
    | [] -> ()
    | pos :: pool ->
      let f acc = function
        | None -> acc
        | Some p when Matrix.get basin p -> acc
        | Some p when Finite.equal nine (Matrix.get hm p) -> acc
        | Some p -> Matrix.edit_in_place (fun _ -> true) p basin ; p :: acc
      in aux (Matrix.adjacents pos hm |> List.fold_left f pool)
  in aux [start] ;
  Matrix.fold (fun x -> (+) (if x then 1 else 0)) basin 0

let largest_pools_product nb_lines nb_columns hm =
  let is_low_point pos _ = is_low_point pos hm in
  let low_points = Matrix.filter is_low_point hm |> List.map fst in
  let sizes = List.map (explore_basin nb_lines nb_columns hm) low_points in
  match List.(sort Int.compare sizes |> rev) with
  | x :: y :: z :: _ -> x * y * z
  | _ -> assert false



let dimensions (lines : string list) =
  let length = List.length lines in
  if length > 0 then
    let nb_lines = Utils.of_int (List.length lines) in
    let nb_columns = Utils.of_int (List.hd lines |> String.length) in
    Some (nb_lines, nb_columns)
  else None

let parse (nb_columns : 'n nat) (line : string) : (height, 'n) Vect.t option =
  let chars = String.to_seq line |> List.of_seq in
  let int_of_char c = int_of_char c - int_of_char '0' in
  let of_list ls = Option.bind ls (fun ls -> Vect.of_list ls nb_columns) in
  List.map int_of_char chars |> convert_data (Finite.of_int ten) |> of_list

let () =
  let lines = Utils.read_lines "Day09/data" in
  let (N nb_lines, N nb_columns) = Option.get @@ dimensions lines in
  match Utils.convert_data (parse nb_columns) lines with
  | Some data ->
    let hm = Option.get @@ Vect.of_list data nb_lines in
    let risk_levels_sum = risk_levels_sum hm in
    let largest_pools_product = largest_pools_product nb_lines nb_columns hm in
    Format.printf "Risk levels sum : %d@." risk_levels_sum ;
    Format.printf "Largest pools product : %d@." largest_pools_product
  | _ -> assert false
