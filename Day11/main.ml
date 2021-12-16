open Utils

type 'n octopuses = (int, 'n, 'n) Matrix.t

let step (octopuses, count) : 'n octopuses * int * bool =
  let octopuses = Matrix.map ((+) 1) octopuses in
  let flashed = Matrix.map (fun _ -> false) octopuses in
  let flash pos level (modified, count) =
    if level > 9 && not (Matrix.get flashed pos) then
      let neighborhood = Matrix.(adjacents pos @ diagonals pos) in
      let f p = Matrix.edit_in_place ((+) 1) p octopuses in
      Matrix.set pos true flashed ;
      List.iter (Option.iter f) neighborhood ;
      true, count + 1
    else modified, count
  in
  let rec loop count =
    let modified, count = Matrix.foldi flash octopuses (false, count) in
    if modified then loop count else count
  in
  let count = loop count in
  let all_flashed = Matrix.fold (&&) flashed true in
  let reset pos flashed = if flashed then Matrix.set pos 0 octopuses in
  Matrix.iteri reset flashed ;
  octopuses, count, all_flashed

let parse_line (size : 'n nat) (s : string) : (int, 'n) Vect.t option =
  let int_of_char c = int_of_char c - int_of_char '0' in
  Vect.of_string s size |> Option.map (Vect.map int_of_char)

let parse (size : 'n nat) (lines : string list) : 'n octopuses option =
  let parsed = Utils.convert_data (parse_line size) lines in
  Option.bind parsed (fun parsed -> Vect.of_list parsed size)

let () =
  let lines = Utils.read_lines "Day11/data" in
  let N size = List.length lines |> of_int in
  match parse size lines with
  | None -> assert false
  | Some octopuses ->
    let rec iter stop i (octopuses, count) =
      if i >= 0 && i < stop then
        let (octopuses, count, all_flashed) = step (octopuses, count) in
        if all_flashed then Format.printf "All flashed at %d!@." (i + 1) ;
        iter stop (i + 1) (octopuses, count)
      else count
    in
    Format.printf "Flashes : %d@." (iter 500 0 (octopuses, 0))
