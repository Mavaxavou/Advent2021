let consumned_fuel_constant data =
  let half = List.length data / 2 in
  let data = List.sort Int.compare data in
  let median_below = List.nth data half in
  let median_above = List.nth data (half + 1) in
  let consumption n = List.fold_left (fun acc x -> acc + abs (x - n)) 0 data in
  min (consumption median_below) (consumption median_above)

let consumned_fuel_prog data =
  let mean_below = List.fold_left (+) 0 data / List.length data in
  let mean_above = mean_below + 1 in
  let sum n = let n = abs n in n * (n + 1) / 2 in
  let consumption n = List.fold_left (fun acc x -> acc + sum (x - n)) 0 data in
  min (consumption mean_below) (consumption mean_above)

let parse s = String.split_on_char ',' s |> Utils.convert_data int_of_string_opt

let () =
  let lines = Utils.read_lines "Day07/data" in
  match Utils.convert_data parse lines with
  | Some [data] ->
    Format.printf "Fuel constant: %d@." (consumned_fuel_constant data) ;
    Format.printf "Fuel prog: %d@." (consumned_fuel_prog data)
  | _ -> assert false
