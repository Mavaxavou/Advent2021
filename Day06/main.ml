let next_day population =
  let length = Array.length population in assert (length = 9) ;
  let to_reproduce = population.(0) in
  for i = 0 to length - 2 do population.(i) <- population.(i + 1) done ;
  population.(6) <- population.(6) + to_reproduce ;
  population.(8) <- to_reproduce

let simulate_n_days n population =
  let population = Array.copy population in
  for i = 1 to n do next_day population done ;
  Array.fold_left (+) 0 population

let parse s =
  let population = Array.make 9 0 in
  let data = String.split_on_char ',' s in
  let f s = let i = int_of_string s in population.(i) <- population.(i) + 1 in
  List.iter f data ;
  Some population

let () =
  let lines = Utils.read_lines "Day06/data" in
  match Utils.convert_data parse lines with
  | Some [population] ->
    let total_80 = simulate_n_days 80 population in
    Format.printf "In  80 days, we have %d lanternfish@." total_80 ;
    let total_256 = simulate_n_days 256 population in
    Format.printf "In 256 days, we have %d lanternfish@." total_256
  | _ -> assert false
