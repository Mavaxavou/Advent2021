(* Invariant : length can not be greater than bit_vector_max_size *)
type bit_vector = { length : int ; data : int }
let bit_vector_max_size = 32

type bit = O | I
let int_of_bit = function O -> 0 | I -> 1
let bit_of_int n = if n == 0 then O else I

let get (i : int) (bv : bit_vector) : bit =
  assert (i < bv.length && i >= 0) ;
  bit_of_int @@ (bv.data land (1 lsl i)) lsr i

let to_int (bv : bit_vector) : int =
  let mask = (1 lsl bv.length) - 1 in
  mask land bv.data

let to_list (bv : bit_vector) : bit list =
  Utils.map_int (fun i -> get i bv) bv.data

let of_list (bs : bit list) : bit_vector =
  let data = List.fold_left (fun res b -> res lsl 1 + int_of_bit b) 0 bs in
  { length = List.length bs ; data }

let map (f : bit -> 'a) (bv : bit_vector) : 'a list =
  to_list bv |> List.map f

let not (bv : bit_vector) : bit_vector =
  { bv with data = ((1 lsl bv.length) - 1) land (lnot bv.data) }



(* Invariant : all the bit vectors have the same length *)
type bit_vectors = { nb_bits : int ; vectors : bit_vector list }

let select_bit (index : int) (bvs : bit_vectors) : bit list  =
  List.map (get index) bvs.vectors

let count_most_common (bs : bit list) : bit =
  let f acc = function O -> acc - 1 | I -> acc + 1 in
  if (List.fold_left f 0 bs) >= 0 then I else O

let filter (f : bit -> bool) (index : int) (bvs : bit_vectors) : bit_vectors =
  { bvs with vectors = List.filter (fun bv -> get index bv |> f) bvs.vectors }



let compute_power_consumption (input : bit_vectors) : int =
  let count_for_index i = select_bit i input |> count_most_common in
  let gamma = Utils.map_int count_for_index input.nb_bits |> of_list in
  to_int gamma * to_int (not gamma)

let compute_rating (f : bit -> bit) (input : bit_vectors) : int =
  let rec aux index bvs =
    assert (index >= 0 || index < bvs.nb_bits) ;
    let common = select_bit index bvs |> count_most_common |> f in
    let bvs = filter (fun b -> b = common) index bvs in
    let length = List.length bvs.vectors in
    if length <= 0 then raise (Invalid_argument "Incoherent input")
    else if length = 1 then List.hd bvs.vectors |> to_int
    else aux (index - 1) bvs
  in aux (input.nb_bits - 1) input

let life_support_rating (input : bit_vectors) : int =
  let oxygen_rating   = compute_rating (fun b -> b) input in
  let scrubber_rating = compute_rating (function O -> I | I -> O) input in
  oxygen_rating * scrubber_rating



let string_to_bit_vector (s : string) : bit_vector option =
  let length = String.length s in
  if length < bit_vector_max_size then
    let module E = struct exception Invalid end in
    let bit = function '0' -> 0 | '1' -> 1 | _ -> raise E.Invalid in
    let add_shift r b = r lsl 1 + bit b in
    try Some { length ; data = String.to_seq s |> Seq.fold_left add_shift 0 }
    with E.Invalid -> None
  else None

let check_input (input : bit_vector list) : bit_vectors =
  match input with
  | [] -> { nb_bits = 0 ; vectors = [] }
  | hd :: tl ->
    List.iter (fun bv -> assert (hd.length = bv.length)) tl ;
    { nb_bits = hd.length ; vectors = hd :: tl }

let () =
  let lines = Utils.read_lines "Day03/data" in
  match Utils.convert_data string_to_bit_vector lines with
  | None -> Format.printf "Invalid data@."
  | Some data ->
    let data = check_input data in
    Format.printf "Power consumption = %d@." (compute_power_consumption data) ;
    Format.printf "Life support rating = %d@." (life_support_rating data)
