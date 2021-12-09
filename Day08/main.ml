open Utils



type size = zero succ succ succ succ succ succ succ
let size = Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))

type 'a segments = ('a, size) Vect.t
let activated () = Vect.make true size
let deactivated () = Vect.make false size
let activated_size xs = Vect.fold (fun b -> (+) (if b then 1 else 0)) xs 0

let of_list xs = Option.get @@ Vect.of_list xs size
let zero  = of_list [ true  ; true  ; true  ; false ; true  ; true  ; true  ]
let one   = of_list [ false ; false ; true  ; false ; false ; true  ; false ]
let two   = of_list [ true  ; false ; true  ; true  ; true  ; false ; true  ]
let three = of_list [ true  ; false ; true  ; true  ; false ; true  ; true  ]
let four  = of_list [ false ; true  ; true  ; true  ; false ; true  ; false ]
let five  = of_list [ true  ; true  ; false ; true  ; false ; true  ; true  ]
let six   = of_list [ true  ; true  ; false ; true  ; true  ; true  ; true  ]
let seven = of_list [ true  ; false ; true  ; false ; false ; true  ; false ]
let eight = of_list [ true  ; true  ; true  ; true  ; true  ; true  ; true  ]
let nine  = of_list [ true  ; true  ; true  ; true  ; false ; true  ; true  ]
let ns = [ zero ; one ; two ; three ; four ; five ; six ; seven ; eight ; nine ]



let matchings patterns =
  let matching = Vect.make (activated ()) size in
  let update_matchings pattern =
    let n = activated_size pattern in
    let is_of_size n xs = activated_size xs = n in
    let numbers = List.filter (is_of_size n) ns in
    let join op = List.fold_left (Vect.map2 op) in
    let may_be = join (||) (deactivated ()) numbers in
    let must_be = Vect.map not (join (&&) (activated ()) numbers) in
    let edit infos i = Vect.edit_in_place ((Vect.map2 (&&)) infos) i matching in
    let update i act = if act then edit may_be i else edit must_be i in
    Vect.iteri update pattern
  in List.iter update_matchings patterns ; matching

let cross_results matching =
  let cross_results index possibilities matching =
    assert (activated_size possibilities != 0) ;
    if activated_size possibilities = 1 then
      let f index' possibilities' =
        if Finite.equal index index'
        then possibilities'
        else Vect.map2 (&&) possibilities' (Vect.map not possibilities)
      in Vect.mapi f matching
    else matching
  in Vect.foldi cross_results matching matching

let flatten possibilities =
  assert (activated_size possibilities = 1) ;
  let module E = struct exception N of size Finite.t end in
  let f i b = if b then raise (E.N i) in
  try Vect.iteri f possibilities ; assert false
  with E.N i -> i

let decode patterns =
  matchings patterns |> cross_results |> Vect.map flatten

let displayed_number number =
  let module E = struct exception Found of int end in
  let f i n = if Vect.equal Bool.equal n number then raise (E.Found i) in
  try List.iteri f ns ; assert false
  with E.Found i -> i

let translate (patterns, data) =
  let translation = decode patterns in
  let translate number =
    let res = deactivated () in
    let edit i = Vect.(edit_in_place (fun _ -> true) (get i translation) res) in
    Vect.iteri (fun i b -> if b then edit i) number ;
    displayed_number res
  in List.map translate data



let a = Finite.of_int size 0 |> Option.get
let b = Finite.of_int size 1 |> Option.get
let c = Finite.of_int size 2 |> Option.get
let d = Finite.of_int size 3 |> Option.get
let e = Finite.of_int size 4 |> Option.get
let f = Finite.of_int size 5 |> Option.get
let g = Finite.of_int size 6 |> Option.get

let index = function
  | 'a' -> a | 'b' -> b | 'c' -> c | 'd' -> d | 'e' -> e
  | 'f' -> f | 'g' -> g | _ -> assert false

let of_string s =
  let segments = deactivated () in
  let f c = Vect.edit_in_place (fun _ -> true) (index c) segments in
  String.iter f s ; segments

let parse s =
  let split s = String.split_on_char ' ' s |> List.map of_string in
  match String.split_on_char '|' s |> List.map String.trim with
  | [ patterns ; to_decode ] -> Some (split patterns, split to_decode)
  | _ -> None



let () =
  let lines = Utils.read_lines "Day08/data" in
  match Utils.convert_data parse lines with
  | Some data ->
    let results = List.map translate data in
    let is_good x = x = 1 || x = 4 || x = 7 || x = 8 in
    let count_good acc x = acc + if is_good x then 1 else 0 in
    let r1 = List.(fold_left (fold_left count_good) 0 results) in
    Format.printf "Res : %d@." r1 ;
    let to_int acc x = acc * 10 + x in
    let numbers = List.(map (fold_left to_int 0) results) in
    let r2 = List.(fold_left (+) 0 numbers) in
    Format.printf "Res : %d@." r2
  | _ -> assert false
