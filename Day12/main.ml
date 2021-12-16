open Utils



module Nodes = Set.Make (String)
module Dict = Map.Make (String)

type 'n graph =
  { indexes : 'n Finite.t Dict.t
  ; backref : (string, 'n) Vect.t
  ; neighboors : (bool, 'n, 'n) Matrix.t
  }

let empty size =
  let indexes = Dict.empty in
  let backref = Vect.make "" size in
  let neighboors = Matrix.make false size size in
  { indexes ; backref ; neighboors }

let children node graph =
  let index = Dict.find node graph.indexes in
  let gather i b acc = if b then Vect.get i graph.backref :: acc else acc in
  Vect.foldi gather (Vect.get index graph.neighboors) []

type node_kind = Start | End | Big | Small
let node_kind node =
  let is_big s = String.(equal s (uppercase_ascii s)) in
  if String.equal node "start" then Start
  else if String.equal node "end" then End
  else if is_big node then Big
  else Small

let count_paths allow_one_small graph =
  let module Visited = Set.Make (String) in
  let sum xs = List.fold_left (+) 0 xs in
  let rec aux (visited, allow_one_small) current =
    let is_visited = Visited.mem current visited in
    let update () = Visited.add current visited in
    let children () = children current graph in
    let next allow = children () |> List.map (aux (update (), allow)) |> sum in
    match node_kind current, is_visited, allow_one_small with
    | End  , _    , _     -> 1
    | Start, true , _     -> 0
    | Small, true , false -> 0
    | Start, false, allow -> next allow
    | Big  , _    , allow -> next allow
    | Small, false, allow -> next allow
    | Small, true , true  -> next false
  in aux (Visited.empty, allow_one_small) "start"



let link s =
  match String.split_on_char '-' s with
  | [ b ; e ] -> Some (b, e)
  | _ -> None

let parse size nodes =
  let index = ref (Finite.of_int size 0) in
  let add n (indexes, backref) =
    if not (Dict.mem n indexes) then
      let i = Option.get !index in
      index := Option.bind !index Finite.increase ;
      Vect.edit_in_place (fun _ -> n) i backref ;
      Dict.add n i indexes, backref
    else indexes, backref
  in
  fun graph (b, e) ->
    let indexes, backref = add b (graph.indexes, graph.backref) |> add e in
    let b_ind = Dict.find b indexes in
    let e_ind = Dict.find e indexes in
    Matrix.set { line = b_ind ; column = e_ind } true graph.neighboors ;
    Matrix.set { line = e_ind ; column = b_ind } true graph.neighboors ;
    { graph with indexes ; backref }



let () =
  let lines = Utils.read_lines "Day12/data" in
  match Utils.convert_data link lines with
  | None -> assert false
  | Some data ->
    let add acc (b, e) = Nodes.(add b acc |> add e) in
    let nodes = List.fold_left add Nodes.empty data in
    let N size = of_int (Nodes.cardinal nodes) in
    let parse = parse size nodes in
    let graph = List.fold_left parse (empty size) data in
    Format.printf "Paths short: %d@." (count_paths false graph) ;
    Format.printf "Paths long : %d@." (count_paths true  graph)
