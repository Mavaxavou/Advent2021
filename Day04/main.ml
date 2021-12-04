open Utils



module Data = struct

  module M = Map.Make(Int)
  type 'n t = 'n Matrix.pos list M.t

  let empty = M.empty

  let find (data : int) (m : 'n t) : 'n Matrix.pos list option =
    M.find_opt data m

  let add (data : int) (p : 'n Matrix.pos) (m : 'n t) : 'n t =
    match M.find_opt data m with
    | None -> M.add data [p] m
    | Some ps -> M.add data (p :: ps) m

end



type 'n bingo =
  { matrix : (int * bool, 'n) Matrix.t
  ; map : 'n Data.t
  ; lines : ('n succ Finite.t, 'n) Vect.t
  ; columns : ('n succ Finite.t, 'n) Vect.t
  }

let create (matrix : (int, 'n) Matrix.t) : 'n bingo =
  let length = Vect.length matrix in
  let counter = Finite.of_nat length in
  let lines = Vect.make length counter in
  let columns = Vect.make length counter in
  let map = Matrix.fold (fun p n -> Data.add n p) matrix Data.empty in
  let matrix = Matrix.map (fun i -> (i, false)) matrix in
  { matrix ; map ; lines ; columns }



type 'n result =
  | Unfinished of 'n bingo
  | LineCompleted of 'n Finite.t * (int * bool, 'n) Matrix.t
  | ColumnCompleted of 'n Finite.t * (int * bool, 'n) Matrix.t

let stamp_a_pos (type n) (pos : n Matrix.pos) (bingo : n bingo) : n result =
  let decrease n = Finite.decrease_not_zero_exn n in
  let decrease i vs = try Ok (Vect.edit decrease i vs) with _ -> Error i in
  let lines = decrease pos.Matrix.line bingo.lines in
  let columns = decrease pos.Matrix.column bingo.columns in
  let matrix = Matrix.edit (fun (n, _) -> (n, true)) pos bingo.matrix in
  match lines, columns with
  | Error line, _ -> LineCompleted (line, matrix)
  | _, Error column -> ColumnCompleted (column, matrix)
  | Ok lines, Ok columns ->
    Unfinished { bingo with matrix ; lines ; columns }

let mark (type n) (number : int) (bingo : n bingo) : n result =
  match Data.find number bingo.map with
  | None -> Unfinished bingo
  | Some ps ->
    let stamp r p = match r with Unfinished b -> stamp_a_pos p b | _ -> r in
    List.fold_left stamp (Unfinished bingo) ps

let rec play (numbers : int list) (bingos : 'n bingo list) =
  match numbers with
  | [] -> Format.printf "No winner...@."
  | n :: ns ->
    let module E = struct exception Winner of int end in
    let round_results = List.map (mark n) bingos in
    let is_there_a_winner index = function
      | Unfinished bingo -> bingo
      | LineCompleted (_, matrix) | ColumnCompleted (_, matrix) ->
        let f _ (v, b) acc = if not b then v + acc else acc in 
        let score = n * Matrix.fold f matrix 0 in
        raise (E.Winner score)
    in
    try play ns (List.mapi is_there_a_winner round_results)
    with E.Winner score -> Format.printf "Final score : %d@." score



type 'n data_read = Sep | Row of (int, 'n) Vect.t

let read_bingos (size : 'n nat) (line : string) : 'n data_read option =
  let not_empty s = String.length s != 0 in
  let split line = String.split_on_char ' ' line |> (List.filter not_empty) in
  let row vs = Row (Vect.map int_of_string vs) in
  if String.length line = 0 then Some Sep
  else Vect.of_list (split line) size |> Option.map row

let rec pack : type m n. n nat -> m nat -> (int, n) Matrix.t list ->
  ((int, n) Vect.t, m) Vect.t -> n data_read list ->
  (int, n) Matrix.t list option =
    fun size read_lines result current_batch data ->
      match dec_eq size read_lines, data with
      | None, [] -> None
      | Some Eq, [] -> Some (current_batch :: result)
      | None, Sep :: data -> None
      | None, Row row :: data ->
        pack size (Succ read_lines) result (row :: current_batch) data
      | Some Eq, Sep :: data ->
        pack size Zero (current_batch :: result) [] data
      | Some Eq, Row row :: data -> None

let read (size : 'n nat) (lines : string list) =
  match lines with
  | numbers :: _ :: bingos ->
    let numbers = String.split_on_char ',' numbers |> List.map int_of_string in
    Option.bind (Utils.convert_data (read_bingos size) bingos) @@ fun data ->
      let matrixes = pack size Zero [] [] data in
      let res matrixes = numbers, List.map create matrixes in
      Option.map res matrixes
  | _ -> None



let () =
  let lines = Utils.read_lines "Day04/data" in
  let size = Succ (Succ (Succ (Succ (Succ Zero)))) in
  match read size lines with
  | None -> assert false
  | Some (numbers, bingos) -> play numbers bingos
