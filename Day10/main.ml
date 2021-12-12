open Utils



type bracket = Round | Square | Curly | Angle
type position = Opening | Closing
type delimiter = position * bracket
type stack = bracket list
type error = Corrupted of bracket | Incomplete of stack



let next (d : delimiter) (s : stack) : (stack, error) Result.t =
  match d, s with
  | (Opening, b), _ -> Ok (b :: s)
  | (Closing, b), [] -> Error (Corrupted b)
  | (Closing, b), b' :: bs when b = b' -> Ok bs
  | (Closing, b), _ :: _ -> Error (Corrupted b)

let validate (input : delimiter list) : (unit, error) Result.t =
  let f stack d = Result.bind stack (next d) in
  match List.fold_left f (Ok []) input with
  | Ok [] -> Ok ()
  | Ok stack -> Error (Incomplete stack)
  | Error e -> Error e

let syntax_error_score (results : (unit, error) Result.t list) : int =
  let f = function Round -> 3 | Square -> 57 | Curly -> 1197 | Angle -> 25137 in
  let score = function Error (Corrupted b) -> f b | _ -> 0 in
  List.map score results |> List.fold_left (+) 0

let autocomplete_score (results : (unit, error) Result.t list) : int =
  let f = function Round -> 1 | Square -> 2 | Curly -> 3 | Angle -> 4 in
  let update score bracket = 5 * score + f bracket in
  let count stack = List.fold_left update 0 stack in
  let score = function Error (Incomplete stack) -> count stack | _ -> 0 in
  let scores = List.map score results |> List.filter ((!=) 0) in
  List.nth (List.sort Int.compare scores) (List.length scores / 2)



let delimiter_of_char = function
  | '(' -> Some (Opening, Round ) | ')' -> Some (Closing, Round )
  | '[' -> Some (Opening, Square) | ']' -> Some (Closing, Square)
  | '{' -> Some (Opening, Curly ) | '}' -> Some (Closing, Curly )
  | '<' -> Some (Opening, Angle ) | '>' -> Some (Closing, Angle )
  | _ -> None

let parse s = String.to_seq s |> List.of_seq |> convert_data delimiter_of_char

let () =
  let lines = Utils.read_lines "Day10/data" in
  match Utils.convert_data parse lines with
  | Some inputs ->
    let results = List.map validate inputs in
    Format.printf "Syntax error score: %d@." (syntax_error_score results) ;
    Format.printf "Autocomplete score: %d@." (autocomplete_score results)
  | _ -> assert false
