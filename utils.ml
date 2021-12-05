let map_int (f : int -> 'a) (n : int) : 'a list =
  let rec aux acc i = if i >= n then acc else aux (f i :: acc) (i + 1) in
  if n < 0 then [] else aux [] 0

let read_lines path = Arg.read_arg path |> Array.to_list

let convert_data (f : 'a -> 'b option) (data : 'a list) : 'b list option =
  try Some (List.map (fun elt -> f elt |> Option.get) data)
  with Invalid_argument _ -> None



type zero = Z
type 'a succ = S : 'a -> 'a succ

type 'n nat =
  | Zero : zero nat
  | Succ : 'a nat -> 'a succ nat

let rec to_int : type n. n nat -> int = function
  | Zero -> 0
  | Succ n -> 1 + to_int n



type ('a, 'b) eq = Eq : ('a, 'a) eq

let rec dec_eq : type n m. n nat -> m nat -> (n, m) eq option = fun n m ->
  match n, m with
  | Zero, Zero -> Some Eq
  | Zero, _ | _, Zero -> None
  | Succ n, Succ m -> match dec_eq n m with None -> None | Some Eq -> Some Eq



module Finite = struct

  type 'n t =
    | FZ : ('n succ) t
    | FS : 'n t -> ('n succ) t

  let rec of_nat : type n. n nat -> n succ t = function
    | Zero -> FZ
    | Succ n -> FS (of_nat n)

  let rec cast : type n. n t -> n succ t = function
    | FZ -> FZ
    | FS f -> FS (cast f)

  let decrease : 'n t -> 'n t option = function
    | FZ -> None
    | FS f -> Some (cast f)

  exception IsZero
  let decrease_not_zero_exn (f : 'n t) : 'n t =
    match decrease f with
    | None | Some FZ -> raise IsZero
    | Some res -> res

  let rec to_int : type n. n t -> int = function
    | FZ -> 0
    | FS f -> 1 + to_int f

  let pretty fmt f = Format.pp_print_int fmt (to_int f)

end



module Vect = struct

  type ('a, 'n) t =
    | Empty : ('a, zero) t
    | Vect : 'n succ nat * 'a array -> ('a, 'n succ) t

  let assert_invariant : type n. ('a, n) t -> ('a, n) t = function
    | Empty -> Empty
    | Vect (size, xs) as vect -> assert (to_int size = Array.length xs) ; vect

  let check_invariant : type n. ('a, n) t -> ('a, n) t option = function
    | Empty -> Some Empty
    | Vect (size, xs) as vect when to_int size = Array.length xs -> Some vect
    | Vect (_, _) -> None

  let of_list : type n. 'a list -> n nat -> ('a, n) t option =
    fun xs -> function
      | Zero -> Some Empty
      | Succ _ as size -> check_invariant (Vect (size, Array.of_list xs))

  let length : type n. ('a, n) t -> n nat = function
    | Empty -> Zero
    | Vect (size, _) -> size

  let make : type n. 'a -> n nat -> ('a, n) t = fun elt -> function
    | Zero -> Empty
    | Succ _ as size -> Vect (size, Array.make (to_int size) elt)
  
  let nil = Empty

  let cons : type n. 'a -> ('a, n) t -> ('a, n succ) t = fun elt -> function
    | Empty -> Vect (Succ Zero, Array.make 1 elt)
    | Vect (size, xs) ->
      let ys = Array.append (Array.make 1 elt) xs in
      assert_invariant (Vect (Succ size, ys))

  type ('n, 'x, 'a) folder = 'n Finite.t -> 'x -> 'a -> 'a
  let fold : type n. (n, 'x, 'a) folder -> ('x, n) t -> 'a -> 'a =
    fun f vect acc -> match vect with
      | Empty -> acc
      | Vect (Succ size, xs) ->
        let open Finite in
        let folder i acc = f i xs.(to_int i) acc in
        let rec aux : type n. (n t -> 'a -> 'a) -> 'a -> n t -> 'a =
          fun f acc -> function
          | FZ -> f FZ acc
          | FS n -> aux (fun i -> f (FS i)) (f FZ acc) n
        in aux folder acc (of_nat size)
  
  let map : type n. ('a -> 'b) -> ('a, n) t -> ('b, n) t = fun f -> function
    | Empty -> Empty
    | Vect (size, xs) -> Vect (size, Array.map f xs) 

  let edit : type n. ('a -> 'a) -> n Finite.t -> ('a, n) t -> ('a, n) t =
    fun f index -> function
      | Empty -> Empty
      | Vect (size, xs) ->
        let ys = Array.copy xs in
        let i = Finite.to_int index in
        ys.(i) <- f ys.(i) ;
        Vect (size, ys)

  let pretty : type n.
    pp_sep: (Format.formatter -> unit -> unit) ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter -> ('a, n) t -> unit =
      fun ~pp_sep pp_data fmt -> function
        | Empty -> ()
        | Vect (Succ _, _) as vect  ->
          let folder index data () =
            match index with
            | Finite.FZ -> pp_data fmt data
            | Finite.FS _ -> Format.fprintf fmt "%a%a" pp_data data pp_sep ()
          in fold folder vect ()

end



module Matrix = struct

  type 'n pos = { line : 'n Finite.t ; column : 'n Finite.t }
  type ('x, 'n) t = (('x, 'n) Vect.t, 'n) Vect.t

  let fold : type n. (n pos -> 'x -> 'a -> 'a) -> ('x, n) t -> 'a -> 'a =
    fun f -> Vect.(fold (fun line -> fold (fun column -> f {line ; column})))

  let map f matrix = Vect.(map (map f) matrix)

  let edit : type n. ('a -> 'a) -> n pos -> ('a, n) t -> ('a, n) t =
    fun f pos matrix -> Vect.(edit (edit f pos.column) pos.line matrix)

  let pretty f fmt matrix =
    let pp_sep = Format.pp_print_space in
    let pp_line fmt =
      Format.fprintf fmt "@[<h>%a@]" (Vect.pretty ~pp_sep f)
    in Format.fprintf fmt "@[<v>%a@]" (Vect.pretty ~pp_sep pp_line) matrix

end
