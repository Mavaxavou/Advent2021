module Option = struct
  include Option
  let zip x y = match x, y with Some x, Some y -> Some (x, y) | _, _ -> None
end



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

type natural = N : 'n nat -> natural

let rec of_int : int -> natural = function
  | 0 -> N Zero
  | n -> let (N k) = of_int (n - 1) in N (Succ k)



type ('a, 'b) eq = Eq : ('a, 'a) eq

let rec dec_eq : type n m. n nat -> m nat -> (n, m) eq option = fun n m ->
  match n, m with
  | Zero, Zero -> Some Eq
  | Zero, _ | _, Zero -> None
  | Succ n, Succ m -> match dec_eq n m with None -> None | Some Eq -> Some Eq



module Finite = struct

  type 'n t = Finite : 'n succ nat * int -> 'n succ t

  let compare : type n. n t -> n t -> int =
    fun (Finite (_, x)) (Finite (_, y)) -> compare x y

  let equal : type n. n t -> n t -> bool =
    fun (Finite (_, x)) (Finite (_, y)) -> x = y

  let is_zero (Finite (_, n)) = n = 0

  let of_nat : type n. n nat -> n succ t =
    fun n -> Finite (Succ n, to_int n)

  let of_int : type n. n nat -> int -> n t option = fun max n ->
    match max with
    | Zero -> None
    | Succ _ as max -> if to_int max > n then Some (Finite (max, n)) else None

  let decrease : type n. n t -> n t option = fun (Finite (card, n)) ->
    if n > 0 then Some (Finite (card, n - 1)) else None

  let increase : type n. n t -> n t option = fun (Finite (card, n)) ->
    if n < to_int card - 1 then Some (Finite (card, n + 1)) else None

  let to_int : type n. n t -> int = fun (Finite (_, n)) -> n

  exception IsZero
  let decrease_not_zero_exn (Finite (card, n)) =
    if n > 1 then Finite (card, n - 1) else raise IsZero

  let range : type n. n t -> n t -> n t list =
    fun (Finite (card, start)) (Finite (_, stop)) ->
      let incr = if start <= stop then fun s -> s - 1 else fun s -> s + 1 in
      let rec aux acc start stop =
        if start = stop then start :: acc
        else aux (stop :: acc) start (incr stop)
      in aux [] start stop |> List.map (fun n -> Finite (card, n))

  let rec fold : type n. (n t -> 'a -> 'a) -> n t -> 'a -> 'a =
    fun f (Finite (card, n) as index) acc ->
      if n = 0 then (f index acc)
      else fold f (Finite (card, n - 1)) (f index acc)

  let (<) : type n. n t -> n t -> bool =
    fun (Finite (_, x)) (Finite (_, y)) -> x < y

  let pretty : type n. Format.formatter -> n t -> unit =
    fun fmt (Finite (_, n)) -> Format.pp_print_int fmt n

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

  let of_string : type n. string -> n nat -> (char, n) t option =
    fun s -> function
      | Zero -> Some Empty
      | Succ _ as size ->
        check_invariant (Vect (size, String.to_seq s |> Array.of_seq))

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

  let fold : type n. ('x -> 'a -> 'a) -> ('x, n) t -> 'a -> 'a =
    fun f vect acc -> match vect with
      | Empty -> acc
      | Vect (Succ _, xs) -> Array.fold_left (fun acc x -> f x acc) acc xs

  type ('n, 'x, 'a) folder = 'n Finite.t -> 'x -> 'a -> 'a
  let foldi : type n. (n, 'x, 'a) folder -> ('x, n) t -> 'a -> 'a =
    fun f vect acc -> match vect with
      | Empty -> acc
      | Vect (Succ size, xs) ->
        let folder i acc = f i xs.(Finite.to_int i) acc in
        Finite.fold folder (Finite.of_nat size) acc

  let iteri : type n. (n Finite.t -> 'x -> unit) -> ('x, n) t -> unit =
    fun f xs -> foldi (fun i x () -> f i x) xs ()

  let map : type n. ('a -> 'b) -> ('a, n) t -> ('b, n) t = fun f -> function
    | Empty -> Empty
    | Vect (size, xs) -> Vect (size, Array.map f xs) 

  let mapi : type n. (n Finite.t -> 'a -> 'b) -> ('a, n) t -> ('b, n) t =
    fun f -> function
      | Empty -> Empty
      | Vect (size, xs) ->
        let f i x = f (Option.get @@ Finite.of_int size i) x in
        Vect (size, Array.mapi f xs)

  let map2 : type n. ('a -> 'b -> 'c) -> ('a, n) t -> ('b, n) t -> ('c, n) t =
    fun f xs ys -> match xs, ys with
      | Empty, Empty -> Empty
      | Vect (size, xs), Vect (_, ys) -> Vect (size, Array.map2 f xs ys)

  let edit_in_place : type n. ('a -> 'a) -> n Finite.t -> ('a, n) t -> unit =
    fun f index -> function
      | Empty -> ()
      | Vect (size, xs) ->
        let i = Finite.to_int index in
        xs.(i) <- f xs.(i)

  let edit : type n. ('a -> 'a) -> n Finite.t -> ('a, n) t -> ('a, n) t =
    fun f index -> function
      | Empty -> Empty
      | Vect (size, xs) ->
        let ys = Array.copy xs in
        let i = Finite.to_int index in
        ys.(i) <- f ys.(i) ;
        Vect (size, ys)

  let get : type n. n Finite.t -> ('a, n) t -> 'a = fun index vect ->
    match index, vect with
    | Finite.Finite _, Vect (_, xs) -> xs.(Finite.to_int index)

  let equal : type n. ('a -> 'a -> bool) -> ('a, n) t -> ('a, n) t -> bool =
    fun eq xs ys -> fold (&&) (map2 eq xs ys) true

  let pretty : type n. (Format.formatter -> 'a -> unit) ->
    Format.formatter -> ('a, n) t -> unit =
      fun pp fmt -> function
        | Empty -> ()
        | Vect (size, xs) ->
          Format.fprintf fmt "%a" pp xs.(0) ;
          for i = 1 to to_int size - 1 do
            Format.fprintf fmt "%a" pp xs.(i)
          done
  
end



module Matrix = struct

  type ('l, 'c) pos = { line : 'l Finite.t ; column : 'c Finite.t }
  type ('x, 'l, 'c) t = (('x, 'c) Vect.t, 'l) Vect.t

  let make elt lines columns =
    let xs = Vect.make () lines in
    Vect.map (fun () -> Vect.make elt columns) xs

  let fold f = Vect.(fold (fold f))

  type ('l, 'c, 'x, 'a) folder = ('l, 'c) pos -> 'x -> 'a -> 'a
  let foldi : type l c. (l, c, 'x, 'a) folder -> ('x, l, c) t -> 'a -> 'a =
    fun f -> Vect.(foldi (fun line -> foldi (fun column -> f {line ; column})))

  let map f matrix = Vect.(map (map f) matrix)

  let iteri f matrix = foldi (fun pos data () -> f pos data) matrix ()

  type ('a, 'l, 'c) pred = ('l, 'c) pos -> 'a -> bool
  let filter : type l c. ('a, l, c) pred -> ('x, l, c) t -> ((l, c) pos * 'a) list =
    fun pred matrix ->
      let f pos x acc = if pred pos x then (pos, x) :: acc else acc in
      foldi f matrix []

  let edit_in_place : type l c. ('a -> 'a) -> (l, c) pos -> ('a, l, c) t -> unit =
    fun f pos -> function
      | Vect.Empty -> ()
      | Vect.Vect (_, lines) ->
        Vect.edit_in_place f pos.column lines.(Finite.to_int pos.line)

  let set : type l c. (l, c) pos -> 'a -> ('a, l, c) t -> unit =
    fun pos data matrix -> edit_in_place (fun _ -> data) pos matrix

  let edit : type l c. ('a -> 'a) -> (l, c) pos -> ('a, l, c) t -> ('a, l, c) t =
    fun f pos matrix -> Vect.(edit (edit f pos.column) pos.line matrix)

  let get : type l c. ('a, l, c) t -> (l, c) pos -> 'a = fun m pos ->
    Vect.get pos.column (Vect.get pos.line m)

  let adjacents : type l c. (l, c) pos -> (l, c) pos option list =
    fun pos ->
      let update_column column = { pos with column } in
      let update_line   line   = { pos with line   } in
      let left  = Option.map update_column (Finite.decrease pos.column) in
      let right = Option.map update_column (Finite.increase pos.column) in
      let above = Option.map update_line   (Finite.decrease pos.line  ) in
      let below = Option.map update_line   (Finite.increase pos.line  ) in
      [ left ; right ; above ; below ]

  let diagonals : type l c. (l, c) pos -> (l, c) pos option list =
    fun { column ; line } ->
      let to_pos (line, column) = { line ; column } in
      let build line column = Option.(zip line column |> map to_pos) in
      let a =  Finite.(build (decrease line) (decrease column)) in
      let b =  Finite.(build (decrease line) (increase column)) in
      let c =  Finite.(build (increase line) (decrease column)) in
      let d =  Finite.(build (increase line) (increase column)) in
      [ a ; b ; c ; d ]

  let adjacents_values pos m = adjacents pos |> List.map (Option.map (get m))

  let pretty pp fmt m =
    let pp fmt = Format.fprintf fmt "%a@." (Vect.pretty pp) in
    Vect.pretty pp fmt m

end
