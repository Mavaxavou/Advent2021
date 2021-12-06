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

  let is_zero (Finite (_, n)) = n = 0

  let of_nat : type n. n nat -> n succ t =
    fun n -> Finite (Succ n, to_int n)

  let of_int : type n. n nat -> int -> n t option = fun max n ->
    match max with
    | Zero -> None
    | Succ _ as max -> if to_int max > n then Some (Finite (max, n)) else None

  let to_int : type n. n t -> int = fun (Finite (_, n)) -> n

  let decrease (Finite (card, n)) =
    if n > 0 then Some (Finite (card, n - 1)) else None

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

  let map : type n. ('a -> 'b) -> ('a, n) t -> ('b, n) t = fun f -> function
    | Empty -> Empty
    | Vect (size, xs) -> Vect (size, Array.map f xs) 

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
  
end



module Matrix = struct

  type 'n pos = { line : 'n Finite.t ; column : 'n Finite.t }
  type ('x, 'n) t = (('x, 'n) Vect.t, 'n) Vect.t

  let make elt size =
    let xs = Vect.make () size in
    Vect.map (fun () -> Vect.make elt size) xs

  let fold f = Vect.(fold (fold f))

  let foldi : type n. (n pos -> 'x -> 'a -> 'a) -> ('x, n) t -> 'a -> 'a =
    fun f -> Vect.(foldi (fun line -> foldi (fun column -> f {line ; column})))

  let map f matrix = Vect.(map (map f) matrix)

  let edit_in_place : type n. ('a -> 'a) -> n pos -> ('a, n) t -> unit =
    fun f pos -> function
      | Vect.Empty -> ()
      | Vect.Vect (_, lines) ->
        Vect.edit_in_place f pos.column lines.(Finite.to_int pos.line)

  let edit : type n. ('a -> 'a) -> n pos -> ('a, n) t -> ('a, n) t =
    fun f pos matrix -> Vect.(edit (edit f pos.column) pos.line matrix)

end
