type ('a, 'b) either = Left of 'a | Right of 'b
type never

val car : 'a * 'b -> 'a
val cdr : 'a * 'b -> 'b

(* generic comparison *)
val eq : int -> bool
val ge : int -> bool
val gt : int -> bool
val le : int -> bool
val lt : int -> bool
val neq : int -> bool

(* operations on unit *)
val unit : unit -> unit
val compare : 'a -> 'a -> bool

(* operations on booleans *)
val or_ : bool -> bool -> bool (* 'or' is an OCaml keyword! *)
val and_ : bool -> bool -> bool (* 'and' is an OCaml keyword! *)
val xor : bool -> bool -> bool
val not : bool -> bool

(* operations on integers and natural numbers *)
(* int and nat are identified *)
val neg : int -> int
val abs : int -> int
val isnat : int -> int option
val int : int -> int
val add : int -> int -> int
val sub : int -> int -> int
val mul : int -> int -> int
val ediv : int -> int -> (int * int) option
