type ('a, 'b) either = Left of 'a | Right of 'b
type never

(* generic comparison *)
val eq : int -> bool
val ge : int -> bool
val gt : int -> bool
val le : int -> bool
val lt : int -> bool
val neq : int -> bool

(* operations on unit *)
val unit : unit -> unit
val compare : 'a -> 'a -> int

(* operations on booleans *)
(* they are overloaded with operations on ints, hence polymorphic *)
val or_ : 'a -> 'a -> 'a (* 'or' is an OCaml keyword! *)
val and_ : 'a -> 'a -> 'a (* 'and' is an OCaml keyword! *)
val xor : 'a -> 'a -> 'a
val not : 'a -> 'a

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
val lsl_ : int -> int -> int (* keyword *)
val lsr_ : int -> int -> int (* keyword *)

(* operations on strings *)
val concat : string -> string -> string
val size : string -> int
val slice : nat -> nat -> string -> string option

(* Operations on pairs and right combs *)
val car : 'a * 'b -> 'a
val cdr : 'a * 'b -> 'b

