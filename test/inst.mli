type ('a, 'b) either = Left of 'a | Right of 'b
type never

val car : 'a * 'b -> 'a
val cdr : 'a * 'b -> 'b
val plus : int -> int -> int
val mult : int -> int -> int

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
val or_ : bool -> bool -> bool (* keyword! *)
val and_ : bool -> bool -> bool (* keyword! *)
