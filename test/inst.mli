type ('a, 'b) either = Left of 'a | Right of 'b

val car : 'a * 'b -> 'a
val cdr : 'a * 'b -> 'b
val plus : int -> int -> int
val mult : int -> int -> int
val unit : unit -> unit

val eq : int -> bool
val ge : int -> bool
val gt : int -> bool
val le : int -> bool
val lt : int -> bool
val neq : int -> bool
val compare : 'a -> 'a -> bool
