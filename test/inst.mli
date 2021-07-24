type ('a, 'b) either = Left of 'a | Right of 'b

val car : 'a * 'b -> 'a
val cdr : 'a * 'b -> 'b
val plus : int * int -> int
val mult : int * int -> int
val unit : unit -> unit
