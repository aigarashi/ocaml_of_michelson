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
type nat = int

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


(* Domain specific data types *)
type timestamp = int (* for simplicity *)
type mutez = int     (* for simplicity *)
type address
type 'a contract
type operation
type key
type key_hash
type signature
type chain_id
type bls12_381_g1
type bls12_381_g2
type bls12_381_fr
(* 
sapling_transaction ms: A Sapling transaction
sapling_state ms: A Sapling state
ticket (t)
 *)

val transfer_tokens : 'a -> mutez -> 'a contract -> operation
val set_delegate : key_hash option -> operation
val balance : unit -> mutez
val address : 'a contract -> address
val contract : address -> 'a contract option
val source : unit -> address
val sender : unit -> address
val self : unit -> 'a contract
val self_address : unit -> address
val amount : unit -> mutez
val implicit_account : key_hash -> unit contract
val voting_power : key_hash -> nat
val now : unit -> timestamp
val chain_id : unit -> chain_id
val level : unit -> nat
val total_voting_power : unit -> nat

(* operations on bytes *)
type bytes

(* Cryptographic primitives *)
val hash_key : key -> key_hash
val blake2b : bytes -> bytes
