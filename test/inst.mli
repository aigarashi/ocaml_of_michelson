type ('a, 'b) or_ = Left of 'a | Right of 'b
type never
type 'a set
type ('a, 'b) pair = 'a * 'b
type ('k, 't) map
type ('k, 't) big_map = ('k, 't) map

val failwith : 'a -> 'b

(* generic comparison *)
val eq : int -> bool
val ge : int -> bool
val gt : int -> bool
val le : int -> bool
val lt : int -> bool
val neq : int -> bool

(* operations on unit *)
val unit : unit
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
val pair : 'a -> 'b -> ('a, 'b) pair
val unpair : ('a, 'b) pair -> 'a * 'b

(* Operations on maps *)
val empty_map : ('k, 'v) map

(* Operations on big_maps *)
val empty_big_map : ('k, 'v) big_map
val get : 'k -> ('k, 'v) big_map -> 'v option
val mem : 'k -> ('k, 'v) big_map -> bool
val update : 'k -> 'v option -> ('k, 'v) big_map -> ('k, 'v) big_map
(* instructions returning multiple values are not supported yet *)
val get_and_update :  'k -> 'v option -> ('k, 'v) big_map -> 'v option * ('k, 'v) big_map

(* Operations on optional values *)
val some : 'a -> 'a option
val none : 'a option

(* Operations on unions *)
val left : 'a -> ('a, 'b) or_
val right : 'b -> ('a, 'b) or_

(* Operations on lists *)
val cons : 'a -> 'a list -> 'a list
val nil : 'a list
val size : 'a list -> nat

(* Domain specific data types *)
type timestamp = int (* for simplicity *)
type mutez = int     (* for simplicity *)
type address = string (* for simplicity *)
type 'a contract
type operation
type key
type key_hash
type signature
type chain_id
type bls12_381_g1 = int
type bls12_381_g2 = int
type bls12_381_fr = int
type 'a sapling_transaction
type 'a sapling_state
(*
ticket (t)
 *)

val transfer_tokens : 'a -> mutez -> 'a contract -> operation
val set_delegate : key_hash option -> operation
val balance : mutez
val address : 'a contract -> address
val contract : address -> 'a contract option
val source : address
val sender : address
val self : 'a contract
val self_address : address
val amount : mutez
val implicit_account : key_hash -> unit contract
val voting_power : key_hash -> nat
val now : timestamp
val chain_id : chain_id
val level : nat
val total_voting_power : nat

(* operations on bytes *)
type bytes = string
val pack : 'a -> bytes
val unpack : bytes -> 'a option
(* concat, size, slice, compare are overleaded *)

(* Cryptographic primitives *)
val hash_key : key -> key_hash
val blake2b : bytes -> bytes
val keccak : bytes -> bytes
val sha256 : bytes -> bytes
val sha512 : bytes -> bytes
val sha3 : bytes -> bytes
val check_siganture : key -> signature -> bytes -> bool

(* BLS12-381 primitives *)
(* neg, add, mul, int are overloaded *)
val paring_check : (bls12_381_g1, bls12_381_g2) pair list -> bool

(* Sapling operations *)
val sapling_verify_update : 'a sapling_transaction -> 'a sapling_state 'a -> (int, 'a sapling_state) pair option
val sapling_empty_state : 'a sapling_state

(* Operations on tickets *)
val ticket : 'a -> nat -> 'a ticket
val read_ticket : 'a ticket -> ('a address, nat) pair * 'a ticket
val split_ticket : 'a ticket -> (nat, nat) pair -> ('a ticket, 'a ticket) pair option
val join_ticket : ('a ticket, 'a ticket) pair -> 'a ticket option

