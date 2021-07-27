type int_ = int
type bool_ = bool
type unit_ = unit
type string_ = string
type ('a, 'b) or_ = Left of 'a | Right of 'b
type 'a option_ = 'a option
type 'a list_ = 'a list
type never_
type 'a set_
type ('a, 'b) pair_ = 'a * 'b
type ('k, 't) map_
type ('k, 't) big_map_ = ('k, 't) map_

val failwith : 'a -> 'b

(* generic comparison *)
val eq : int_ -> bool_
val ge : int_ -> bool_
val gt : int_ -> bool_
val le : int_ -> bool_
val lt : int_ -> bool_
val neq : int_ -> bool_

(* operations on unit *)
val unit : unit_
val compare : 'a -> 'a -> int_

(* operations on booleans *)
(* they are overloaded with operations on ints, hence polymorphic *)
val or_ : 'a -> 'a -> 'a (* 'or' is an OCaml keyword! *)
val and_ : 'a -> 'a -> 'a (* 'and' is an OCaml keyword! *)
val xor : 'a -> 'a -> 'a
val not : 'a -> 'a

(* operations on integers and natural numbers *)
(* int and nat are identified *)
type nat_ = int_

val neg : int_ -> int_
val abs : int_ -> int_
val isnat : int_ -> int_ option_
val int : int_ -> int_
val add : int_ -> int_ -> int_
val sub : int_ -> int_ -> int_
val mul : int_ -> int_ -> int_
val ediv : int_ -> int_ -> (int_ * int_) option_
val lsl_ : int_ -> int_ -> int_ (* keyword *)
val lsr_ : int_ -> int_ -> int_ (* keyword *)

(* operations on strings *)
val concat : string_ -> string_ -> string_
val size : string_ -> int_
val slice : nat_ -> nat_ -> string_ -> string_ option_

(* Operations on pairs and right combs *)
val car : 'a * 'b -> 'a
val cdr : 'a * 'b -> 'b
val pair : 'a -> 'b -> ('a, 'b) pair_
val unpair : ('a, 'b) pair_ -> 'a * 'b

(* Operations on maps *)
val empty_map : ('k, 'v) map_

(* Operations on big_maps *)
val empty_big_map : ('k, 'v) big_map_
val get : 'k -> ('k, 'v) big_map_ -> 'v option_
val mem : 'k -> ('k, 'v) big_map_ -> bool_
val update : 'k -> 'v option_ -> ('k, 'v) big_map_ -> ('k, 'v) big_map_
(* instructions returning multiple values are not supported yet *)
val get_and_update :  'k -> 'v option_ -> ('k, 'v) big_map_ -> 'v option_ * ('k, 'v) big_map_

(* Operations on optional values *)
val some : 'a -> 'a option_
val none : 'a option_

(* Operations on unions *)
val left : 'a -> ('a, 'b) or_
val right : 'b -> ('a, 'b) or_

(* Operations on lists *)
val cons : 'a -> 'a list_ -> 'a list_
val nil : 'a list_
val size : 'a list -> nat_

(* Domain specific data types *)
type timestamp_ = int_ (* for simplicity *)
type mutez_ = int_     (* for simplicity *)
type address_ = string_ (* for simplicity *)
type 'a contract_
type operation_
type key_
type key_hash_
type signature_
type chain_id_
type bls12_381_g1_ = int_
type bls12_381_g2_ = int_
type bls12_381_fr_ = int_
type 'a sapling_transaction_
type 'a sapling_state_
type 'a ticket_

val transfer_tokens : 'a -> mutez_ -> 'a contract_ -> operation_
val set_delegate : key_hash_ option_ -> operation_
val balance : mutez_
val address : 'a contract_ -> address_
val contract : address_ -> 'a contract_ option_
val source : address_
val sender : address_
val self : 'a contract_
val self_address : address_
val amount : mutez_
val implicit_account : key_hash_ -> unit_ contract_
val voting_power : key_hash_ -> nat_
val now : timestamp_
val chain_id : chain_id_
val level : nat_
val total_voting_power : nat_

(* operations on bytes *)
type bytes_ = string_
val pack : 'a -> bytes_
val unpack : bytes_ -> 'a option_
(* concat, size, slice, compare are overleaded *)

(* Cryptographic primitives *)
val hash_key : key_ -> key_hash_
val blake2b : bytes_ -> bytes_
val keccak : bytes_ -> bytes_
val sha256 : bytes_ -> bytes_
val sha512 : bytes_ -> bytes_
val sha3 : bytes_ -> bytes_
val check_signature : key_ -> signature_ -> bytes_ -> bool_

(* BLS12-381 primitives *)
(* neg, add, mul, int are overloaded *)
val paring_check : (bls12_381_g1_, bls12_381_g2_) pair_ list_ -> bool_

(* Sapling operations *)
val sapling_verify_update : 'a sapling_transaction_ -> 'a sapling_state_ -> (int_, 'a sapling_state_) pair_ option_
val sapling_empty_state : 'a sapling_state_

(* Operations on tickets *)
val ticket : 'a -> nat_ -> 'a ticket_
val read_ticket : 'a ticket_ -> ('a, nat_) pair_ * 'a ticket_
val split_ticket : 'a ticket_ -> (nat_, nat_) pair_ -> ('a ticket_, 'a ticket_) pair_ option_
val join_ticket : ('a ticket_, 'a ticket_) pair_ -> 'a ticket_ option_

