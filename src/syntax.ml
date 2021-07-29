(* (untyped) Michelson program *)

type inst =
  | Simple of string
  | SimpleArgCon of string * Parsetree.expression (* such as PUSH int 0 *)
  | SimpleWithNum of string * int
  | OneBlock of string * inst list  (* such as DIP code *)
  | OneBlockWithNum of string * int * inst list  (* such as DIP 3 code *)
  | OneBlockWithTwoTys of string * Parsetree.core_type * Parsetree.core_type * inst list  (* such as LAMBDA ty ty code *)
  | TwoBlocks of string * inst list * inst list  (* such IF code1 code 2 *)

type program = 
    Code of (Parsetree.core_type * Parsetree.core_type) option * inst list

