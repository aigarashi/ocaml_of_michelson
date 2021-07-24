(* (untyped) Michelson program *)

type inst =
  | Simple of string
  | SimpleArg1 of string * string (* such as NIL operation *)
  | SimpleArg2 of string * string * Parsetree.expression (* such as PUSH int 0 *)
  | SimpleArgTyTy of string * string * string (* such as EMPTY_BIG_MAP int int *)
  | SimpleWithNum of string * int
  | OneBlock of string * inst list
  | OneBlockWithNum of string * int * inst list
  | TwoBlocks of string * inst list * inst list

type program = 
    Code of inst list

