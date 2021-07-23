(* (untyped) Michelson program *)

type inst =
  | Simple of string
  | SimpleArg1 of string * string (* such as NIL operation *)
  | SimpleArg2 of string * string * Parsetree.expression (* such as PUSH int 0 *)
  | SimpleWithNum of string * int
  | OneBlock of string * inst list
  | TwoBlocks of string * inst list * inst list

type program = 
    Code of inst list

