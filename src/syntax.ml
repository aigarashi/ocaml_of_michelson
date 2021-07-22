(* (untyped) Michelson program *)

type inst =
  | Simple of string
  | OneBlock of string * inst list
  | TwoBlocks of string * inst list * inst list

type program = 
    Code of inst list

