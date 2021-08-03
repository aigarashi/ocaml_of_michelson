open Ast_helper
open Syntax
open Trans
open MySupport

let rec read_eval_print () =
  let code = McParser.toplevel McLexer.main (Lexing.from_channel stdin) in
  let v = [ Str.value Asttypes.Nonrecursive
              [{ pvb_pat = pat_of_var "main";
                 pvb_expr = exp_of_code code;
                 pvb_attributes = [];
                 pvb_loc = Location.none } ] ]
  in
  print_string "open Inst\n";
  print_string (Pprintast.string_of_structure v);
  print_newline ()

let _ = read_eval_print ()
