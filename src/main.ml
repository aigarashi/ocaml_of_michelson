open Syntax
open Trans

let rec read_eval_print () =
  let code = McParser.toplevel McLexer.main (Lexing.from_channel stdin) in
  let v = [exp_of_code code] in
  print_string "open Inst\n";
  print_string (Pprintast.string_of_structure v);
  print_newline ()

let _ = read_eval_print ()
