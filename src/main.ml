open Syntax
open Trans

let rec read_eval_print () =
  let code = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  print_string (string_of_exp (exp_of_code code));
  print_newline ()

let _ = read_eval_print ()
