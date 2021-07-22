open Syntax
open Trans

let rec read_eval_print () =
  let code = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let exp = string_of_exp (exp_of_code code) in
  print_string "type ('a, 'b) either = Left of 'a | Right of 'b\n";
  print_string "let car (x, y) = x\n";
  print_string "let cdr (x, y) = y\n";
  print_string "let plus (x, y) = x + y\n";
  print_string "let mult (x, y) = x * y\n";
  print_string "let main param_st =\n";
  print_string exp;
  print_newline ()

let _ = read_eval_print ()
