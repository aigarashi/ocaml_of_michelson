open Syntax
open Trans

let rec read_eval_print () =
  let code = McParser.toplevel McLexer.main (Lexing.from_channel stdin) in
  let exp = exp_of_code code in
  print_string "type ('a, 'b) either = Left of 'a | Right of 'b\n";
  print_string "let car (x, y) = x\n";
  print_string "let cdr (x, y) = y\n";
  print_string "let plus (x, y) = x + y\n";
  print_string "let mult (x, y) = x * y\n";
  print_string "let unit () = ()\n";
  print_string "let main param_st =\n";
  print_string (Pprintast.string_of_expression exp);
  print_newline ()

let _ = read_eval_print ()
