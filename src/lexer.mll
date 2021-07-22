{
let reservedWords = [
  (* Keywords *)
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "{" { Parser.LBRACE }
| "}" { Parser.RBRACE }
| ";" { Parser.SEMI }

| "code" { Parser.CODE }
| ['A'-'Z']+
    { Parser.MNEMONIC (Lexing.lexeme lexbuf) }
| eof { EOF }


