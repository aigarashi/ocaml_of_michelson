{
let reservedWords = [
  (* Keywords *)
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| ['0'-'9']+
    { McParser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "{" { McParser.LBRACE }
| "}" { McParser.RBRACE }
| ";" { McParser.SEMI }

| "code" { McParser.CODE }
| ['A'-'Z' '_' ]+
    { McParser.MNEMONIC (Lexing.lexeme lexbuf) }
| eof { EOF }


