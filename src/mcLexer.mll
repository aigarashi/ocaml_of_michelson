{
let reservedWords = [
  (* Keywords *)
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| ['0'-'9']+
    { McParser.INTV (Lexing.lexeme lexbuf) }

| "{" { McParser.LBRACE }
| "}" { McParser.RBRACE }
| ";" { McParser.SEMI }

| "code" { McParser.CODE }
| ['A'-'Z' '_' ]+
    { McParser.MNEMONIC (Lexing.lexeme lexbuf) }
| ['a'-'z']+
    { McParser.LCID (Lexing.lexeme lexbuf) }
| '"' (_ # '"')* '"'
    { let lexeme = Lexing.lexeme lexbuf in
       McParser.STR (String.sub lexeme 1 (String.length lexeme - 2))
    }
| eof { EOF }
