{
let reservedWords = [
  (* Keywords *)
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }
| ['%' ':'] ['A'-'Z' 'a'-'z' '_']+  { main lexbuf }
| '#' (_ # '\n')* '\n' { main lexbuf }

| "-"? ['0'-'9']+
    { McParser.INTV (Lexing.lexeme lexbuf) }
| "-"? "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
    { McParser.INTV (Lexing.lexeme lexbuf) }

| "(" { McParser.LPAREN }
| ")" { McParser.RPAREN }
| "{" { McParser.LBRACE }
| "}" { McParser.RBRACE }
| ";" { McParser.SEMI }

| "code" { McParser.CODE }
| "parameter" { McParser.PARAM }
| "storage" { McParser.STORAGE }
| "True" { McParser.BOOL true }
| "False" { McParser.BOOL false }
| "Unit" { McParser.UNIT }

| ['A'-'Z' '_' ]+
    { McParser.MNEMONIC (Lexing.lexeme lexbuf) }
| ['a'-'z' '_' ]+
    { McParser.LCID (Lexing.lexeme lexbuf) }
| '"' (_ # '"')* '"'
    { let lexeme = Lexing.lexeme lexbuf in
       McParser.STR (String.sub lexeme 1 (String.length lexeme - 2))
    }
| eof { EOF }
| _ { prerr_string "Lexer: Unknown character: "; prerr_string (Lexing.lexeme lexbuf); exit 0 }