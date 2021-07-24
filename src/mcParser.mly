%{
    open Syntax
    open Ast_helper
%}

%token CODE LPAREN RPAREN LBRACE RBRACE SEMI EOF

%token <string> INTV
%token <bool> BOOL
%token <string> STR
%token <string> MNEMONIC
%token <string> LCID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    CODE LBRACE is=InstList RBRACE EOF { Code is }

Ty :
    LCID { () }
  | LPAREN Tys RPAREN { () }
Tys :
    Ty { () }
  | Ty Tys { () }

SingleInst :
  | m=MNEMONIC { Simple m }
  | m=MNEMONIC Ty { Simple m }
  | m=MNEMONIC Ty Ty { Simple m }
  | m=MNEMONIC Ty s=STR { SimpleArgCon (m, Exp.constant (Pconst_string (s, Location.none, None))) }
  | m=MNEMONIC Ty i=INTV { SimpleArgCon (m, Exp.constant (Pconst_integer (i, None))) }
  | m=MNEMONIC Ty b=BOOL { SimpleArgCon (m, Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None) }
  | m=MNEMONIC i=INTV { SimpleWithNum (m, int_of_string i) }
  | m=MNEMONIC LBRACE is=InstList RBRACE { OneBlock (m, is) }
  | m=MNEMONIC i=INTV LBRACE is=InstList RBRACE { OneBlockWithNum (m, int_of_string i, is) }
  | m=MNEMONIC LBRACE is1=InstList RBRACE LBRACE is2=InstList RBRACE { TwoBlocks (m, is1, is2) }
                 
InstList :
    /* empty */ { [] }
  | i=SingleInst { [ i ] }
  | i=SingleInst SEMI is=InstList { i :: is }
