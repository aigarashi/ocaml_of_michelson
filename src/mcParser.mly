%{
    open Syntax
    open Ast_helper
%}

%token CODE LBRACE RBRACE SEMI EOF

%token <string> INTV
%token <string> MNEMONIC
%token <string> LCID
%token <string> STR

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    CODE LBRACE is=InstList RBRACE EOF { Code is }

SingleInst :
  | m=MNEMONIC { Simple m }
  | m=MNEMONIC ty=LCID { SimpleArg1 (m, ty) }
  | m=MNEMONIC ty=LCID s=STR { SimpleArg2 (m, ty, Exp.constant (Pconst_string (s, Location.none, None))) }
  | m=MNEMONIC ty=LCID i=INTV { SimpleArg2 (m, ty, Exp.constant (Pconst_integer (i, None))) }
  | m=MNEMONIC LBRACE is=InstList RBRACE { OneBlock (m, is) }
  | m=MNEMONIC LBRACE is1=InstList RBRACE LBRACE is2=InstList RBRACE { TwoBlocks (m, is1, is2) }
                 
InstList :
    /* empty */ { [] }
  | i=SingleInst { [ i ] }
  | i=SingleInst SEMI is=InstList { i :: is }
