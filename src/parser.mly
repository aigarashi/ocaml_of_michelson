%{
open Syntax
%}

%token CODE LBRACE RBRACE SEMI EOF

%token <int> INTV
%token <string> MNEMONIC

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    CODE LBRACE is=InstList RBRACE EOF { Code is }

SingleInst :
  | m=MNEMONIC { Simple m }
  | m=MNEMONIC LBRACE is=InstList RBRACE { OneBlock (m, is) }
  | m=MNEMONIC LBRACE is1=InstList RBRACE LBRACE is2=InstList RBRACE { TwoBlocks (m, is1, is2) }
                 
InstList :
    /* empty */ { [] }
  | i=SingleInst { [ i ] }
  | i=SingleInst SEMI is=InstList { i :: is }
