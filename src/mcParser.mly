%{
    open Syntax
    open Ast_helper
%}

%token PARAM STORAGE CODE LPAREN RPAREN LBRACE RBRACE SEMI EOF

%token <string> INTV
%token <bool> BOOL
%token <string> STR
%token <string> MNEMONIC
%token <string> LCID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
  | sc=Script EOF { sc }

Script:
  | CODE LBRACE is=InstList RBRACE { Code (None, is) }
  | PARAM pty=Ty SEMI STORAGE stty=Ty SEMI CODE LBRACE is=InstList RBRACE { Code (Some (pty, stty), is) }

Ty :
    ty=LCID { Typ.constr (Location.mknoloc (Longident.Lident ty)) [] }
  | LPAREN ty=LCID tys=Tys RPAREN {
      let ty = if ty = "or" then "or_" else ty in
      Typ.constr (Location.mknoloc (Longident.Lident ty)) tys
    }
Tys :
    /* empty */ { [] }
  | ty=Ty tys=Tys { ty::tys }

SingleInst :
  | m=MNEMONIC { Simple m }
  | m=MNEMONIC Ty { Simple m }
  | m=MNEMONIC Ty Ty { Simple m }
  | m=MNEMONIC Ty s=STR { SimpleArgCon (m, Exp.constant (Pconst_string (s, Location.none, None))) }
  | m=MNEMONIC Ty i=INTV { SimpleArgCon (m, Exp.constant (Pconst_integer (i, None))) }
  | m=MNEMONIC Ty b=BOOL { SimpleArgCon (m, Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None) }
  | m=MNEMONIC i=INTV { SimpleWithNum (m, int_of_string i) }
  | m=MNEMONIC LBRACE is=InstList RBRACE { OneBlock (m, is) }
  | m=MNEMONIC ty1=Ty ty2=Ty LBRACE is=InstList RBRACE { OneBlockWithTwoTys (m, ty1, ty2, is) }
  | m=MNEMONIC i=INTV LBRACE is=InstList RBRACE { OneBlockWithNum (m, int_of_string i, is) }
  | m=MNEMONIC LBRACE is1=InstList RBRACE LBRACE is2=InstList RBRACE { TwoBlocks (m, is1, is2) }
  | m=MNEMONIC LBRACE sc=Script RBRACE { CreateContract (m, sc) }
  | LBRACE is=InstList RBRACE { Block is }
  | m=MNEMONIC error { prerr_string m; exit 1 }

InstList :
    /* empty */ { [] }
  | i=SingleInst { [ i ] }
  | i=SingleInst SEMI is=InstList { i :: is }
