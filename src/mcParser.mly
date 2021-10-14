%{
    open Syntax
    open Ast_helper
    open MySupport
#if OCAML_VERSION >= (4, 11, 0)
#define PCONST_STRING(s, l, o) Pconst_string(s, l, o)
#else
#define PCONST_STRING(s, l, o) Pconst_string(s, o)
#endif
%}

%token PARAM STORAGE CODE LPAREN RPAREN LBRACE RBRACE SEMI EOF
%token UNIT PAIR LEFT RIGHT SOME NONE ELT

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

Script :
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

Literal :
  | s=STR { Exp.constant (PCONST_STRING(s, Location.none, None)) }
  | i=INTV { Exp.constant (Pconst_integer (i, None)) }
  | b=BOOL { Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool b))) None }
  | UNIT { Exp.tuple [] }
  | LBRACE l=SemiLits RBRACE { l }  /* list/set literal; pair could be of the same form but we ignore */
  | LBRACE kvs=KVLists RBRACE
    {
      Exp.apply (exp_of_var "map_of_assoc") [Asttypes.Nolabel, kvs]
    }
  | NONE { Exp.construct (Location.mknoloc (Longident.Lident "None")) None }
  | SOME l=Literal { Exp.construct (Location.mknoloc (Longident.Lident "Some")) (Some l) }
  | LPAREN LEFT l=Literal RPAREN { Exp.construct (Location.mknoloc (Longident.Lident "Left")) (Some l) }
  | LPAREN RIGHT l=Literal RPAREN { Exp.construct (Location.mknoloc (Longident.Lident "Right")) (Some l) }
  | LPAREN PAIR ls=Lits RPAREN { ls }

SemiLits :
  | /* empty */
    { Exp.construct (Location.mknoloc (Longident.Lident "[]")) None }
  | l=Literal 
    {
      Exp.construct
        (Location.mknoloc (Longident.Lident "::"))
        (Some (Exp.tuple [l; Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))
    }
  | l=Literal SEMI ls=SemiLits
    {
      Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [l; ls]))
    }

OptSEMI : /* empty */ { () } | SEMI { () }
KVLists : /* Cannot be empty to distinguish from the empty list */
  | ELT l1=Literal l2=Literal OptSEMI
    { Exp.construct
        (Location.mknoloc (Longident.Lident "::"))
        (Some (Exp.tuple [Exp.tuple [l1; l2];
                          Exp.construct (Location.mknoloc (Longident.Lident "[]")) None]))
    }
  | ELT l1=Literal l2=Literal SEMI ls=KVLists
    {
      Exp.construct (Location.mknoloc (Longident.Lident "::")) (Some (Exp.tuple [Exp.tuple [l1; l2]; ls]))
    }

Lits :
  | l1=Literal l2=Literal { Exp.tuple [l1; l2] }
  | l=Literal ls=Lits { Exp.tuple [l; ls] }

SingleInst :
  | m=MNEMONIC { Simple m }
  | m=MNEMONIC Ty { Simple m }
  | m=MNEMONIC Ty Ty { Simple m }
  | m=MNEMONIC Ty l=Literal { SimpleArgCon (m, l) }
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
