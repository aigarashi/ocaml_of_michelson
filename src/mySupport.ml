open Ast_helper

(* Auxiliary functions to build AST *)
let exp_of_var id =
  Exp.ident (Location.mknoloc (Longident.Lident id))

let pat_of_var id = Pat.var (Location.mknoloc id)

let exp_of_tuple_vars = function
  | [id] -> exp_of_var id
  | ids -> Exp.tuple (List.map (fun id -> exp_of_var id) ids)

let pat_of_tuple_vars = function
  | [id] -> pat_of_var id
  | ids -> Pat.tuple (List.map (fun id -> pat_of_var id) ids)

let let_ ids rhs body =
  Exp.let_ Asttypes.Nonrecursive
    [ { pvb_pat  = pat_of_tuple_vars ids;
        pvb_expr = rhs;
        pvb_attributes = [];
       pvb_loc = Location.none } ]
    body

let letp_ pat rhs body =
  Exp.let_ Asttypes.Nonrecursive
    [ { pvb_pat  = pat;
        pvb_expr = rhs;
        pvb_attributes = [];
       pvb_loc = Location.none } ]
    body

let call id ids =
  match ids with
    [] -> exp_of_var id
  | _ -> Exp.apply (exp_of_var id)
           (List.map (fun id -> Asttypes.Nolabel, exp_of_var id) ids)

let ifleft exp0 var1 exp1 var2 exp2 =
  Exp.match_ exp0
    [ Exp.case
        (Pat.construct
           (Location.mknoloc (Longident.Lident "Left"))
           (Some (pat_of_var var1)))
        exp1
    ; Exp.case
        (Pat.construct
           (Location.mknoloc (Longident.Lident "Right"))
           (Some (pat_of_var var2)))
        exp2
    ]

let ifnone exp0 exp1 var2 exp2 =
  Exp.match_ exp0
    [ Exp.case
        (Pat.construct
           (Location.mknoloc (Longident.Lident "None")) None)
        exp1
    ; Exp.case
        (Pat.construct
           (Location.mknoloc (Longident.Lident "Some"))
           (Some (pat_of_var var2)))
        exp2
    ]

let ifcons exp0 var11 var12 exp1 exp2 =
  Exp.match_ exp0
    [ Exp.case
        (Pat.construct
           (Location.mknoloc (Longident.Lident "::"))
           (Some (Pat.tuple [pat_of_var var11; pat_of_var var12])))
        exp1 ;
      Exp.case
        (Pat.construct
           (Location.mknoloc (Longident.Lident "[]")) None)
        exp2
    ]


let if_ exp0 exp1 exp2 =
  Exp.ifthenelse exp0 exp1 (Some exp2)
