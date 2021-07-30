open Asttypes
open Parsetree
open Ast_mapper
open MySupport
open Ast_helper

exception NonLinear

let rec subst_mapper id e seen =
  { default_mapper with
    expr = fun mapper expr ->
           match expr with
           | { pexp_desc = Pexp_ident {txt=Lident id'} } when id = id' ->
              if !seen then raise NonLinear
              else seen := true; e
           | other -> default_mapper.expr mapper other }

let subst id e expr =
  let seen = ref false in
  let mapper = subst_mapper id e seen in
  mapper.expr mapper expr

let rec simultaneous_subst pats rhss body =
  assert (List.length pats = List.length rhss);
  match pats, rhss with
  | [], [] -> ([], [], body)
  | ({ppat_desc = Ppat_var {txt=id}} as pat) :: restpats, expr :: restrhss ->
     let newpats, newrhss, newbody = simultaneous_subst restpats restrhss body in
     begin try newpats, newrhss, subst id expr newbody with
             NonLinear -> pat :: newpats, expr :: newrhss, newbody
     end
  | pat :: restpats, expr :: restrhss ->
     let newpats, newrhss, newbody = simultaneous_subst restpats restrhss body in
     pat :: newpats, expr :: newrhss, newbody

let linear_mapper =
  { default_mapper with
    expr = fun mapper expr ->
           match expr with
           | { pexp_desc =
                 Pexp_let (Nonrecursive,
                           [{pvb_pat = {ppat_desc = Ppat_var {txt=id}};
                             pvb_expr = e;}],
                           body) } ->
              let inlined_body = mapper.expr mapper body in
              let inlined_e = mapper.expr mapper e in
              begin
                try subst id inlined_e inlined_body with
                  NonLinear -> MySupport.let_ [id] inlined_e (mapper.expr mapper inlined_body)
              end
           | { pexp_desc =
                 Pexp_let (Nonrecursive,
                           [{pvb_pat = {ppat_desc = Ppat_tuple pats};
                             pvb_expr = {pexp_desc = Pexp_tuple rhss}}],
                           body) } ->
              let inlined_body = mapper.expr mapper body in
              let inlined_rhss = List.map (mapper.expr mapper) rhss in
              let newpats, newrhss, newbody = simultaneous_subst pats inlined_rhss inlined_body in
              Exp.let_ Nonrecursive
                [ { pvb_pat = (match newpats with [pat] -> pat | _ -> Pat.tuple newpats);
                    pvb_expr = (match newrhss with [rhs] -> rhs | _ -> Exp.tuple newrhss);
                    pvb_attributes = [];
                    pvb_loc = Location.none } ]
                newbody
           | other -> default_mapper.expr mapper other }

let linear expr =
  linear_mapper.expr linear_mapper expr

let remove_trivial_let_mapper =
  { default_mapper with
    expr = fun mapper expr ->
           match expr with
           | { pexp_desc =
                 Pexp_let (Nonrecursive,
                           [{pvb_pat = {ppat_desc = Ppat_tuple []};
                             pvb_expr ={pexp_desc = Pexp_tuple []};}],
                           body) } ->
              (* let () = () in e  ==> e *)
              mapper.expr mapper body
           | { pexp_desc =
                 Pexp_let (Nonrecursive,
                           [{pvb_pat = {ppat_desc = Ppat_tuple []};
                             pvb_expr = rhs}],
                           body) } ->
              (* let () = e1 in e2  ==> e1; e2 *)
              let rhs' = mapper.expr mapper rhs in
              let body' = mapper.expr mapper body in
              (match body' with
                 (* let () = e in ()  ==> e *)
                 { pexp_desc = Pexp_tuple [] } -> rhs'
               | _ -> Exp.sequence rhs' body')
           | other -> default_mapper.expr mapper other }

let remove_trivial_let expr =
  remove_trivial_let_mapper.expr remove_trivial_let_mapper expr

let tidy_up_if_mapper =
  { default_mapper with
    expr = fun mapper expr ->
           match expr with
           | { pexp_desc = Pexp_ifthenelse (expr1, expr2, Some expr3) } ->
              begin match mapper.expr mapper expr2 with
              | { pexp_desc = Pexp_tuple [] } ->
                 let expr1 = Exp.apply (exp_of_var "not") [Asttypes.Nolabel, expr1] in
                 Exp.ifthenelse expr1 (mapper.expr mapper expr3) None
              | expr2' ->
                 begin match mapper.expr mapper expr3 with
                 | { pexp_desc = Pexp_tuple [] } ->
                    Exp.ifthenelse expr1 expr2' None
                 | expr3' -> Exp.ifthenelse expr1 expr2' (Some expr3')
                 end
              end
           | other -> default_mapper.expr mapper other }

let tidy_up_if expr =
  tidy_up_if_mapper.expr tidy_up_if_mapper expr
