open Asttypes
open Parsetree
open Ast_mapper
open MySupport
open Ast_helper

exception NonLinear
exception Found

let rec occur_mapper id =
  { default_mapper with
    expr = fun mapper expr ->
           match expr with
           | { pexp_desc = Pexp_ident {txt=Lident id'} } when id = id' ->
              raise Found
           | other -> default_mapper.expr mapper other }

let occur id expr =
  let mapper = occur_mapper id in
  try
    ignore (mapper.expr mapper expr);
    false
  with
    Found -> true

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
     (* prerr_string ("simultaneous_subst: trying " ^ id ^ "\n"); *)
     let newpats, newrhss, newbody = simultaneous_subst restpats restrhss body in
     begin try newpats, newrhss, subst id expr newbody with
             NonLinear -> (* prerr_string (id ^ " is used more than once.\n"); *)
                          pat :: newpats, expr :: newrhss, newbody
     end
  | pat :: restpats, expr :: restrhss ->
     let newpats, newrhss, newbody = simultaneous_subst restpats restrhss body in
     pat :: newpats, expr :: newrhss, newbody

let simplify_let_mapper =
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
                           [{pvb_pat = {ppat_desc = Ppat_tuple []};
                             pvb_expr = rhs}],
                           body) } ->
              (* let () = e1 in e2 *)
              let rhs' = mapper.expr mapper rhs in
              let body' = mapper.expr mapper body in
              (match rhs', body' with
               | (* let () = () in e ==> e *)
                 { pexp_desc = Pexp_tuple [] }, _ -> body'
               | (* let () = e in ()  ==> e *)
                 _, { pexp_desc = Pexp_tuple [] } -> rhs'
               | (* let () = e1 in e2 ==> e1; e2 *)
                 _ -> Exp.sequence rhs' body')
           | { pexp_desc =
                 Pexp_let (Nonrecursive,
                           [{pvb_pat = {ppat_desc = Ppat_tuple pats};
                             pvb_expr = {pexp_desc = Pexp_tuple rhss}}],
                           body) } ->
              let inlined_body = mapper.expr mapper body in
              let inlined_rhss = List.map (mapper.expr mapper) rhss in
              (* DEBUG
              if List.length pats > 0 then
                prerr_string ("linear_mapper: let (" ^ (string_of_int (List.length pats)) ^ ") = ...\n");
              List.iter (Pprintast.pattern Format.err_formatter) pats;
               *)
              let newpats, newrhss, newbody = simultaneous_subst pats inlined_rhss inlined_body in
              begin match newpats with
              | [] -> newbody
              | _ -> Exp.let_ Nonrecursive
                       [ { pvb_pat = (match newpats with [pat] -> pat | _ -> Pat.tuple newpats);
                           pvb_expr = (match newrhss with [rhs] -> rhs | _ -> Exp.tuple newrhss);
                           pvb_attributes = [];
                           pvb_loc = Location.none } ]
                       newbody
              end
           | other -> default_mapper.expr mapper other }

let simplify_let expr =
  simplify_let_mapper.expr simplify_let_mapper expr

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
