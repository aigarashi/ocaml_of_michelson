open Asttypes
open Parsetree
open Ast_mapper

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
                  NonLinear -> Trans.let_ [id] inlined_e (mapper.expr mapper inlined_body)
              end
           | other -> default_mapper.expr mapper other }

let linear exp =
  linear_mapper.expr linear_mapper exp
