open Parsetree

let sig_ = Parse.interface (Lexing.from_channel (open_in "../test/inst.mli"))

(* Takes an arrow type and returns the numbers of arguments and results,
    assuming the arrow type is of the form ty1 -> ... -> tyn -> ty'1 * ... * ty'm,
    where ty is not a function type *)

let rec spec_of_ty = function
  | Ptyp_arrow (_, _, {ptyp_desc = res_ty}) ->
     let consume, produce = spec_of_ty res_ty in
     (consume + 1, produce)
  | Ptyp_tuple tys -> (0, List.length tys)
  | _ -> (0, 1)
       
let rec spec_of_item = function
  | [] -> []
  | { psig_desc =
        Psig_value {
            pval_name = name;
            pval_type = { ptyp_desc = ty }
          }
    } :: rest ->
     let (consume, produce) = spec_of_ty ty in
     prerr_string name.txt; prerr_int consume; prerr_string ","; prerr_int produce; prerr_newline();
     (name.txt, spec_of_ty ty) :: spec_of_item rest
  | _ :: rest -> spec_of_item rest

let v = spec_of_item sig_
