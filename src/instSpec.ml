open Parsetree

let sig_ = Parse.interface (Lexing.from_channel (open_in "../test/inst.mli"))

(* Takes an arrow type and returns the number of arguments,
    assuming the arrow type is of the form ty1 -> ... -> tyn -> ty,
    where ty is not a function type *)

let rec spec_of_ty = function
  | Ptyp_arrow (_, _, {ptyp_desc = res_ty}) -> 1 + spec_of_ty res_ty
  | _ -> 0
       
let rec spec_of_item = function
  | [] -> []
  | { psig_desc =
        Psig_value {
            pval_name = name;
            pval_type = { ptyp_desc = ty }
          }
    } :: rest ->
     prerr_string name.txt; prerr_int (spec_of_ty ty); prerr_newline();
     (name.txt, (spec_of_ty ty, 1)) :: spec_of_item rest
  | _ :: rest -> spec_of_item rest

let v = spec_of_item sig_
