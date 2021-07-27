open Syntax
open MySupport
open Ast_helper

let newVar =
  let counter = ref (-1) in
  fun () -> counter := !counter + 1; "x" ^ (string_of_int !counter)

let rec newVars n =
  if n = 0 then []
  else let var = newVar () in var :: newVars (n-1)

let rec take n l =
  if n = 0 then []
  else match l with [] -> failwith "Take" | h :: t -> h :: take (n-1) t

let rec drop n l =
  if n = 0 then l
  else match l with [] -> failwith "Take" | h :: t -> drop (n-1) t

(* pretty printing *)
let rec string_of_ids = function
  | [] -> "()"
  | [x] -> x
  | x :: xs -> x ^ ", " ^ string_of_ids xs

(* compute diffs of two var lists *)
(* diff [x1, ..., xn, y1, ..., ym] [z1, .., zk, y1, ..., ym] = [x1, ..., xn] *)
let diff vars1 vars2 =
  let rev_vars1 = List.rev vars1 in
  let rev_vars2 = List.rev vars2 in
  let rec diff_aux = function
    | x :: vars1, y :: vars2 when x = y -> diff_aux (vars1, vars2)
    | [], vars2 -> []
    | vars1, _ -> vars1
  in List.rev (diff_aux (rev_vars1, rev_vars2))

let init_kont exp = exp

let find_spec =
  (* the instruction name maybe a keyword, in which case an underscore
     is added to the function name in test/inst.mli *)
  fun s -> try (s, List.assoc s InstSpec.v) with
           | Not_found -> let s = s ^ "_" in (s, List.assoc s InstSpec.v)


(* translator exp_of_prog
 kont represents instructions already processed *)
let rec exp_of_prog kont = function
  | [], vars -> (kont, Some vars)
  (* CAST is no-op *)
  | Simple "CAST" :: rest, vars -> exp_of_prog kont (rest, vars)
  (* PUSH *)
  | SimpleArgCon ("PUSH", c) :: rest, vars ->
     let var0 = newVar() in
     exp_of_prog
       (fun exp ->
         kont (let_ [var0] c exp))
       (rest, var0 :: vars)
  (* FAILWITH is a special instruction *)
  | Simple "FAILWITH" :: rest, var0 :: vars ->
     ((fun exp -> kont (call "failwith" [var0])), None)
  (* DUP duplicates name of the nth element of the stack *)
  | Simple "DUP" :: rest, vars ->
     exp_of_prog kont (SimpleWithNum ("DUP", 1) :: rest, vars)
  | SimpleWithNum ("DUP", n) :: rest, vars ->
     (* DEBUG *)
     prerr_string (string_of_ids vars); prerr_string " DUP"; prerr_newline();
     (* DEBUG *)
     assert(n >= 1 && List.length vars >= n);
     exp_of_prog kont (rest, List.nth vars (n-1) :: vars)
  | Simple "SWAP" :: rest, var1 :: var2 :: vars ->
     (* DEBUG *)
     prerr_string (string_of_ids (var1::var2::vars)); prerr_string " SWAP"; prerr_newline();
     (* DEBUG *)
     exp_of_prog kont (rest, var2 :: var1 :: vars)
  (* DIP needs special treatment *)
  | OneBlock ("DIP", is) :: rest, vars ->
     exp_of_prog kont (OneBlockWithNum ("DIP", 1, is) :: rest, vars)
  | OneBlockWithNum ("DIP", n, is) :: rest, vars ->
     (* DEBUG *)
     prerr_string (string_of_ids vars); prerr_string " DIP ";
     prerr_string (string_of_int n); prerr_newline();
     (* DEBUG END *)
     if n = 0 then exp_of_prog kont (rest, vars)
     else
       let protected, restvars = take n vars, drop n vars in
       let kont_body, final_vars = exp_of_prog init_kont (is, restvars) in
       begin match final_vars with
       | None -> ((fun exp -> kont (kont_body exp)), None)
       | Some final_vars ->
          (* DEBUG *)
          prerr_string ("INIT: "^string_of_ids vars); prerr_newline();
          prerr_string ("FINAL: "^string_of_ids final_vars); prerr_newline();
          (* DEBUG END *)
          let rec aux = function (* replace newly introduced vars in finalvars with fresh vars for the continuation *)
            | [] -> [], [], []
            | id :: rest_final_vars ->
               let freshvars, newvars, updated_final_vars = aux rest_final_vars in
               if List.mem id vars then freshvars, newvars, id::updated_final_vars
               else
                 let freshvar = newVar () in
                 freshvar :: freshvars, id :: newvars, freshvar :: updated_final_vars           in
          let freshvars, newvars, updated_final_vars = aux final_vars in
          exp_of_prog
            (fun exp -> kont (let_ freshvars
                                (kont_body (exp_of_tuple_vars newvars))
                                exp))
            (rest, protected @ updated_final_vars) end
  (* DIG *)
  | SimpleWithNum ("DIG", n) :: rest, vars ->
     (* DEBUG *)
     prerr_string (string_of_ids vars); prerr_string " DIG "; prerr_string (string_of_int n); prerr_newline();
     (* DEBUG *)
     assert(n >= 0 && List.length vars >= n+1);
     exp_of_prog kont (rest, List.nth vars n :: take n vars @ drop (n+1) vars)
  (* DUG *)
  | SimpleWithNum ("DUG", n) :: rest, var1 :: vars ->
     (* DEBUG *)
     prerr_string (string_of_ids (var1::vars)); prerr_string " DUG "; prerr_string (string_of_int n); prerr_newline();
     (* DEBUG *)
     assert(n >= 0 && List.length vars >= n);
     exp_of_prog kont (rest, take n vars @ var1 :: drop n vars)
  (* branching instructions *)
  | TwoBlocks ("IF_LEFT", is1, is2) :: rest, var0 :: vars ->
     let var1 = newVar () in
     let kont_body1, final_vars1 = exp_of_prog init_kont (is1, var1::vars) in
     let var2 = newVar () in
     let kont_body2, final_vars2 = exp_of_prog init_kont (is2, var2::vars) in
     gen_branch kont rest var0 vars kont_body1 final_vars1 kont_body2 final_vars2
       (fun exp0 exp1 exp2 -> ifleft exp0 var1 exp1 var2 exp2)
  | TwoBlocks ("IF", is1, is2) :: rest, var0 :: vars ->
     let kont_body1, final_vars1 = exp_of_prog init_kont (is1, vars) in
     let kont_body2, final_vars2 = exp_of_prog init_kont (is2, vars) in
     gen_branch kont rest var0 vars kont_body1 final_vars1 kont_body2 final_vars2 if_
  | TwoBlocks ("IF_NONE", is1, is2) :: rest, var0 :: vars ->
     let kont_body1, final_vars1 = exp_of_prog init_kont (is1, vars) in
     let var2 = newVar () in
     let kont_body2, final_vars2 = exp_of_prog init_kont (is2, var2::vars) in
     gen_branch kont rest var0 vars kont_body1 final_vars1 kont_body2 final_vars2
       (fun exp0 exp1 exp2 -> ifnone exp0 exp1 var2 exp2)
  | TwoBlocks ("IF_CONS", is1, is2) :: rest, var0 :: vars ->
     let var11 = newVar () and var12 = newVar() in
     let kont_body1, final_vars1 = exp_of_prog init_kont (is1, var11::var12::vars) in
     let kont_body2, final_vars2 = exp_of_prog init_kont (is2, vars) in
     gen_branch kont rest var0 vars kont_body1 final_vars1 kont_body2 final_vars2
       (fun exp0 exp1 exp2 -> ifcons exp0 var11 var12 exp1 exp2)
  (* DROP *)
  | Simple "DROP" :: rest, vars -> exp_of_prog kont (SimpleWithNum ("DROP", 1) :: rest, vars)
  | SimpleWithNum ("DROP", n) :: rest, vars ->
     assert (n >= 0 && List.length vars >= n);
     exp_of_prog kont (rest, drop n vars)
  (* General instructions consuming n values and producing m values *)
  | Simple s :: rest, vars -> begin
      try
        let funname, (n, m) = find_spec (String.lowercase_ascii s) in
        (* DEBUG *)
        prerr_string (string_of_ids vars); prerr_string funname; prerr_newline();
        (* DEBUG *)
        assert(List.length vars >= n);
        let consumed_vars, untouched_vars = take n vars, drop n vars in
        let produced_vars = newVars m in
        exp_of_prog
          (fun exp ->
            kont (let_ produced_vars (call funname consumed_vars) exp))
          (rest, produced_vars @ untouched_vars)
     with Not_found -> failwith ("Instruction not implemented: " ^ s)
     end
  and gen_branch kont rest var0 vars kont_body1 final_vars1 kont_body2 final_vars2 branch =
  match final_vars1, final_vars2 with
  | None, None ->
     ((fun exp -> kont (branch (exp_of_var var0)
                          (kont_body1 (exp_of_tuple_vars []))
                          (kont_body2 (exp_of_tuple_vars [])))),
      None)
  | Some final_vars1, None ->
     let newvars1 = diff final_vars1 vars in
     let num_newvars = List.length newvars1 in
     let newvars = newVars num_newvars in
     exp_of_prog
       (fun exp -> kont (let_ newvars
                           (branch (exp_of_var var0)
                              (kont_body1 (exp_of_tuple_vars newvars1))
                              (kont_body2 (exp_of_tuple_vars [])))
                           exp))
       (rest, newvars @ drop num_newvars final_vars1)
  | None, Some final_vars2 ->
     let newvars2 = diff final_vars2 vars in
     let num_newvars = List.length newvars2 in
     let newvars = newVars num_newvars in
     exp_of_prog
       (fun exp -> kont (let_ newvars
                           (branch (exp_of_var var0)
                              (kont_body1 (exp_of_tuple_vars []))
                              (kont_body2 (exp_of_tuple_vars newvars2)))
                           exp))
       (rest, newvars @ drop num_newvars final_vars2)
  | Some final_vars1, Some final_vars2 ->
     let newvars1 = diff final_vars1 vars in
     let newvars2 = diff final_vars2 vars in
     let num_newvars = (max (List.length newvars1) (List.length newvars2)) in
     let newvars = newVars num_newvars in
     let newvars1 = take num_newvars final_vars1 in
     let newvars2 = take num_newvars final_vars2 in
     exp_of_prog
       (fun exp -> kont (let_ newvars
                           (branch (exp_of_var var0)
                              (kont_body1 (exp_of_tuple_vars newvars1))
                              (kont_body2 (exp_of_tuple_vars newvars2)))
                           exp))
       (rest, newvars @ drop num_newvars final_vars1)
    
let exp_of_code (Code (optty, body)) =
  let kont, ids = exp_of_prog init_kont (body, ["param_st"]) in
  let body =
    Inliner.tidy_up_if 
      (Inliner.remove_trivial_let
         (Inliner.linear
            (match ids with
             | Some [] -> failwith "exp_of_code: Stack is empty!"
             | Some (x :: _) -> kont (exp_of_tuple_vars [x])
             | None -> kont (exp_of_tuple_vars [])))) in
  let body =
    match optty with
    | None -> body
    | Some (_, st_ty) -> Exp.constraint_ body (Typ.tuple [Typ.constr (Location.mknoloc (Longident.Lident "list_"))
                                                            [Typ.constr (Location.mknoloc (Longident.Lident "operation_")) []];
                                                          st_ty]) in
  let param =
    match optty with
      None -> pat_of_var "param_st"
    | Some (param_ty, st_ty) ->
       Pat.constraint_ (pat_of_var "param_st") (Typ.tuple [param_ty; st_ty])
  in
  Str.value Asttypes.Nonrecursive
    [{ pvb_pat = pat_of_var "main";
       pvb_expr = Exp.fun_ Asttypes.Nolabel None param body;
       pvb_attributes = [];
       pvb_loc = Location.none } ]

