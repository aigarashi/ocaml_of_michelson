open Syntax 

let newVar =
  let counter = ref (-1) in
  fun () -> counter := !counter + 1; "x" ^ (string_of_int !counter)

let rec newVars n =
  if n = 0 then []
  else let var = newVar () in var :: newVars (n-1)

let rec take n l =
  if n = 0 then []
  else match l with [] -> failwith "Take" | h :: t -> h :: take (n-1) t

type branching_type = Either of string * string | Bool | Option of string

type exp =
  | LetExp of string list * rhs * exp
  | MatchExp of branching_type * string * exp * exp
  | RetExp of string list
and rhs =
  | Apply of string * string list
  | Nop of string list
  | Nested of exp

(* pretty printing *)
let rec string_of_ids = function
  | [] -> "()"
  | [x] -> x
  | x :: xs -> x ^ ", " ^ string_of_ids xs

let rec string_of_exp = function
  | LetExp (vars, Apply (fname, args), e2) -> "let " ^ string_of_ids vars ^ " = " ^ fname ^ "(" ^ string_of_ids args ^ ") in\n" ^ string_of_exp e2
  | LetExp (vars, Nop args, e2) -> "let " ^ string_of_ids vars ^ " = " ^ string_of_ids args ^ " in\n" ^ string_of_exp e2
  | LetExp (vars, Nested e1, e2) -> "let " ^ string_of_ids vars ^ " = " ^ string_of_exp e1 ^ " in\n" ^ string_of_exp e2
  | MatchExp (Either (l, r), x, e1, e2) -> "match " ^ x ^ " with Left " ^ l ^ " -> " ^ string_of_exp e1 ^ " | Right " ^ r ^ " -> " ^ string_of_exp e2
  | RetExp vars -> string_of_ids vars

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

(* translator exp_of_prog
 kont represents instructions already processed *)
let rec exp_of_prog kont = function
  | [], vars -> (kont, vars)
  (* Instructions consuming one value and producing one value *)
  | Simple (("CAR" | "CDR") as s) :: rest , var1 :: vars ->
     (* DEBUG *)
     print_string (string_of_ids (var1::vars)); print_string s; print_newline();
     (* DEBUG *)
     let var2 = newVar() in
     exp_of_prog
       (fun exp -> kont (LetExp ([var2], Apply (String.lowercase_ascii s, [var1]), exp)))
       (rest, var2 :: vars)
  (* Instructions consuming two values and producing one value *)
  | Simple (("PLUS" | "MULT") as s) :: rest , var1 :: var2 :: vars ->
     (* DEBUG *)
     print_string (string_of_ids (var1::var2::vars)); print_string s; print_newline();
     (* DEBUG *)
     let var3 = newVar() in
     exp_of_prog
       (fun exp ->
         kont (LetExp ([var3], Apply (String.lowercase_ascii s, [var1; var2]), exp)))
       (rest, var3 :: vars)
  (* DUP duplicates name of the stack top *)
  | Simple "DUP" :: rest , var1 :: vars ->
     (* DEBUG *)
     print_string (string_of_ids (var1::vars)); print_string " DUP"; print_newline();
     (* DEBUG *)
     exp_of_prog kont (rest, var1 :: var1 :: vars)
  (* DIP needs special treatment *)
  | OneBlock ("DIP", is) :: rest, var1 :: vars ->
     (* DEBUG *)
     print_string (string_of_ids (var1::vars)); print_string " DIP"; print_newline();
     (* DEBUG END *)
     let kont_body, final_vars = exp_of_prog init_kont (is, vars) in
     (* DEBUG *)
     print_string ("INIT: "^string_of_ids (var1::vars)); print_newline();
     print_string ("FINAL: "^string_of_ids final_vars); print_newline();
     (* DEBUG END *)
     let newvars = diff final_vars vars in
     (* DEBUG *)
     print_string (string_of_exp (LetExp (newvars, Nested (kont_body (RetExp newvars)), RetExp ["[_]"]))); print_newline();
     (* DEBUG END *)
     exp_of_prog
       (fun exp -> kont (LetExp (newvars, Nested (kont_body (RetExp newvars)), exp)))
       (rest, var1 :: final_vars)
  | TwoBlocks ("IF_LEFT", is1, is2) :: rest, var0 :: vars ->
     let var1 = newVar () in
     let kont_body1, final_vars1 = exp_of_prog init_kont (is1, var1::vars) in
     let var2 = newVar () in
     let kont_body2, final_vars2 = exp_of_prog init_kont (is2, var2::vars) in
     let newvars1 = diff final_vars1 (var1::vars) in
     let newvars2 = diff final_vars2 (var2::vars) in
     let num_newvars = (max (List.length newvars1) (List.length newvars2)) in
     let newvars = newVars num_newvars in
     let newvars1 = take num_newvars final_vars1 in
     let newvars2 = take num_newvars final_vars2 in
     exp_of_prog
       (fun exp -> kont (LetExp (newvars,
                                 Nested (MatchExp (Either (var1, var2), var0, kont_body1 (RetExp newvars1), kont_body2 (RetExp newvars2))),
                                 exp)))
       (rest, newvars)

let exp_of_code (Code body) =
  let kont, ids = exp_of_prog init_kont (body, ["param_st"]) in
  match ids with
    [] -> failwith "exp_of_code: Stack is empty!"
  | x :: _ -> kont (RetExp [x])
