open Ast_helper
open Trans
open MySupport

let usage_msg = "ocaml_of_michelson -spec <specfile> <file2> -o <output>"
let spec_file = ref "./inst.mli"
let output_file = ref ""
let input_file = ref ""
let speclist =
  [("-spec", Arg.Set_string input_file, "Set instruction spec file (default: ./inst.mli)");
   ("-o", Arg.Set_string output_file, "Set output file name")]

let read_trans_print () =
  (* parse the arguments *)
  Arg.parse speclist (fun s -> input_file := s) usage_msg;
  (* parse the spec file *)
  InstSpec.v := InstSpec.spec_of_item (Parse.interface (Lexing.from_channel (open_in !spec_file)));
  (* parse the input file *)
  let in_chan = if !input_file = "" then Lexing.from_channel stdin
                else Lexing.from_channel (open_in !input_file) in
  let code = McParser.toplevel McLexer.main in_chan in
  (* prepare the output channel *)
  let out_chan = if !output_file = "" then stdout
                 else open_out !output_file in
  (* then translate! *)
  let v = [ Str.value Asttypes.Nonrecursive
              [{ pvb_pat = pat_of_var "main";
                 pvb_expr = exp_of_code code;
                 pvb_attributes = [];
                 pvb_loc = Location.none } ] ]
  in
  output_string out_chan "open Inst\n";
  output_string out_chan (Pprintast.string_of_structure v);
  output_char out_chan '\n'

let _ = read_trans_print ()
