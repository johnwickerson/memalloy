(*
MIT License

Copyright (c) 2017 by John Wickerson, Nathan Chong and Tyler Sorensen

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

(** Compares memory models *)

open! Format
open! General_purpose

let succ_paths = ref []
let also_succ_paths = ref []
let fail_paths = ref []
let withinit = ref false
let hints = ref []
let eventcount = ref 0
let eventcount2 = ref 0
let description = ref ""
let fencerels = ref false
let minimal = ref false
let exact = ref false

let emptytxns = ref false	      

let min_thds = ref 0
let max_thds = ref (-1)
let min_locs = ref 0
let max_locs = ref (-1)
let min_txns = ref 0
let max_txns = ref (-1)

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let write_file als_file pp =
  let oc = open_out als_file in
  pp (formatter_of_out_channel oc);
  close_out oc

(** Parse architecture from given .cat file *)
let arch_of cat_path =
  let ic = open_in cat_path in
  let lexbuf = Lexing.from_channel ic in
  let model_type,_,_ = Cat_parser.main Cat_lexer.token lexbuf in
  Archs.parse_arch model_type

let pp_open_modules succ_sig fail_sig oc =
  let chop_extn = Filename.chop_extension in
  for i = 1 to List.length !succ_paths do
    let model = chop_extn (List.nth !succ_paths (i-1)) in
    fprintf oc "open ../../%s[%s] as M%d\n" model succ_sig i
  done;
  for i = 1 to List.length !fail_paths do
    let model = chop_extn (List.nth !fail_paths (i-1)) in
    fprintf oc "open ../../%s[%s] as N%d\n" model fail_sig i
  done;
  for i = 1 to List.length !also_succ_paths do
    let model = chop_extn (List.nth !also_succ_paths (i-1)) in
    let i = i + List.length !succ_paths in
    fprintf oc "open ../../%s[%s] as M%d\n" model fail_sig i
  done

let rec mk_indent n = if n=0 then "" else " " ^ mk_indent (n-1)
                    
let pp_violated_models indent e exec_sig oc =
  MyList.iteri (
      fun i _ ->
      fprintf oc "%snot(N%d/consistent[%s,%s])\n"
        (mk_indent indent) (i+1) e exec_sig;
      fprintf oc "%sN%d/dead[%s,%s]\n\n"
        (mk_indent indent) (i+1) e exec_sig
    ) !fail_paths

let pp_satisfied_models indent e exec_sig oc =
  MyList.iteri (
      fun i _ ->
      fprintf oc "%sM%d/consistent[%s,%s]\n\n"
        (mk_indent indent) (i+1) e exec_sig
    ) !succ_paths
	       
let pp_also_satisfied_models indent e exec_sig oc =
  MyList.iteri (fun i _ ->
      let i = i + List.length !succ_paths in
      fprintf oc "%sM%d/consistent[%s,%s]\n\n"
        (mk_indent indent) (i+1) e exec_sig
    ) !also_succ_paths
  
(** [min_classes ev n r dom oc] generates an Alloy constraint (sent to [oc]) that requires the existence of [n] distinct objects of type [ev], all in [dom], and none of which are related by [r]*)
let min_classes ev n r dom oc =
  let es = List.map (sprintf "e%d") (range 1 n) in
  fprintf oc "  some disj %a : %s {\n"
	  (MyList.pp_gen ", " pp_str) es ev;
  fprintf oc "    %a in %s\n" (MyList.pp_gen "+" pp_str) es dom;
  fprintf oc "    no ((sq[%a]-iden) & X.%s)\n"
	  (MyList.pp_gen "+" pp_str) es r;
  fprintf oc "  }\n"

(** [pp_min_classes name ev n r dom oc] generates an Alloy constraint (sent to [oc]) that requires the existence of [n] distinct objects of type [ev], all in [dom], none of which are related by [r]. The [name] is for a descriptive comment.*)
let pp_min_classes name ev n r dom oc =
  if 0 < n then (
    fprintf oc "  // At least %d %s\n" n name;
    min_classes ev n r dom oc
  )
	  
(** [pp_max_classes name ev n r dom oc] generates an Alloy constraint (sent to [oc]) that requires the non-existence of [n+1] distinct objects of type [ev], all in [dom], none of which are related by [r]. The [name] is for a descriptive comment.*)
let pp_max_classes name ev n r dom oc =
  if 0 <= n then (
    fprintf oc "  // At most %d %s\n" n name;
    fprintf oc "  not (\n";
    min_classes ev (n+1) r dom oc;
    fprintf oc "  )\n"
  )
		   
let pp_file oc path =
  let ic = open_in path in
  try while true do fprintf oc "%s\n" (input_line ic) done
  with End_of_file -> close_in ic
			      
let pp_hint_predicates oc = List.iter (pp_file oc) !hints

let pp_hint_name oc hint_file =
  let hint_name =
    Filename.chop_extension (Filename.basename hint_file)
  in
  fprintf oc "  %s[X]\n" hint_name

(** [pp_comparator arch oc] generates an Alloy file (sent to [oc]) that can be used to find an execution of type [arch] that satisfies all the models in [!succ_paths] and violates all the models in [!fail_paths]. *)
let pp_comparator arch oc =
  if !description <> "" then fprintf oc "/* %s */\n" !description;
  pp_open_modules "E" "E" oc;
  fprintf oc "sig E {}\n\n";
  fprintf oc "pred interesting[e:PTag->E, X:%a] {\n\n" Archs.pp_Arch arch;
  pp_violated_models 2 "e" "X" oc;
  pp_satisfied_models 2 "e" "X" oc;
  pp_also_satisfied_models 2 "e" "X" oc;
  fprintf oc "}\n\n";
  fprintf oc "pred gp [X:%a] {\n\n" Archs.pp_Arch arch;
  if !withinit then
    fprintf oc "  withinit[X]\n"
  else
    fprintf oc "  withoutinit[X]\n";
  fprintf oc "  E in X.EV\n\n";
  if !emptytxns then (
    fprintf oc "  // Every event is a read, write, fence or transaction\n";
    fprintf oc "  E in X.R + X.W + X.F + dom[X.stxn]\n\n";
    fprintf oc "  // All nop events are singleton-transactions\n";
    fprintf oc "  all n : dom[X.stxn] - (X.R + X.W + X.F) |\n";
    fprintf oc "    one n.(X.stxn)\n\n"
  );
  fprintf oc "  interesting[none->none, X]\n\n";
  if !minimal then (
    fprintf oc "  not some e : X.EV | interesting[rm_EV->e, X]\n";
    List.iter (fun rel ->
        let extra =
          if List.mem rel ["ad";"cd";"dd"] then ""
          else " & imm[X.sb]"
        in
        fprintf oc "  not some e : dom[X.%s%s] |\n" rel extra;
        fprintf oc "    interesting[rm_%s->e, X]\n" rel
      ) (Archs.arch_min_rels !fencerels arch);
    List.iter (fun set ->
        fprintf oc
          "  not some e : X.%s | interesting[rm_%s->e, X]\n" set set
      ) (Archs.arch_min_sets !fencerels arch);
    let tag = "rm_txn->e" in
    fprintf oc "  not some e : dom[X.stxn]  {\n";
    fprintf oc "    let tpo = X.sb & X.stxn |\n";
    fprintf oc "    no tpo.e or no e.tpo\n";
    pp_violated_models 4 tag "X" oc;
    pp_satisfied_models 4 tag "X" oc;
    pp_also_satisfied_models 4 tag "X" oc;
    fprintf oc "  }\n";
  );
  List.iter (pp_hint_name oc) !hints;
  pp_min_classes "threads" "E" !min_thds "sthd" "X.EV - X.IW" oc;
  pp_max_classes "threads" "E" !max_thds "sthd" "X.EV - X.IW" oc;
  pp_min_classes "locations" "E" !min_locs "sloc" "X.R + X.W" oc;
  pp_max_classes "locations" "E" !max_locs "sloc" "X.R + X.W" oc;
  pp_min_classes "transactions" "E" !min_txns "stxn" "dom[X.stxn]" oc;
  pp_max_classes "transactions" "E" !max_txns "stxn" "dom[X.stxn]" oc;
  fprintf oc "}\n\n";
  pp_hint_predicates oc;
  fprintf oc "run gp for 1 Exec, %s%d E, 3 Int\n"
    (if !exact then "exactly " else "") !eventcount

(** [pp_comparator2 arch mapping_path arch2 oc] generates an Alloy file (sent to [oc]) that can be used to find an execution {i X} of type [arch] and an execution {i Y} of type [arch2] such that {i X} satisfies all the models in [!succ_paths], {i Y} violates all the models in [!fail_paths], and {i X} and {i Y} are related by the mapping in [mapping_path] *)
let pp_comparator2 arch mapping_path arch2 oc =
  if !withinit then
    failwith "Initial writes not supported in compiler mappings";
  if !description <> "" then fprintf oc "/* %s */\n" !description;
  pp_open_modules "HE" "SE" oc;
  let mapping = Filename.chop_extension mapping_path in
  fprintf oc "open ../../%s[SE,HE] as mapping\n\n" mapping;
  fprintf oc "sig SE, HE {}\n\n";
  fprintf oc "pred gp [X:%a, Y:%a, map:SE->HE] {\n\n"
    Archs.pp_Arch arch Archs.pp_Arch arch2;
  fprintf oc "  withoutinit[X]\n";
  fprintf oc "  withoutinit[Y]\n\n";
  pp_violated_models 2 "none->none" "X" oc;
  pp_satisfied_models 2 "none->none" "Y" oc;
  pp_also_satisfied_models 2 "none->none" "X" oc;
  List.iter (pp_hint_name oc) !hints;
  fprintf oc "  // We have a valid application of the mapping\n";
  fprintf oc "  apply_map[X, Y, map]\n\n";
  pp_min_classes "threads" "SE" !min_thds "sthd" "X.EV - X.IW" oc;
  pp_max_classes "threads" "SE" !max_thds "sthd" "X.EV - X.IW" oc;
  pp_min_classes "locations" "SE" !min_locs "sloc" "X.R + X.W" oc;
  pp_max_classes "locations" "SE" !max_locs "sloc" "X.R + X.W" oc;
  pp_min_classes "transactions" "SE" !min_txns "stxn" "dom[X.stxn]" oc;
  pp_max_classes "transactions" "SE" !max_txns "stxn" "dom[X.stxn]" oc;
  fprintf oc "}\n\n";
  pp_hint_predicates oc;
  fprintf oc "run gp for exactly 1 M1/Exec, exactly 1 N1/Exec, %d SE, %d HE, 3 Int\n" !eventcount !eventcount2

let get_args () =
  let arch = ref None in
  let arch2 = ref None in
  let mapping_path = ref None in
  let comparator_als = ref None in
  let speclist = [
      ("-satisfies", Arg.String (set_list_ref succ_paths),
       "Execution should satisfy this model (repeatable)");
      ("-violates", Arg.String (set_list_ref fail_paths),
       "Execution should violate this model (repeatable)");
      ("-arch", Arg.String (set_option_ref arch),
       "Type of executions being compared (required)");
      ("-events", Arg.Set_int eventcount, "Max number of events");
      ("-mapping", Arg.String (set_option_ref mapping_path),
       "An .als file representing a mapping between executions");
      ("-arch2", Arg.String (set_option_ref arch2),
       "Type of target execution (required iff -mapping is given)");
      ("-events2", Arg.Set_int eventcount2,
       "Max number of target events (required iff -mapping is given)");
      ("-alsosatisfies", Arg.String (set_list_ref also_succ_paths),
       "Execution should also satisfy this model (repeatable; always refers to the 'source' model when checking compilers)");
      ("-desc", Arg.Set_string description,
       "Textual description (optional)");
      ("-o", Arg.String (set_option_ref comparator_als),
       "Output .als file (optional, default stdout)");
      ("-hint", Arg.String (set_list_ref hints),
       "An .als file containing a 'hint_*[X]' predicate (optional, repeatable)");
      ("-minthreads", Arg.Set_int min_thds,
       "Find executions with at least N threads (default 0)");
      ("-maxthreads", Arg.Set_int max_thds,
       "Find executions with at most N threads");
      ("-threads",
       Arg.Int (fun i -> min_thds := i; max_thds := i),
       "Find executions with exactly N threads");
      ("-minlocations", Arg.Set_int min_locs,
       "Find executions with at least N locations (default 0)");
      ("-maxlocations", Arg.Set_int max_locs,
       "Find executions with at most N locations");
      ("-locations",
       Arg.Int (fun i -> min_locs := i; max_locs := i),
       "Find executions with exactly N locations");
      ("-mintransactions", Arg.Set_int min_txns,
       "Find executions with at least N transactions (default 0)");
      ("-maxtransactions", Arg.Set_int max_txns,
       "Find executions with at most N transactions");
      ("-transactions",
       Arg.Int (fun i -> min_txns := i; max_txns := i),
       "Find executions with exactly N transactions");
      ("-emptytxns", Arg.Set emptytxns, "Option: allow empty transactions");
      ("-withinit", Arg.Set withinit,
       "Option: explicit initial writes");
      ("-fencerels", Arg.Set fencerels,
       "Option: fences as relations");
      ("-minimal", Arg.Set minimal,
       "Option: only generate minimal executions");
      ("-exact", Arg.Set exact,
       "Option: solutions must use all events");
    ] in
  let usage_msg =
    "Generating an Alloy file that can be run to compare two models.\nUsage: `comparator [options]`. There must be at least one -satisfies or -violates flag.\nOptions available:"
  in
  let bad_arg _ =
    Arg.usage speclist usage_msg;
    raise (Arg.Bad "Missing or too many arguments.")
  in
  Arg.parse speclist bad_arg usage_msg;
  let arch = match !arch with
    | Some arch -> Archs.parse_arch arch
    | None -> failwith "Expected one -arch"
  in
  let arch2 = match !arch2 with
    | Some arch -> Some (Archs.parse_arch arch)
    | None -> None
  in
  let oc = match !comparator_als with
    | Some f -> open_out f
    | None -> stdout
  in
  arch, !mapping_path, arch2, oc

(** Print a description of the comparison being undertaken *)
let pp_description () =
  printf "\n";
  printf "\n";
  printf "==============================\n";
  printf "%s\n" !description;
  printf "------------------------------\n"      

let main () =
  let arch, mapping_path, arch2, oc = get_args () in
  if !succ_paths @ !fail_paths = [] then
    failwith "Expected at least one -satisfies or -violates flag";
  let pp =
    match mapping_path, arch2, !eventcount2 with
    | None, None, 0 -> pp_comparator arch
    | Some mapping_path, Some arch2, n when n>0 ->
       pp_comparator2 arch mapping_path arch2
    | _ ->
       failwith "Expected all or none of: -mapping, -arch2, -events2"
  in
  pp (formatter_of_out_channel oc);
  close_out oc;
  exit 0
       
     let _ = main ()

